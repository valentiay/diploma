package classifier

import java.util.UUID

import cats.effect.concurrent.MVar
import com.twitter.finagle.Http
import fs2.kafka.{ProducerRecord, ProducerRecords, consumerStream, produce}
import core.domain.Point
import core.database.{MongoRulesStorage, RulesStorage}
import core.indices.{ERIO, Index}
import io.finch._
import io.prometheus.client.{CollectorRegistry, Counter, Gauge}
import io.prometheus.client.exporter.common.TextFormat
import io.prometheus.client.hotspot.DefaultExports
import reactivemongo.api.AsyncDriver
import zio.{Fiber, Task, ZIO}
import zio.console._
import zio.duration._
import zio.clock._
import zio.interop.catz._
import java.io.StringWriter

import classifier.ZookeeperConfigService.ClusterConfig

object Main extends zio.App with Endpoint.Module[Task] {

  implicit def runtime: zio.Runtime[zio.ZEnv] = Main

  private val pointsTotal =
    Counter
      .build("classifier_points_processed_total", "Number of points processed")
      .labelNames("group")
      .register()

  private val matchesTotal =
    Counter
      .build("classifier_matches_found_total", "Number of matches found")
      .labelNames("group")
      .register()

  private val group =
    Gauge.build("classifier_group", "Current group").register()

  private val rulesNumber =
    Gauge.build("classifier_rules_total", "Number of rules in index").register()

  val api: Endpoint[Task, String] = get("metrics") {
    Task {
      val writer = new StringWriter()
      TextFormat.write004(writer, CollectorRegistry.defaultRegistry.metricFamilySamples())
      Ok(writer.toString)
    }
  }

  def startPipeline(config: ClassifierConfig, clusterConfig: ClusterConfig, index: Index): ERIO[Unit] =
    consumerStream[ERIO, UUID, Point](config.consumerSettings.withGroupId(s"classifier-${clusterConfig.group}"))
      .evalTap(_.subscribeTo(config.inputTopic))
      .flatMap(_.stream)
      .map(_.record.value)
      .evalTap(_ => Task(pointsTotal.labels(clusterConfig.group.toString).inc()))
      .through(index.findRules)
      .evalTap(_ => Task(matchesTotal.labels(clusterConfig.group.toString).inc()))
      .map { mat =>
        val record = ProducerRecord(config.outputTopic, UUID.randomUUID(), mat)
        ProducerRecords.one(record)
      }
      .through(produce(config.producerSettings))
      .compile
      .drain
      .onInterrupt(putStrLn("Interrupting process"))

  def startProcess(config: ClassifierConfig, clusterConfig: ClusterConfig, rulesStorage: RulesStorage): ERIO[Unit] =
      rulesStorage
        .getRulesBatch(clusterConfig.group, clusterConfig.groupNumber)
        .tap(rules => ZIO(rulesNumber.set(rules.size)))
        .tap(rules => sleep(1.minute).when(rules.isEmpty))
        .doUntil(_.nonEmpty)
        .tap(_ => putStrLn("Building Index"))
        .map(config.makeIndex)
        .tap(_ => putStrLn("Index built"))
        .flatMap(index => startPipeline(config, clusterConfig, index).timeout(config.rulesTtl))
        .forever

  def loadConfig(
                  config: ClassifierConfig,
                  rulesStorage: RulesStorage,
                  mvar: MVar[ERIO, (ClusterConfig, Fiber[Throwable, Unit])]
                )(clusterConfig: ClusterConfig): ERIO[Unit] = {
    putStrLn(s"Received new cluster config: $clusterConfig") *>
      mvar.isEmpty.flatMap(isEmpty =>
        if (isEmpty) {
          putStrLn("It is first configuration, starting process") *>
            startProcess(config, clusterConfig, rulesStorage).fork.flatMap(fiber =>
              mvar.put((clusterConfig, fiber))
            )
        } else {
          for {
            (oldConfig, oldFiber) <- mvar.take
            _ <- if (oldConfig == clusterConfig) {
              putStrLn("Old cluster config is equal to new one. Doing nothing") *>
                mvar.put((clusterConfig, oldFiber))
            } else {
              putStrLn(s"Old cluster config ($oldConfig) is not equal to new one. Replacing") *>
                oldFiber.interrupt *>
                startProcess(config, clusterConfig, rulesStorage).fork.flatMap(fiber =>
                  mvar.put((clusterConfig, fiber))
                )
            }
          } yield ()
        }
      ) *>
      ZIO(group.set(clusterConfig.group))
  }

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      _ <- Task(DefaultExports.initialize())
      config <- ClassifierConfig.read
      driver = AsyncDriver()
      rulesStorage <- MongoRulesStorage.make(driver, config.mongo)
      mvar <- MVar.empty[ERIO, (ClusterConfig, Fiber[Throwable, Unit])]
      rulesRangeStream <- ZookeeperConfigService.rulesRangeStream(config)
      _ <- rulesRangeStream.evalTap(loadConfig(config, rulesStorage, mvar)).compile.drain.forkDaemon
      _ <- Task(Http.server.serve(":8081", api.toServiceAs[Text.Plain])).forkDaemon *> ZIO.never
    } yield 0).catchAllCause(err => putStrLn(err.untraced.prettyPrint).as(1))
}
