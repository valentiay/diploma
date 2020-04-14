package classifier

import java.util.UUID

import com.twitter.finagle.Http
import fs2.kafka.{ProducerRecord, ProducerRecords, consumerStream, produce}
import core.domain.{Point, Rule}
import core.database.{MongoRulesStorage, RulesStorage}
import core.indices.{BulkRTreeRulesIndex, ERIO, Index}
import core.indices.XTreeBuilder.XTreeConfig
import io.finch._
import io.prometheus.client.{CollectorRegistry, Counter}
import io.prometheus.client.exporter.common.TextFormat
import io.prometheus.client.hotspot.DefaultExports
import reactivemongo.api.AsyncDriver
import zio.{Task, ZIO}
import zio.console._
import zio.interop.catz._

import java.io.StringWriter

object Main extends zio.App with Endpoint.Module[Task] {

  implicit def runtime: zio.Runtime[zio.ZEnv] = Main

  private val pointsTotal =
    Counter.build("classifier_points_processed_total", "Number of points processed").register()

  private val matchesTotal =
    Counter.build("classifier_matches_found_total", "Number of matches found").register()

  val api: Endpoint[Task, String] = get("metrics") {
    Task {
      val writer = new StringWriter()
      TextFormat.write004(writer, CollectorRegistry.defaultRegistry.metricFamilySamples())
      Ok(writer.toString)
    }
  }

  def makeIndex(config: ClassifierConfig)(rules: List[(Rule, UUID)]) =
    new BulkRTreeRulesIndex(rules, XTreeConfig(156, 250, config.dimensions, 0.5))

  def runPipeline(config: ClassifierConfig, index: Index): ERIO[Unit] =
    consumerStream[ERIO, UUID, Point](config.consumerSettings)
      .evalTap(_.subscribeTo(config.inputTopic))
      .flatMap(_.stream)
      .map(_.record.value)
      .evalTap(_ => Task(pointsTotal.inc()))
      .through(index.findRules)
      .evalTap(_ => Task(matchesTotal.inc()))
      .map { mat =>
        val record = ProducerRecord(config.outputTopic, UUID.randomUUID(), mat)
        ProducerRecords.one(record)
      }
      .through(produce(config.producerSettings))
      .compile
      .drain

  def runProcess(config: ClassifierConfig, rulesStorage: RulesStorage): ERIO[Unit] =
    rulesStorage
      .getRules
      .doUntil(_.nonEmpty)
      .tap(_ => putStrLn("Building Index"))
      .map(makeIndex(config))
      .flatMap(index => runPipeline(config, index))
      .timeout(config.rulesTtl)
      .forever

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      _ <- Task(DefaultExports.initialize())
      config <- ClassifierConfig.read
      driver = AsyncDriver()
      rulesStorage <- MongoRulesStorage.make(driver, config.mongo)
      _ <- runProcess(config, rulesStorage).forkDaemon
      _ <- Task(Http.server.serve(":8080", api.toServiceAs[Text.Plain])) *> ZIO.never
    } yield 0).catchAllCause(err => putStrLn(err.untraced.prettyPrint).as(1))
}
