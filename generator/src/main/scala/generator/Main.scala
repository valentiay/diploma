package generator

import java.io.StringWriter
import java.util.UUID

import com.twitter.finagle.Http
import core.database.MongoRulesStorage
import core.domain.Point
import core.indices.ERIO
import fs2.Stream
import fs2.kafka._
import io.finch._
import io.prometheus.client.{CollectorRegistry, Counter}
import io.prometheus.client.exporter.common.TextFormat
import io.prometheus.client.hotspot.DefaultExports
import reactivemongo.api.AsyncDriver
import zio.console._
import zio.interop.catz._
import zio.{Task, ZIO}

object Main extends zio.App with Endpoint.Module[Task] {
  implicit def runtime: zio.Runtime[zio.ZEnv] = Main

  private val pointsGeneratedTotal =
    Counter.build("generator_points_generated_total", "Number of points generated").register()

  val api: Endpoint[Task, String] = get("metrics") {
    Task {
      val writer = new StringWriter()
      TextFormat.write004(writer, CollectorRegistry.defaultRegistry.metricFamilySamples())
      Ok(writer.toString)
    }
  }

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      _ <- Task(DefaultExports.initialize())
      config <- GeneratorConfig.read
      driver = AsyncDriver()
      rulesStorage <- MongoRulesStorage.make(driver, config.mongo)

      rules <- ZIO.collectAll(ZIO.replicate(config.numRules)(config.genRule))
      _ <- ZIO.foreach(rules.grouped(10000).toList)(group => rulesStorage.putRules(group) *> putStrLn("Rules group put"))

      _ <- Stream.repeatEval[ERIO, Point](config.genPoint)
        .map { point =>
          val record = ProducerRecord(config.outputTopic, UUID.randomUUID, point)
          ProducerRecords.one(record)
        }
        .through(produce(config.producerSettings))
        .evalTap(_ => Task(pointsGeneratedTotal.inc()))
        .compile
        .drain
        .forkDaemon

      _ <- Task(Http.server.serve(":8080", api.toServiceAs[Text.Plain])) *> ZIO.never

    } yield 0).catchAllCause(err => putStrLn(err.untraced.prettyPrint).as(1))
}
