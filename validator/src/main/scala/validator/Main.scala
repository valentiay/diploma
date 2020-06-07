package validator

import java.io.StringWriter

import com.twitter.finagle.Http
import core.database.MongoRulesStorage
import core.domain._
import fs2.kafka._
import io.finch.{Endpoint, Ok, Text}
import io.prometheus.client.{CollectorRegistry, Counter}
import io.prometheus.client.exporter.common.TextFormat
import reactivemongo.api.AsyncDriver
import zio.{Task, ZIO}
import zio.console._
import zio.interop.catz._
import zio.duration._

object Main extends zio.App with Endpoint.Module[Task] {

  implicit def runtime: zio.Runtime[zio.ZEnv] = Main

  private val matchesTotal =
    Counter.build("validator_matches_total", "Number of matches processed").labelNames("result").register()

  val api: Endpoint[Task, String] = get("metrics") {
    Task {
      val writer = new StringWriter()
      TextFormat.write004(writer, CollectorRegistry.defaultRegistry.metricFamilySamples())
      Ok(writer.toString)
    }
  }

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      config <- ValidatorConfig.read
      driver = AsyncDriver()
      rulesStorage <- MongoRulesStorage.make(driver, config.mongo)
      rulesMap <- rulesStorage.getRules.map(_.map(_.swap).toMap).doUntil(_.nonEmpty).cached(1.minute)
      _ <- consumerStream(config.consumerSettings)
        .evalTap(_.subscribeTo(config.inputTopic))
        .flatMap(_.stream)
        .map(_.record.value)
        .evalTap(mat => putStrLn(mat.toString))
        .evalTap(_ => Task(matchesTotal.labels("any")))
//        .evalTap{ mat =>
//          rulesMap.flatMap(_.get(mat.ruleId) match {
//            case Some(rule) =>
//              if (ruleSatisfied(mat.point, rule)) {
//                Task(matchesTotal.labels("valid").inc())
//              } else {
//                Task(matchesTotal.labels("invalid").inc())
//              }
//            case None =>
//              Task(matchesTotal.labels("missing").inc())
//          })
//        }
        .compile
        .drain
        .forkDaemon

      _ <- Task(Http.server.serve(":8082", api.toServiceAs[Text.Plain])) *> ZIO.never

    } yield 0).catchAllCause(err => putStrLn(err.untraced.prettyPrint).as(1))
}
