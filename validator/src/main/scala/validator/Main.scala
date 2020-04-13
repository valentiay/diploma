package validator

import core.database.MongoRulesStorage
import core.domain._
import fs2.kafka._
import reactivemongo.api.AsyncDriver
import zio.ZIO
import zio.console._
import zio.interop.catz._

object Main extends zio.App {

  implicit def runtime: zio.Runtime[zio.ZEnv] = Main

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      config <- ValidatorConfig.read
      driver = AsyncDriver()
      rulesStorage <- MongoRulesStorage.make(driver, config.mongo)
      rulesMap <- rulesStorage.getRules.map(_.map(_.swap).toMap).doUntil(_.nonEmpty)
      _ <- consumerStream(config.consumerSettings)
        .evalTap(_.subscribeTo(config.inputTopic))
        .flatMap(_.stream)
        .map(_.record.value)
        .evalTap(mat => putStrLn(mat.toString))
        .evalTap{ mat =>
          rulesMap.get(mat.ruleId) match {
            case Some(rule) =>
              putStrLn(s"Rule not satisfied:\nRule: $rule\nPoint: ${mat.point}").when(!ruleSatisfied(mat.point, rule))
            case None =>
              putStrLn(s"Rule not found: ${mat.ruleId}")
          }
        }
        .compile
        .drain
    } yield 0).catchAllCause(err => putStrLn(err.untraced.prettyPrint).as(1))
}
