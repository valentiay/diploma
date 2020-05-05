package generator

import java.util.UUID

import core.database.MongoRulesStorage
import core.domain.{Point, serde}
import core.indices.ERIO
import fs2.Stream
import fs2.kafka._
import reactivemongo.api.AsyncDriver
import zio.console._
import zio.interop.catz._
import zio.ZIO

object Main extends zio.App {
  implicit def runtime: zio.Runtime[zio.ZEnv] = Main


  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      config <- GeneratorConfig.read
      driver = AsyncDriver()
      rulesStorage <- MongoRulesStorage.make(driver, config.mongo)

      _ <- rulesStorage.clear
      rules <- ZIO.collectAll(ZIO.replicate(config.numRules)(config.genRule))
      _ <- ZIO.foreach(rules.grouped(10000).toList)(group => rulesStorage.putRules(group) *> putStrLn("Rules group put"))

      _ <- Stream.repeatEval[ERIO, Point](config.genPoint)
        .evalTap(i => putStrLn(i.toString))
        .map { point =>
          val record = ProducerRecord(config.outputTopic, UUID.randomUUID, point)
          ProducerRecords.one(record)
        }
        .through(produce(config.producerSettings))
        .compile
        .drain
    } yield 0).catchAllCause(err => putStrLn(err.untraced.prettyPrint).as(1))
}
