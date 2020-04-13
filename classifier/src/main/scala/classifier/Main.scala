package classifier

import java.util.UUID

import core.generation.Uniform
import cats.effect.{ConcurrentEffect, ContextShift, Timer}
import com.twitter.finagle.Http
import fs2.Stream
import fs2.kafka.{AutoOffsetReset, ConsumerSettings, Deserializer, ProducerRecord, ProducerRecords, ProducerSettings, Serializer, consumerStream, produce}
import core.domain.{Match, Point, Rule, serde}
import core.database.{MongoRulesStorage, RulesStorage}
import core.indices.{BulkRTreeRulesIndex, ERIO, Index}
import core.indices.XTreeBuilder.XTreeConfig
import io.finch._
import reactivemongo.api.AsyncDriver
import zio.{RIO, Task, ZIO}
import zio.clock._
import zio.duration._
import zio.console._
import zio.duration._
import zio.interop.catz._


object Main extends zio.App with Endpoint.Module[Task] {

  implicit def runtime: zio.Runtime[zio.ZEnv] = Main

  //  val api: Endpoint[Task, String] = get("hello") { Ok("Hello, World!") }

  //  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
  //    (Task(Http.server.serve(":8080", api.toServiceAs[Text.Plain])) *> ZIO.never)
  //      .as(0)
  //      .catchAll(err => putStrLn(err.toString).as(1))
  //  }

  def makeIndex(config: ClassifierConfig)(rules: List[(Rule, UUID)]) =
    new BulkRTreeRulesIndex(rules, XTreeConfig(156, 250, config.dimensions, 0.5))

  def runPipeline(config: ClassifierConfig, index: Index): ERIO[Unit] =
    consumerStream[ERIO, UUID, Point](config.consumerSettings)
      .evalTap(_.subscribeTo(config.inputTopic))
      .flatMap(_.stream)
      .map(_.record.value)
      .evalTap(point => putStrLn(s"Point: ${point.toString}"))
      .through(index.findRules)
      .evalTap(mat => putStrLn(s"Match: $mat"))
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
      config <- ClassifierConfig.read
      driver = AsyncDriver()
      rulesStorage <- MongoRulesStorage.make(driver, config.mongo)
      _ <- runProcess(config, rulesStorage)
    } yield 0).catchAllCause(err => putStrLn(err.untraced.prettyPrint).as(1))
}
