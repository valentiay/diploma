package classifier

import java.util.UUID

import core.generation.Uniform
import cats.effect.{ConcurrentEffect, ContextShift, Timer}
import com.twitter.finagle.Http
import fs2.kafka.{AutoOffsetReset, ConsumerSettings, Deserializer, ProducerRecord, ProducerRecords, ProducerSettings, Serializer, consumerStream, produce}
import core.domain.{Point, serde}
import core.indices.{BulkRTreeRulesIndex, ERIO}
import core.indices.XTreeBuilder.XTreeConfig
import io.finch._
import zio.{RIO, Task, ZIO}
import zio.console._
import zio.interop.catz._

object Main extends zio.App with Endpoint.Module[Task] {

  implicit def runtime: zio.Runtime[zio.ZEnv] = Main

  //  val api: Endpoint[Task, String] = get("hello") { Ok("Hello, World!") }

  //  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
  //    (Task(Http.server.serve(":8080", api.toServiceAs[Text.Plain])) *> ZIO.never)
  //      .as(0)
  //      .catchAll(err => putStrLn(err.toString).as(1))
  //  }

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      config <- ClassifierConfig.read

      consumerSettings = ConsumerSettings(
        keyDeserializer = Deserializer[ERIO, UUID],
        valueDeserializer = serde.pointDeserializer[ERIO]
      ).withAutoOffsetReset(AutoOffsetReset.Latest)
        .withBootstrapServers(config.bootstrapServers)
        .withGroupId(config.group)

      producerSettings = ProducerSettings(
        keySerializer = Serializer[ERIO, UUID],
        valueSerializer = serde.matchSerializer[ERIO]
      ).withBootstrapServers(config.bootstrapServers)

      rules <- ZIO.foreach(List.fill(2000)(UUID.randomUUID()))(uuid =>
        Uniform.genRule(config.dimensions).map(rule => (rule, uuid))
      )
      index = new BulkRTreeRulesIndex(rules, XTreeConfig(156, 250, config.dimensions, 0.5))
      _ <- putStrLn(index.toString)
      _ <- consumerStream[ERIO, UUID, Point](consumerSettings)
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
        .through(produce(producerSettings))
        .compile
        .drain

    } yield 0).catchAllCause(err => putStrLn(err.untraced.prettyPrint).as(1))
}
