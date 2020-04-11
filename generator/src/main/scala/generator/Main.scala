package generator

import java.util.UUID

import cats.effect.{ConcurrentEffect, ContextShift}
import fs2.Stream
import fs2.kafka._
import core.domain.{Point, serde}
import core.indices.ERIO
import zio.{RIO, UIO, ZIO}
import zio.clock._
import zio.duration._
import zio.console._
import zio.interop.catz._
import zio.system._

object Main extends zio.App {
  implicit def runtime: zio.Runtime[zio.ZEnv] = Main


  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      config <- GeneratorConfig.read

      producerSettings = ProducerSettings(
        keySerializer = Serializer[ERIO, UUID],
        valueSerializer = serde.pointSerializer[ERIO]
      ).withBootstrapServers(config.bootstrapServers)

      _ <- Stream.repeatEval[ERIO, Point](config.genPoint)
        .evalTap(i => putStrLn(i.toString))
        .map { point =>
          val record = ProducerRecord(config.outputTopic, UUID.randomUUID, point)
          ProducerRecords.one(record)
        }
        .through(produce(producerSettings))
        .compile
        .drain
    } yield 0).catchAllCause(err => putStrLn(err.untraced.prettyPrint).as(1))
}
