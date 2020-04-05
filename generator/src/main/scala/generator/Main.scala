package generator

import java.util.UUID

import cats.effect.{ConcurrentEffect, ContextShift}
import fs2.Stream
import fs2.kafka._
import generator.Main.ERIO
import zio.{RIO, ZIO}
import zio.clock._
import zio.duration._
import zio.console._
import zio.system._

object Main extends zio.App {
  type ERIO[A] = RIO[zio.ZEnv, A]

  implicit val concurrentEffectTask: ConcurrentEffect[ERIO] = zio.interop.catz.taskEffectInstance(Main)
  implicit val contextShiftTask: ContextShift[ERIO] = zio.interop.catz.zioContextShift

  val topic = "points"

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      bootstrapServers <- env("KAFKA_BOOTSTRAP_SERVERS").flatMap {
        case Some(servers) => ZIO.effectTotal(servers)
        case None => ZIO.fail(new IllegalArgumentException("KAFKA_BOOTSTRAP_SERVERS env variable required"))
      }

      _ <- putStrLn(bootstrapServers)

      producerSettings = ProducerSettings(
        keySerializer = Serializer[ERIO, UUID],
        valueSerializer = Serializer[ERIO, String]
      ).withBootstrapServers(bootstrapServers)

      _ <- Stream.iterateEval[ERIO, Int](0)(i => sleep(1.second).as(i + 1))
        .evalTap(i => putStrLn(i.toString))
        .map { i =>
          val record = ProducerRecord(topic, UUID.randomUUID, i.toString)
          ProducerRecords.one(record)
        }
        .through(produce(producerSettings))
        .compile
        .drain
    } yield 0).catchAllCause(err => putStrLn(err.toString).as(1))
}
