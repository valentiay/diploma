package validator

import java.util.UUID

import cats.effect.{ConcurrentEffect, ContextShift, Timer}
import fs2.kafka._
import zio.{RIO, ZIO}
import zio.console._
import zio.system._

object Main extends zio.App {
  type ERIO[A] = RIO[zio.ZEnv, A]

  implicit val concurrentEffectTask: ConcurrentEffect[ERIO] = zio.interop.catz.taskEffectInstance(Main)
  implicit val contextShiftTask: ContextShift[ERIO] = zio.interop.catz.zioContextShift
  implicit val timerTask: Timer[ERIO] = zio.interop.catz.zioTimer

  val topic = "points"
  val group = "group"

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
  (for {
      bootstrapServers <- env("KAFKA_BOOTSTRAP_SERVERS").flatMap {
        case Some(servers) => ZIO.effectTotal(servers)
        case None => ZIO.fail(new IllegalArgumentException("KAFKA_BOOTSTRAP_SERVERS env variable required"))
      }

      _ <- putStrLn(bootstrapServers)

      consumerSettings = ConsumerSettings(
        keyDeserializer = Deserializer[ERIO, UUID],
        valueDeserializer = Deserializer[ERIO, String]
      ).withAutoOffsetReset(AutoOffsetReset.Earliest)
      .withBootstrapServers(bootstrapServers)
      .withGroupId(group)

      _ <- consumerStream(consumerSettings)
            .evalTap(_.subscribeTo(topic))
            .flatMap(_.stream)
            .evalTap(record => putStrLn(record.record.value))
            .compile
            .drain
    } yield 0).catchAllCause(err => putStrLn(err.toString).as(1))
}
