package validator

import java.util.UUID

import domain.serde
import fs2.kafka._
import indices.ERIO
import zio.ZIO
import zio.console._
import zio.interop.catz._

object Main extends zio.App {

  implicit def runtime: zio.Runtime[zio.ZEnv] = Main

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      config <- ValidatorConfig.read

      consumerSettings = ConsumerSettings(
        keyDeserializer = Deserializer[ERIO, UUID],
        valueDeserializer = serde.matchDeserializer[ERIO]
      ).withAutoOffsetReset(AutoOffsetReset.Latest)
        .withBootstrapServers(config.bootstrapServers)
        .withGroupId(config.group)

      _ <- consumerStream(consumerSettings)
        .evalTap(_.subscribeTo(config.inputTopic))
        .flatMap(_.stream)
        .evalTap(record => putStrLn(record.record.value.toString))
        .compile
        .drain
    } yield 0).catchAllCause(err => putStrLn(err.untraced.prettyPrint).as(1))
}
