package validator

import java.util.UUID

import core.database.MongoRulesStorage.MongoConfig
import core.domain.{Match, serde}
import core.indices.ERIO
import fs2.kafka.{AutoOffsetReset, ConsumerSettings, Deserializer}
import zio.RIO
import zio.interop.catz._
import zio.system.{System, env}

final case class ValidatorConfig(
                                  dimensions: Int,
                                  inputTopic: String,
                                  consumerSettings: ConsumerSettings[ERIO, UUID, Match],
                                  mongo: MongoConfig
                                )

object ValidatorConfig {
  def read: RIO[System, ValidatorConfig] =
    for {
      dimensions <- env("DIMENSIONS")
        .map(_.flatMap(_.toIntOption))
        .collect(new IllegalArgumentException("DIMENSIONS must be set to an integer")) {
          case Some(value) => value
        }

      bootstrapServers <- env("KAFKA_BOOTSTRAP_SERVERS")
        .collect(new IllegalArgumentException("KAFKA_BOOTSTRAP_SERVERS env variable required")) {
          case Some(value) => value
        }

      consumerSettings = ConsumerSettings(
        keyDeserializer = Deserializer[ERIO, UUID],
        valueDeserializer = serde.matchDeserializer[ERIO]
      ).withAutoOffsetReset(AutoOffsetReset.Latest)
        .withBootstrapServers(bootstrapServers)
        .withGroupId("validator")


      inputTopic <- env("INPUT_TOPIC").collect(new IllegalArgumentException("INPUT_TOPIC must be set")) {
        case Some(value) => value
      }

      mongoHosts <- env("MONGO_HOSTS").collect(new IllegalArgumentException("MONGO_HOSTS must be set")) {
        case Some(host) => List(host)
      }

    } yield ValidatorConfig(
      dimensions = dimensions,
      consumerSettings = consumerSettings,
      inputTopic = inputTopic,
      mongo = MongoConfig(mongoHosts, "thesis", "password")
    )
}