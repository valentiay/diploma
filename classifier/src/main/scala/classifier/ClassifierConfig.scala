package classifier

import java.util.UUID

import core.database.MongoRulesStorage.MongoConfig
import core.domain.{Match, Point, serde}
import core.indices.ERIO
import fs2.kafka.{AutoOffsetReset, ConsumerSettings, Deserializer, ProducerSettings, Serializer}
import zio.RIO
import zio.duration._
import zio.system._
import zio.interop.catz._

final case class ClassifierConfig(
                                   rulesTtl: Duration,
                                   dimensions: Int,
                                   inputTopic: String,
                                   outputTopic: String,
                                   consumerSettings: ConsumerSettings[ERIO, UUID, Point],
                                   producerSettings: ProducerSettings[ERIO, UUID, Match],
                                   mongo: MongoConfig,
                                 )

object ClassifierConfig {
  def read: RIO[System, ClassifierConfig] =
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

      inputTopic <- env("INPUT_TOPIC").collect(new IllegalArgumentException("INPUT_TOPIC must be set")) {
        case Some(value) => value
      }

      outputTopic <- env("OUTPUT_TOPIC").collect(new IllegalArgumentException("OUTPUT_TOPIC must be set")) {
        case Some(value) => value
      }

      consumerSettings = ConsumerSettings(
        keyDeserializer = Deserializer[ERIO, UUID],
        valueDeserializer = serde.pointDeserializer[ERIO]
      ).withAutoOffsetReset(AutoOffsetReset.Latest)
        .withBootstrapServers(bootstrapServers)
        .withGroupId("classifier")

      producerSettings = ProducerSettings(
        keySerializer = Serializer[ERIO, UUID],
        valueSerializer = serde.matchSerializer[ERIO]
      ).withBootstrapServers(bootstrapServers)

      mongoHosts <- env("MONGO_HOSTS").collect(new IllegalArgumentException("MONGO_HOSTS must be set")) {
        case Some(host) => List(host)
      }

    } yield ClassifierConfig(
      rulesTtl = 5.minutes,
      dimensions = dimensions,
      inputTopic = inputTopic,
      outputTopic = outputTopic,
      consumerSettings = consumerSettings,
      producerSettings = producerSettings,
      mongo = MongoConfig(mongoHosts, "diploma", "password")
    )
}
