package classifier

import java.util.UUID

import core.database.MongoRulesStorage.MongoConfig
import core.domain.{Match, Point, Rule, serde}
import core.indices.{ERIO, Index}
import fs2.kafka.{AutoOffsetReset, ConsumerSettings, Deserializer, ProducerSettings, Serializer}
import zio.RIO
import zio.duration._
import zio.system._
import core.indices.XTreeBuilder.XTreeConfig
import core.indices._
import zio.interop.catz._

final case class ClassifierConfig(
                                   rulesTtl: Duration,
                                   dimensions: Int,
                                   makeIndex: List[(Rule, UUID)] => Index,
                                   inputTopic: String,
                                   outputTopic: String,
                                   consumerSettings: ConsumerSettings[ERIO, UUID, Point],
                                   producerSettings: ProducerSettings[ERIO, UUID, Match],
                                   mongo: MongoConfig,
                                   zookeeperConnect: String,
                                   groupSize: Int
                                 )

object ClassifierConfig {
  def read: RIO[System, ClassifierConfig] =
    for {
      dimensions <- env("DIMENSIONS")
        .map(_.flatMap(_.toIntOption))
        .collect(new IllegalArgumentException("DIMENSIONS must be set to an integer")) {
          case Some(value) => value
        }

      treeConfig = XTreeConfig(40, 100, dimensions, 0.5)

      makeIndex <- env("INDEX_TYPE")
        .collect(new IllegalArgumentException("INDEX_TYPE must be set to one of [quadratic, xTreePoint, xTreeRule, bulkRTreePoint, bulkRTreeRule]")) {
          case Some("quadratic") => rules: List[(Rule, UUID)] => new Quadratic(rules)
          case Some("xTreePoint") => rules: List[(Rule, UUID)] => new XTreePointIndex(rules, 10000, treeConfig)
          case Some("xTreeRule") => rules: List[(Rule, UUID)] => new XTreeRulesIndex(rules, treeConfig)
          case Some("bulkRTreePoint") => rules: List[(Rule, UUID)] => new BulkRTreePointIndex(rules, 10000, treeConfig)
          case Some("bulkRTreeRule") => rules: List[(Rule, UUID)] => new BulkRTreeRulesIndex(rules, treeConfig)
        }

      bootstrapServers <- env("KAFKA_BOOTSTRAP_SERVERS")
        .collect(new IllegalArgumentException("KAFKA_BOOTSTRAP_SERVERS env variable required")) {
          case Some(value) => value
        }

      inputTopic <- env("INPUT_TOPIC")
        .collect(new IllegalArgumentException("INPUT_TOPIC must be set")) {
          case Some(value) => value
        }

      outputTopic <- env("OUTPUT_TOPIC")
        .collect(new IllegalArgumentException("OUTPUT_TOPIC must be set")) {
          case Some(value) => value
        }

      consumerSettings = ConsumerSettings(
        keyDeserializer = Deserializer[ERIO, UUID],
        valueDeserializer = serde.pointDeserializer[ERIO]
      ).withAutoOffsetReset(AutoOffsetReset.Latest)
        .withBootstrapServers(bootstrapServers)

      producerSettings = ProducerSettings(
        keySerializer = Serializer[ERIO, UUID],
        valueSerializer = serde.matchSerializer[ERIO]
      ).withBootstrapServers(bootstrapServers)

      mongoHosts <- env("MONGO_HOSTS")
        .collect(new IllegalArgumentException("MONGO_HOSTS must be set")) {
          case Some(host) => List(host)
        }

      zookeeperConnect <- env("ZOOKEEPER_CONNECT")
        .collect(new IllegalArgumentException("ZOOKEEPER_CONNECT must be set")) {
          case Some(value) => value
        }

      groupSize <- env("GROUP_SIZE")
        .map(_.flatMap(_.toIntOption))
        .collect(new IllegalArgumentException("GROUP_SIZE must be set to an integer")) {
          case Some(value) => value
        }
    } yield ClassifierConfig(
      rulesTtl = 5.minutes,
      dimensions = dimensions,
      makeIndex = makeIndex,
      inputTopic = inputTopic,
      outputTopic = outputTopic,
      consumerSettings = consumerSettings,
      producerSettings = producerSettings,
      mongo = MongoConfig(mongoHosts, "diploma", "password"),
      zookeeperConnect = zookeeperConnect,
      groupSize = groupSize
    )
}
