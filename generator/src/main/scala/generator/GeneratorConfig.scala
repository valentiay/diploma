package generator

import java.util.UUID

import core.indices.ERIO
import core.database.MongoRulesStorage.MongoConfig
import core.generation.{Uniform, UniformDiscrete, UniformLimited}
import core.domain.{Point, Rule, serde}
import fs2.kafka.{ProducerSettings, Serializer}
import zio.{RIO, UIO}
import zio.interop.catz._
import zio.system._

final case class GeneratorConfig(
                                  dimensions: Int,
                                  numRules: Int,
                                  genRule: UIO[Rule],
                                  genPoint: UIO[Point],
                                  outputTopic: String,
                                  producerSettings: ProducerSettings[ERIO, UUID, Point],
                                  mongo: MongoConfig
                                )

object GeneratorConfig {
  def read: RIO[System, GeneratorConfig] =
    for {
      dimensions <- env("DIMENSIONS")
        .map(_.flatMap(_.toIntOption))
        .collect(new IllegalArgumentException("DIMENSIONS must be set to an integer")) {
          case Some(value) => value
        }

      numRules <- env("NUM_RULES")
        .map(_.flatMap(_.toIntOption))
        .collect(new IllegalArgumentException("NUM_RULES must be set to an integer")) {
          case Some(value) => value
        }

      genRule <- env("GEN_RULE")
        .collect(new IllegalArgumentException("GEN_RULE must be set to one of [uniform, uniformLimited, uniformDiscrete]")) {
          case Some("uniform") => Uniform.genRule(dimensions)
          case Some("uniformLimited") => UniformLimited.genRule(dimensions)
          case Some("uniformDiscrete") => UniformDiscrete.genRule(dimensions)
        }

      genPoint <- env("GEN_POINT")
        .map(_.fold("uniform")(_.toLowerCase))
        .collect(new IllegalArgumentException("GEN_POINT must be one of [uniform]")) {
          case "uniform" => Uniform.genPoint(dimensions)
        }

      bootstrapServers <- env("KAFKA_BOOTSTRAP_SERVERS")
        .collect(new IllegalArgumentException("KAFKA_BOOTSTRAP_SERVERS env variable required")) {
          case Some(value) => value
        }

      producerSettings = ProducerSettings(
        keySerializer = Serializer[ERIO, UUID],
        valueSerializer = serde.pointSerializer[ERIO]
      ).withBootstrapServers(bootstrapServers)


      outputTopic <- env("OUTPUT_TOPIC").collect(new IllegalArgumentException("OUTPUT_TOPIC must be set")) {
        case Some(value) => value
      }

      mongoHosts <- env("MONGO_HOSTS").collect(new IllegalArgumentException("MONGO_HOSTS must be set")) {
        case Some(host) => List(host)
      }

    } yield GeneratorConfig(
      dimensions = dimensions,
      numRules = numRules,
      genRule = genRule,
      genPoint = genPoint,
      producerSettings = producerSettings,
      outputTopic = outputTopic,
      mongo = MongoConfig(mongoHosts, "diploma", "password")
    )
}