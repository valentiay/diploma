package generator

import core.generation.Uniform
import core.domain.Point
import zio.{UIO, RIO}
import zio.system._

final case class GeneratorConfig(
                                  dimensions: Int,
                                  genPoint: UIO[Point],
                                  bootstrapServers: String,
                                  outputTopic: String
                                )

object GeneratorConfig {
  def read: RIO[System, GeneratorConfig] =
    for {
      dimensions <- env("DIMENSIONS")
        .map(_.flatMap(_.toIntOption))
        .collect(new IllegalArgumentException("DIMENSIONS must be set to an integer")) {
          case Some(value) => value
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

      outputTopic <- env("OUTPUT_TOPIC").collect(new IllegalArgumentException("OUTPUT_TOPIC must be set")) {
        case Some(value) => value
      }

    } yield GeneratorConfig(
      dimensions = dimensions,
      genPoint = genPoint,
      bootstrapServers = bootstrapServers,
      outputTopic = outputTopic
    )
}