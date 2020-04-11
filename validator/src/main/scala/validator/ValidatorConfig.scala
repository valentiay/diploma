package validator

import zio.RIO
import zio.system.{System, env}

final case class ValidatorConfig(
                                  dimensions: Int,
                                  bootstrapServers: String,
                                  inputTopic: String,
                                  group: String
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

      inputTopic <- env("INPUT_TOPIC").collect(new IllegalArgumentException("INPUT_TOPIC must be set")) {
        case Some(value) => value
      }

    } yield ValidatorConfig(
      dimensions = dimensions,
      bootstrapServers = bootstrapServers,
      inputTopic = inputTopic,
      group = "validator"
    )
}