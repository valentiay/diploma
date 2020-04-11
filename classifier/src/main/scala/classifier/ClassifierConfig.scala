package classifier

import zio.RIO
import zio.system._

final case class ClassifierConfig(
                                   dimensions: Int,
                                   bootstrapServers: String,
                                   inputTopic: String,
                                   outputTopic: String,
                                   group: String
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

    } yield ClassifierConfig(
      dimensions = dimensions,
      bootstrapServers = bootstrapServers,
      inputTopic = inputTopic,
      outputTopic = outputTopic,
      group = "classifier"
    )
}
