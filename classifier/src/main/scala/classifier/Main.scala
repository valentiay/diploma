package classifier

import cats.effect.Effect
import com.twitter.finagle.Http
import io.finch._
import zio.{Task, ZIO}
import zio.console._

object Main extends zio.App with Endpoint.Module[Task] {
  implicit val effectForTask: Effect[Task] = zio.interop.catz.taskEffectInstance[Any](Main)

  val api: Endpoint[Task, String] = get("hello") { Ok("Hello, World!") }

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    (Task(Http.server.serve(":8080", api.toServiceAs[Text.Plain])) *> ZIO.never)
      .as(0)
      .catchAll(err => putStrLn(err.toString).as(1))
  }
}
