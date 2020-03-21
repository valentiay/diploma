package benchmarking.script

import indices.Quadratic
import benchmarking.IndexBenchmark
import benchmarking.generation.Uniform
import zio.{UIO, ZIO}
import zio.console._

object TestQuadratic extends zio.App {
  val dimensions = 8

  val rulesNumbers = fibonaccisUntil(50000)
  val pointsNumbers = List(1000)

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =

    (for {
      results <- new IndexBenchmark(
          "Quadratic",
          rules => new Quadratic(rules),
          rulesNumbers,
          pointsNumbers,
          Uniform.genRule(dimensions),
          Uniform.genPoint(dimensions)
        ).results
      _ <- putStrLn(results.toTsv)
    } yield 0).catchAll(_ => UIO(1))
}
