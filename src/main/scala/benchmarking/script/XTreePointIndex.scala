package benchmarking.script

import benchmarking.IndexBenchmark
import indices.XTreePointIndex
import indices.XTreeBuilder.XTreeConfig
import benchmarking.generation.Uniform
import zio.{UIO, ZIO}
import zio.console.putStrLn

object XTreePointIndex extends zio.App {
  val dimensions = 8

  val config = XTreeConfig(40, 100, dimensions, 0.5)

  val rulesNumbers = fibonaccisUntil(200000)
  val pointsNumbers = List(30000)

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      results <- new IndexBenchmark(
        "XTreePointIndex",
        rules => new XTreePointIndex(rules, 10000, config),
        rulesNumbers,
        pointsNumbers,
        Uniform.genRule(dimensions),
        Uniform.genPoint(dimensions)
      ).results
      _ <- putStrLn(results.toTsv)
    } yield 0).catchAll(_ => UIO(1))
}
