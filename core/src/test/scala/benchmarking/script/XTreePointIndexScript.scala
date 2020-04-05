package benchmarking.script

import benchmarking.{BenchmarkResults, IndexBenchmark}
import indices.XTreePointIndex
import indices.XTreeBuilder.XTreeConfig
import benchmarking.generation.Uniform
import zio.{UIO, ZIO}
import zio.console.putStrLn

object XTreePointIndexScript extends zio.App {
  val defaultScriptConfig =
    ScriptConfig(
      name = "XTreePoint",
      dimensions = List(8),
      rulesNumbers = fibonaccisUntil(50000),
      pointsNumbers = List(30000),
      Uniform.genRule,
      Uniform.genPoint
    )

  val defaultTreeConfig: Int => XTreeConfig =
    dimensions => XTreeConfig(40, 100, dimensions, 0.5)

  def runScript(
                 scriptConfig: ScriptConfig,
                 treeConfig: Int => XTreeConfig,
                 chunkSize: Int
               ): ZIO[zio.ZEnv, Throwable, BenchmarkResults] =
    new IndexBenchmark(
      "XTreePointIndex",
      (dimensions, rules) => new XTreePointIndex(rules, chunkSize, treeConfig(dimensions)),
      scriptConfig.dimensions,
      scriptConfig.rulesNumbers,
      scriptConfig.pointsNumbers,
      scriptConfig.genRule,
      scriptConfig.genPoint
    ).results

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      results <- runScript(defaultScriptConfig, defaultTreeConfig, 10000)
      _ <- putStrLn(results.toTsv)
    } yield 0).catchAll(err => putStrLn(err.toString).as(1))
}
