package benchmarking.script

import benchmarking.{BenchmarkResults, IndexBenchmark}
import indices.XTreePointIndex
import indices.XTreeBuilder.XTreeConfig
import benchmarking.generation.Uniform
import zio.{UIO, ZIO}
import zio.console.putStrLn

object XTreePointIndexScript extends zio.App {
  val dimensions = 8

  val defaultScriptConfig =
    ScriptConfig(
      dimensions = dimensions,
      rulesNumbers = fibonaccisUntil(50000),
      pointsNumbers = List(30000),
      Uniform.genRule(dimensions),
      Uniform.genPoint(dimensions)
    )

  val defaultTreeConfig =
    XTreeConfig(40, 100, dimensions, 0.5)

  def runScript(
                 scriptConfig: ScriptConfig,
                 treeConfig: XTreeConfig,
                 chunkSize: Int
               ): ZIO[zio.ZEnv, Throwable, BenchmarkResults] =
    new IndexBenchmark(
      "XTreePointIndex",
      rules => new XTreePointIndex(rules, chunkSize, treeConfig),
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
