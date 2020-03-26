package benchmarking.script

import benchmarking.{BenchmarkResults, IndexBenchmark}
import indices.XTreeRulesIndex
import indices.XTreeBuilder.XTreeConfig
import benchmarking.generation.Uniform
import zio.ZIO
import zio.console.putStrLn

object XTreeRuleIndexScript extends zio.App {
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
               ): ZIO[zio.ZEnv, Throwable, BenchmarkResults] =
    new IndexBenchmark(
      "XTreeRulesIndex",
      rules => new XTreeRulesIndex(rules, treeConfig),
      scriptConfig.rulesNumbers,
      scriptConfig.pointsNumbers,
      scriptConfig.genRule,
      scriptConfig.genPoint
    ).results

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      results <- runScript(defaultScriptConfig, defaultTreeConfig)
      _ <- putStrLn(results.toTsv)
    } yield 0).catchAll(err => putStrLn(err.toString).as(1))
}
