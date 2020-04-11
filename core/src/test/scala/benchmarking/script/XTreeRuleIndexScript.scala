package benchmarking.script

import benchmarking.{BenchmarkResults, IndexBenchmark}
import core.indices.XTreeRulesIndex
import core.indices.XTreeBuilder.XTreeConfig
import core.generation.Uniform
import zio.ZIO
import zio.console.putStrLn

object XTreeRuleIndexScript extends zio.App {
  val defaultScriptConfig =
    ScriptConfig(
      name = "XTreeRule",
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
               ): ZIO[zio.ZEnv, Throwable, BenchmarkResults] =
    new IndexBenchmark(
      "XTreeRulesIndex",
      (dimensions, rules) => new XTreeRulesIndex(rules, treeConfig(dimensions)),
      scriptConfig.dimensions,
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
