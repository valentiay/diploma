package benchmarking.script

import core.indices.Quadratic
import benchmarking.{BenchmarkResults, IndexBenchmark}
import core.generation.Uniform
import zio.{UIO, ZIO}
import zio.console._

object QuadraticScript extends zio.App {
  val defaultScriptConfig =
    ScriptConfig(
      name = "Quadratic",
      dimensions = List(8),
      rulesNumbers = fibonaccisUntil(50000),
      pointsNumbers = List(30000),
      Uniform.genRule,
      Uniform.genPoint
    )
  def runScript(scriptConfig: ScriptConfig): ZIO[zio.ZEnv, Throwable, BenchmarkResults] =
    new IndexBenchmark(
      "Quadratic",
      (dimensions, rules) => new Quadratic(rules),
      scriptConfig.dimensions,
      scriptConfig.rulesNumbers,
      scriptConfig.pointsNumbers,
      scriptConfig.genRule,
      scriptConfig.genPoint
    ).results

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =

    (for {
      results <- runScript(defaultScriptConfig)
      _ <- putStrLn(results.toTsv)
    } yield 0).catchAll(_ => UIO(1))
}
