package benchmarking.script

import indices.Quadratic
import benchmarking.{BenchmarkResults, IndexBenchmark}
import benchmarking.generation.Uniform
import zio.{UIO, ZIO}
import zio.console._

object QuadraticScript extends zio.App {
  val dimensions = 8

  val defaultScriptConfig =
    ScriptConfig(
      dimensions = dimensions,
      rulesNumbers = fibonaccisUntil(50000),
      pointsNumbers = List(30000),
      Uniform.genRule(dimensions),
      Uniform.genPoint(dimensions)
    )
  def runScript(scriptConfig: ScriptConfig): ZIO[zio.ZEnv, Throwable, BenchmarkResults] =
    new IndexBenchmark(
      "Quadratic",
      rules => new Quadratic(rules),
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
