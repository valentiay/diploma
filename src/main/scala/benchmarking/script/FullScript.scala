package benchmarking.script
import benchmarking.generation.Uniform
import zio.ZIO
import zio.console._
import zio.interop.catz._
import cats.syntax.traverse._
import cats.instances.list._
import indices.XTreeBuilder.XTreeConfig

object FullScript extends zio.App {
  val dimensions = 8
  val scriptConfigs = List(
    ScriptConfig(
      dimensions = dimensions,
      rulesNumbers = fibonaccisUntil(10000),
      pointsNumbers = List(100000),
      Uniform.genRule(dimensions),
      Uniform.genPoint(dimensions)
    )
  )

  val treeConfig = XTreeConfig(40, 500, dimensions, 0.5)

  def singleRun(scriptConfig: ScriptConfig) =
    for {
      quadraticResults <- QuadraticScript.runScript(scriptConfig)
      bulkRTreeRuleResults <- BulkRTreeRuleIndexScript.runScript(scriptConfig, treeConfig)
      bulkRTreePointResults <- BulkRTreePointIndexScript.runScript(scriptConfig, treeConfig, 20000)
      xTreeRuleResults <- XTreeRuleIndexScript.runScript(scriptConfig, treeConfig)
      xTreePointResults <- XTreePointIndexScript.runScript(scriptConfig, treeConfig, 20000)
      _ <- putStrLn(quadraticResults.toTsv)
      _ <- putStrLn(bulkRTreeRuleResults.toTsv)
      _ <- putStrLn(bulkRTreePointResults.toTsv)
      _ <- putStrLn(xTreeRuleResults.toTsv)
      _ <- putStrLn(xTreePointResults.toTsv)
    } yield ()

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    scriptConfigs.traverse(scriptConfig => singleRun(scriptConfig)).as(0).catchAll(err => putStrLn(err.toString).as(1))
}
