package benchmarking.script

import java.nio.file.{Files, Path, Paths}

import benchmarking.generation.{Uniform, UniformDiscrete, UniformLimited}
import zio.ZIO
import zio.console._
import zio.interop.catz._
import cats.syntax.traverse._
import cats.instances.list._
import indices.XTreeBuilder.XTreeConfig
import zio.blocking._
import zio.clock._

object FullScript extends zio.App {
  val scriptConfigs = List(
    ScriptConfig(
      name = "uniform",
      dimensions = List(2, 4, 8, 16, 24, 32),
      rulesNumbers = fibonaccisUntil(100000),
      pointsNumbers = List(50000),
      Uniform.genRule,
      Uniform.genPoint
    ),
    ScriptConfig(
      name = "uniformLimited",
      dimensions = List(2, 4, 8, 16, 24, 32),
      rulesNumbers = fibonaccisUntil(100000),
      pointsNumbers = List(50000),
      UniformLimited.genRule,
      Uniform.genPoint
    ),
    ScriptConfig(
      name = "uniformDiscrete",
      dimensions = List(2, 4, 8, 16, 24, 32),
      rulesNumbers = fibonaccisUntil(100000),
      pointsNumbers = List(50000),
      UniformDiscrete.genRule,
      Uniform.genPoint
    )
  )

  val treeConfig: Int => XTreeConfig =
    dimensions => XTreeConfig(165, 250, dimensions, 0.5)

  def singleRun(rootDir: Path, scriptConfig: ScriptConfig): ZIO[zio.ZEnv, Throwable, Unit] =
    for {
      _ <- putStrLn(scriptConfig.toString)
      singleRunDir = rootDir.resolve(scriptConfig.name)
      _ <- effectBlocking(Files.createDirectories(singleRunDir))

      _ <- putStrLn("Quadratic")
      quadraticResults <- QuadraticScript.runScript(scriptConfig)
      _ <- effectBlocking(Files.write(singleRunDir.resolve("quadratic.tsv"), quadraticResults.toTsv.getBytes))

      _ <- putStrLn("BulkRTreeRule")
      bulkRTreeRuleResults <- BulkRTreeRuleIndexScript.runScript(scriptConfig, treeConfig)
      _ <- effectBlocking(Files.write(singleRunDir.resolve("bRTreeRule.tsv"), bulkRTreeRuleResults.toTsv.getBytes))

      _ <- putStrLn("BulkRTreePoint")
      bulkRTreePointResults <- BulkRTreePointIndexScript.runScript(scriptConfig, treeConfig, 10000)
      _ <- effectBlocking(Files.write(singleRunDir.resolve("bRTreePoint.tsv"), bulkRTreePointResults.toTsv.getBytes))

      _ <- putStrLn("XTreeRule")
      xTreeRuleResults <- XTreeRuleIndexScript.runScript(scriptConfig, treeConfig)
      _ <- effectBlocking(Files.write(singleRunDir.resolve("xTreeRule.tsv"), xTreeRuleResults.toTsv.getBytes))

      _ <- putStrLn("XTreePoint")
      xTreePointResults <- XTreePointIndexScript.runScript(scriptConfig, treeConfig, 10000)
      _ <- effectBlocking(Files.write(singleRunDir.resolve("xTreePoint.tsv"), xTreePointResults.toTsv.getBytes))
    } yield ()

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      now <- currentDateTime
      rootDir = Paths.get(s"results/${now.toLocalDateTime}")
      _ <- scriptConfigs.traverse(scriptConfig => singleRun(rootDir, scriptConfig))
    } yield 0).catchAll(err => putStrLn(err.toString).as(1))
}
