package benchmarking.script

import java.nio.file.{Files, Paths}

import benchmarking.BenchmarkResults
import core.generation.{Uniform, UniformLimited}
import core.indices.XTreeBuilder.XTreeConfig
import zio.ZIO
import zio.blocking.effectBlocking
import zio.clock.currentDateTime
import zio.console._

object PointScript extends zio.App{
  val rulesNumbers = fibonaccisUntil(50000)

  val chunkSizes = fibonaccisUntil(50000)

  val dimensionSizes = List(8)

  val pointsNumber = List(50000)

  val treeConfig: Int => XTreeConfig =
    dimensions => XTreeConfig(40, 100, dimensions, 0.5)

  val scriptConfig = ScriptConfig("BulkRTreePoint", dimensionSizes, rulesNumbers, pointsNumber, UniformLimited.genRule, Uniform.genPoint)

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      results <- ZIO.foreach(chunkSizes){ chunkSize =>
        putStrLn(chunkSize.toString) *>
          BulkRTreePointIndexScript.runScript(scriptConfig, treeConfig, chunkSize).map(
            _.lines.map{
              result => s"$chunkSize\t${result.toTsvLine}"
            }
          )
      }
      tsv = (s"chunkSize\t${BenchmarkResults.tsvHeader}" :: results.flatten).mkString("\n")
      now <- currentDateTime
      dir = Paths.get(s"results/${now.toLocalDateTime}/pointScript")
      _ <- effectBlocking(Files.createDirectories(dir))
      _ <- effectBlocking(Files.write(dir.resolve("pointScript.tsv"), tsv.getBytes))
    } yield 0).catchAll(err => putStrLn(err.toString).as(1))
}
