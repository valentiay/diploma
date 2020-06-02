package benchmarking.script

import java.nio.file.{Files, Paths}

import benchmarking.BenchmarkResults
import core.generation.{Uniform, UniformLimited}
import core.indices.XTreeBuilder.XTreeConfig
import zio.ZIO
import zio.blocking.effectBlocking
import zio.clock.currentDateTime
import zio.console._

object PointScript extends zio.App {
  val rulesNumbers = fibonacchis.dropWhile(_ < 100).takeWhile(_ < 100000).toList

  val chunkSizes = fibonacchis.dropWhile(_ < 75000).takeWhile(_ < 220000).toList

  val dimensionSizes = List(16)

  val pointsNumber = List(400000)

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
