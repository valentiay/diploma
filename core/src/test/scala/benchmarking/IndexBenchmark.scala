package benchmarking

import java.util.UUID

import cats.syntax.traverse._
import cats.instances.list._
import core.domain.{Point, Rule}
import core.indices.Index
import zio.{Task, ZIO}
import zio.interop.catz._
import zio.console._
import zio.clock._
import zio.blocking._
import fs2.Stream

class IndexBenchmark(
                      name: String,
                      mkIndex: (Int, List[(Rule, UUID)]) => Index,
                      dimensionsNumber: List[Int],
                      rulesNumbers: List[Int],
                      pointsNumbers: List[Int],
                      genRule: Int => Task[Rule],
                      genPoint: Int => Task[Point]
                    ) {
  def singleResult(dimensions: Int, rulesNumber: Int, pointsNumber: Int): ZIO[zio.ZEnv, Throwable, BenchmarkResult] = {
    for {
      rules <- List.fill(rulesNumber)(genRule(dimensions).map(rule => (rule, UUID.randomUUID()))).sequence
      points = Stream.repeatEval(genPoint(dimensions)).take(pointsNumber)
      start <- nanoTime
      _ <- mkIndex(dimensions, rules).findRules(points).compile.drain
      end <- nanoTime
      timeNs = end - start
      _ <- putStr("|")
      _ <- effectBlocking(System.gc())
    } yield
      BenchmarkResult(
        dimensions,
        rulesNumber,
        pointsNumber,
        timeNs / 1000000,
        timeNs / pointsNumber / 1000,
        0
      )
  }


  def results: ZIO[zio.ZEnv, Throwable, BenchmarkResults] =
    putStrLn("<" + "-" * (dimensionsNumber.size * rulesNumbers.size * pointsNumbers.size - 2) + ">") *>
      (for {
        dimensions <- dimensionsNumber
        rulesNumber <- rulesNumbers
        pointsNumber <- pointsNumbers
      } yield singleResult(dimensions, rulesNumber, pointsNumber)).sequence.map { lines =>
        BenchmarkResults(name, Map(), lines)
      } <*
      putStrLn("")
}
