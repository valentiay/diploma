package benchmarking

import cats.syntax.traverse._
import cats.instances.list._
import domain.{Point, Rule}
import indices.Index
import zio.{Task, ZIO}
import zio.interop.catz._
import zio.console._
import zio.clock._
import zio.duration._
import fs2.Stream

class IndexBenchmark(
                      name: String,
                      mkIndex: List[Rule] => Index,
                      rulesNumbers: List[Int],
                      pointsNumbers: List[Int],
                      genRule: Task[Rule],
                      genPoint: Task[Point]
                    ) {
  def singleResult(rulesNumber: Int, pointsNumber: Int): ZIO[zio.ZEnv, Throwable, BenchmarkResult] = {
    for {
      _ <- putStrLn(s"Starting ($rulesNumber, $pointsNumber)")
      rules <- List.fill(rulesNumber)(genRule).sequence
      points = Stream.repeatEval(genPoint).take(pointsNumber)
//      _ <- points.compile.fold(0)((a, b) => a + 1).flatMap(s => putStrLn("Size: " + s.toString))
//      _ <- points.grouped(10000).runCount.flatMap(s => putStrLn("Size: " + s.toString))
      start <- nanoTime
      _ <- mkIndex(rules).findRules(points).compile.drain
      end <- nanoTime
      timeNs = (end - start)
    } yield
      BenchmarkResult(
        rulesNumber,
        pointsNumber,
        timeNs / 100000,
        timeNs / pointsNumber / 1000,
        0
      )
  }


  def results: ZIO[zio.ZEnv, Throwable, BenchmarkResults] =
    (for {
      rulesNumber <- rulesNumbers
      pointsNumber <- pointsNumbers
    } yield singleResult(rulesNumber, pointsNumber)).sequence.map{ lines =>
      BenchmarkResults(name, Map(), lines)
    }
}
