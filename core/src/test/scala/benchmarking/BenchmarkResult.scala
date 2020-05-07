package benchmarking

final case class BenchmarkResult(
                                  dimensions: Int,
                                  rules: Int,
                                  points: Int,
                                  timeMs: Long,
                                  timeNormUs: Long,
                                  matchesPercent: Double
                                ) {
  def toTsvLine: String =
    s"${dimensions.formatted("%9d")}\t${rules.formatted("%9d")}\t${points.formatted("%9d")}\t${timeMs.formatted("%9d")}\t${timeNormUs.formatted("%9d")}\t${matchesPercent.formatted("%2.2f")}"
}

final case class BenchmarkResults(
                                   name: String,
                                   lines: List[BenchmarkResult],
                                 ) {
  def toTsv: String = {
    s"""
       |${BenchmarkResults.tsvHeader}
       |${lines.map(_.toTsvLine).mkString("\n")}
    """.stripMargin
  }
}

object BenchmarkResults {
  val tsvHeader =
    "dimensions\trules\tpoints\ttimeMs\ttimeNormUs\tmatchesPercent"
}