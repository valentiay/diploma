package benchmarking

final case class BenchmarkResult(
                                  dimensions: Int,
                                  rules: Int,
                                  points: Int,
                                  timeMs: Long,
                                  timeNormUs: Long,
                                  matchesPercent: Double
                                )

final case class BenchmarkResults(
                                   name: String,
                                   extras: Map[String, String],
                                   lines: List[BenchmarkResult],
                                 ) {
  def toTsv: String = {
    val extrasString = extras.toList.map { case (k, v) => s"$k->$v" }.mkString(",", ",", "")
    val linesStrings =
      lines.map { line =>
        import line._
        s"${dimensions.formatted("%9d")}\t${rules.formatted("%9d")}\t${points.formatted("%9d")}\t${timeMs.formatted("%9d")}\t${timeNormUs.formatted("%9d")}\t${matchesPercent.formatted("%2.2f")}"
      }
    s"""
       |$name $extrasString
       |dimensions\t    rules\t   points\t   timeMs\ttimeNormUs\tmatchesPercent
       |${linesStrings.mkString("\n")}
    """.stripMargin
  }
}