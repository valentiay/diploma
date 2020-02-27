package testing.script

import indices.Quadratic
import testing.generation.Uniform

object TestQuadratic extends App {
  val sizes = List(1, 10, 100, 1000, 5000, 10000, 20000, 50000)
  val pointsSize = 1000
  val dimensions = 8

  println("Quadratic")
  println("Size\tTime, ms\tMatches\tMatches norm.")
  for (size <- sizes) {
    val rules = Uniform.generateRules(dimensions, size)
    val points = Uniform.generatePoints(dimensions, size)
    val start = System.nanoTime()
    val index = new Quadratic(rules)
    val result = for (point <- points) yield {
      index.findRules(point)
    }
    val end = System.nanoTime()
    val totalMatches = result.flatten.size
    val time = (end - start).toDouble / pointsSize / 1000000
    println(s"$size\t${time.formatted("%9.4f")}\t$totalMatches\t${totalMatches.toDouble / size}")
  }
}
