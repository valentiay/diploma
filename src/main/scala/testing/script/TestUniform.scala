package testing.script

import indices.Quadratic
import testing.generation.Uniform

object TestUniform extends App {
  val sizes = List(1, 10, 100, 1000, 5000, 10000, 20000, 100000).reverse
  val pointsSize = 1000
  val dimensions = 8

  println("Quadratic")
  println("Size\tTime, ms")
  for (size <- sizes) {
    val rules = Uniform.generateRules(dimensions, size)
    val points = Uniform.generatePoints(dimensions, size)
    val index = new Quadratic(rules)
    val start = System.nanoTime()
    for (point <- points) {
      index.findRules(point)
    }
    val end = System.nanoTime()
    val time = (end - start).toDouble / pointsSize / 1000000
    println(s"$size\t ${time.formatted("%.4f")}")
  }
}
