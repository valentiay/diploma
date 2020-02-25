package testing.script

import indices.XTreeBuilder
import indices.XTreeBuilder.XTreeConfig
import testing.generation.Uniform

object XTreeBenchmark extends App {
  val sizes = List(1, 10, 100, 1000, 5000, 10000, 20000)
  val pointsSize = 1000
  val dimensions = 3

  val points = Uniform.generatePoints(dimensions, 100)
  val xtree = XTreeBuilder.fromPoints(XTreeConfig(2, 5, dimensions, 0.5), points)
  xtree.printMe()
}
