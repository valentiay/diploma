package testing.script

import indices.XTreeBuilder
import indices.XTreeBuilder.XTreeConfig
import testing.generation.Uniform

object XTreeBenchmark extends App {
  val sizes = List(1, 10, 100, 1000, 5000, 10000, 20000)
  val pointsSize = 1000
  val dimensions = 8

//  println("Quadratic")
//  println("Size\tTime, ms")
//  for (size <- sizes) {
//    val points = Uniform.generatePoints(dimensions, size)
//    val start = System.nanoTime()
//    val xtree = XTreeBuilder.fromPoints(XTreeConfig(2, 100, dimensions, 0.5), points)
//    val end = System.nanoTime()
//    println(s"$size: ${(end - start) / 1000000} ms")
//  }
  val points = Uniform.generatePoints(dimensions, 200)
  val xtree = XTreeBuilder.fromPoints(XTreeConfig(2, 10, dimensions, 0.5), points)
  xtree.printMe()
}
