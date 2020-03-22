package benchmarking.script

import indices.XTreeBuilder
import indices.XTreeBuilder.XTreeConfig
import benchmarking.generation.Uniform

import scala.util.Random

object XTreeBenchmark extends App {
  val dimensions = 8

  Random.setSeed(10)

  //  val xtree = XTreeBuilder.fromPoints(XTreeConfig(2, 5, dimensions, 0.5), points)

  val points = List.fill(100)(Uniform.genPointUnsafe(dimensions))
  val config = XTreeConfig(2, 5, dimensions, 0.5)
  val xtree = new XTreeBuilder(config)
  points.foreach { point =>
    xtree.insertPoint(point)
//    xtree.printMe()
//    println()
//    println()
  }


  val rules = List.fill(5)(Uniform.genRule(dimensions))
//  rules.foreach{ r =>
//    println("NEW")
//    println(r)
//    xtree.find(r)
//  }
  xtree.printMe()
}
