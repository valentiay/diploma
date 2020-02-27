package testing.script

import indices.XTreeBuilder
import indices.XTreeBuilder.XTreeConfig
import testing.generation.Uniform

import scala.util.Random

object XTreeBenchmark extends App {
  val sizes = List(1, 10, 100, 1000, 5000, 10000, 20000)
  val pointsSize = 1000
  val dimensions = 8

  Random.setSeed(10)

  //  val xtree = XTreeBuilder.fromPoints(XTreeConfig(2, 5, dimensions, 0.5), points)

  val points = Uniform.generatePoints(dimensions, 10000)
  val config = XTreeConfig(20, 50, dimensions, 0.5)
  val xtree = new XTreeBuilder(config)
  points.foreach { point =>
    xtree.insert(point)
//    xtree.printMe()
//    println()
//    println()
  }


  val rules = Uniform.generateRules(dimensions, 5)
//  rules.foreach{ r =>
//    println("NEW")
//    println(r)
//    xtree.find(r)
//  }
  xtree.printMe()
}
