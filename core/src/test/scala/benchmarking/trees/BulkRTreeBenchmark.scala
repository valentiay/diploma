package benchmarking.trees

import java.util.UUID

import benchmarking.generation.Uniform
import indices.{BulkRTreeVertex, MBR}

import scala.util.Random

object BulkRTreeBenchmark extends App {
  val dimensions = 5

  Random.setSeed(10)

  //  val xtree = XTreeBuilder.fromPoints(XTreeConfig(2, 5, dimensions, 0.5), points)

  val points = List.fill(10000)(Uniform.genPointUnsafe(dimensions))
  val xtree = BulkRTreeVertex.build(dimensions, 5, points.map(point => (MBR.single(point), UUID.randomUUID())))

  val rules = List.fill(5)(Uniform.genRule(dimensions))
  //  rules.foreach{ r =>
  //    println("NEW")
  //    println(r)
  //    xtree.find(r)
  //  }
  xtree.printMe()
}
