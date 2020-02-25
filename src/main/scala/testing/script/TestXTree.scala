package testing.script

import indices.XTreeBuilder
import indices.XTreeBuilder.XTreeConfig
import testing.generation.Uniform

object TestXTree extends App {
  val sizes = List(1, 10, 100, 1000, 5000, 10000, 20000)
  val pointsSize = 1000
  val dimensions = 8

  val config = XTreeConfig(2, 100, dimensions, 0.5)

  println("X-Tree")
  println("Size\tTime, ms\tIdxBuild, %\tMatches\tMatches norm.")
  for (size <- sizes) {
    val rules = Uniform.generateRules(dimensions, size)
    val points = Uniform.generatePoints(dimensions, size)
    val start = System.nanoTime()
    val index = XTreeBuilder.fromPoints(config, points)
    val indexBuilt = System.nanoTime()
    val result =
      for (rule <- rules) yield {
        index.find(rule)
      }
    val end = System.nanoTime()
    val totalMatches = result.flatten.size
    val time = (end - start).toDouble / pointsSize / 1000000
    val indexTime = (indexBuilt - start).toDouble / (end - start) * 100
    println(s"$size\t${time.formatted("%9.4f")}\t${indexTime.formatted("%4.2f")}\t$totalMatches\t${totalMatches.toDouble / size}")
  }
}
