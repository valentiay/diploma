package indices

import domain.{Point, Rule}
import indices.XTreeBuilder.XTreeConfig
import zio.Task
import fs2.Stream
import zio.interop.catz._

class XTreePointIndex(rules: List[Rule], chunkSize: Int, config: XTreeConfig) extends Index {
  def findRules(points: Stream[Task, Point]): Stream[Task, (Point, Rule)] =
    points.chunkN(chunkSize).flatMap { chunk =>
//      println(this)
//      println(chunk.take(2))
//      println(chunk.size)
      val xTreeBuilder = XTreeBuilder.fromChunk(config, chunk)
      Stream.fromIterator[Task](rules.flatMap(rule => xTreeBuilder.find(rule).map(point => (point, rule))).iterator)
    }
}
