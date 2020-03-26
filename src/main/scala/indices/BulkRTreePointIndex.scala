package indices

import domain.{Point, Rule}
import fs2.{Chunk, Stream}
import indices.XTreeBuilder.XTreeConfig
import zio.Task
import zio.interop.catz._

import scala.collection.immutable.ArraySeq

class BulkRTreePointIndex(rules: List[Rule], chunkSize: Int, config: XTreeConfig) extends Index {
  val rulesMBR: List[MBR] = rules.map(MBR.fromRule)

  def chunkToSeq(chunk: Chunk[Point]): Seq[MBR] = {
    var i = 0
    val array = ArraySeq.newBuilder[MBR]
    array.sizeHint(chunk.size)
    while (i < chunk.size) {
      array.addOne(MBR.single(chunk.apply(i)))
      i += 1
    }
    array.result()
  }

  def findRules(points: Stream[Task, Point]): Stream[Task, (Point, Rule)] =
    points.chunkN(chunkSize).flatMap { chunk =>
      //      println(this)
      //      println(chunk.take(2))
      //      println(chunk.size)
      val rTree = BulkRTreeVertex.build(config.dimensions, config.maxChildren, chunkToSeq(chunk))
      Stream.fromIterator[Task](rulesMBR.flatMap(rule => rTree.findMBR(rule).map(mbr => (Point(mbr.starts), rules.head))).iterator)
    }
}
