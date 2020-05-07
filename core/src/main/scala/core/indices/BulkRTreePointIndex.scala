package core.indices

import java.util.UUID

import core.domain.{Match, Point, Rule}
import fs2.{Chunk, Stream}
import core.indices.XTreeBuilder.XTreeConfig
import zio.interop.catz._

import scala.collection.immutable.ArraySeq

class BulkRTreePointIndex(rules: List[(Rule, UUID)], chunkSize: Int, config: XTreeConfig) extends Index {
  private val rulesMBRs: List[(MBR, UUID)] =
    rules.map { case (rule, uuid) => (MBR.fromRule(rule), uuid) }

  private val uuidStub = UUID.randomUUID()

  def chunkToSeq(chunk: Chunk[Point]): Seq[(MBR, UUID)] = {
    var i = 0
    val array = ArraySeq.newBuilder[(MBR, UUID)]
    array.sizeHint(chunk.size)
    while (i < chunk.size) {
      array.addOne((MBR.single(chunk.apply(i)), uuidStub))
      i += 1
    }
    array.result()
  }

  def findRules(points: Stream[ERIO, Point]): Stream[ERIO, Match] =
    points.chunkN(chunkSize).flatMap { chunk =>
      val rTree = BulkRTree.build(config.dimensions, config.maxChildren, chunkToSeq(chunk))
      Stream.fromIterator[ERIO](
        rulesMBRs
          .iterator
          .flatMap { rule =>
            rTree
              .findMBR(rule._1)
              .iterator
              .map(res => Match(res._1.toPoint, rule._2))
          }
      )
    }
}
