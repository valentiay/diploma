package indices

import java.util.UUID

import domain.{Match, Point, Rule}
import indices.XTreeBuilder.XTreeConfig
import fs2.Stream
import zio.interop.catz._

class XTreePointIndex(rules: List[(Rule, UUID)], chunkSize: Int, config: XTreeConfig) extends Index {
  def findRules(points: Stream[ERIO, Point]): Stream[ERIO, Match] =
    points.chunkN(chunkSize).flatMap { chunk =>
      val xTreeBuilder = XTreeBuilder.fromChunk(config, chunk)
      Stream.fromIterator[ERIO](
        rules.flatMap(rule => xTreeBuilder.findRule(rule._1).map(res => Match(res._1.toPoint, res._2))).iterator
      )
    }
}
