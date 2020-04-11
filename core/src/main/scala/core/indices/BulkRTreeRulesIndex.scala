package core.indices

import java.util.UUID

import core.domain.{Match, Point, Rule}
import core.indices.XTreeBuilder.XTreeConfig
import fs2.Stream
import zio.interop.catz._

class BulkRTreeRulesIndex(rules: List[(Rule, UUID)], config: XTreeConfig) extends Index {

  private val index =
    BulkRTreeVertex.build(
      config.dimensions, config.maxChildren, rules.map { case (rule, uuid) => (MBR.fromRule(rule), uuid) }
    )

  def findRules(points: Stream[ERIO, Point]): Stream[ERIO, Match] = {
    points.flatMap { point =>
      Stream.fromIterator[ERIO](index.findPoint(point).map(res => Match(point, res._2)).iterator)
    }
  }
}
