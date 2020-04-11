package core.indices

import java.util.UUID

import core.domain.{Match, Point, Rule}
import fs2.Stream
import core.indices.XTreeBuilder.XTreeConfig
import zio.interop.catz._

class XTreeRulesIndex(rules: List[(Rule, UUID)], config: XTreeConfig) extends Index {

  private val index = XTreeBuilder.fromRules(config, rules)

  def findRules(points: Stream[ERIO, Point]): Stream[ERIO, Match] = {
    points.flatMap { point =>
      Stream.fromIterator[ERIO](
        index.findPoint(point).map(res => Match(point, res._2)).iterator
      )
    }
  }
}
