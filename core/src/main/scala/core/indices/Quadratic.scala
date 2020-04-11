package core.indices

import java.util.UUID

import core.domain.{Match, Point, Rule}
import fs2.Stream
import zio.interop.catz._

final class Quadratic(rules: List[(Rule, UUID)]) extends Index {
  def ruleSatisfied(point: Point, rule: Rule): Boolean =
    rule.zip(point.values).forall {
      case (restriction, feature) => restriction.satisfied(feature)
    }

  def findRules(points: Stream[ERIO, Point]): Stream[ERIO, Match] =
    points.flatMap { point =>
      Stream.fromIterator[ERIO](rules.collect { case (rule, uuid) if ruleSatisfied(point, rule) => Match(point, uuid) }.iterator)
    }
}