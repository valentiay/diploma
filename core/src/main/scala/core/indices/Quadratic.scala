package core.indices

import java.util.UUID

import core.domain.{Match, Point, Rule}
import fs2.Stream
import core.domain._
import zio.interop.catz._

final class Quadratic(rules: List[(Rule, UUID)]) extends Index {
  def findRules(points: Stream[ERIO, Point]): Stream[ERIO, Match] =
    points.flatMap { point =>
      Stream.fromIterator[ERIO](rules.collect { case (rule, uuid) if ruleSatisfied(point, rule) => Match(point, uuid) }.iterator)
    }
}