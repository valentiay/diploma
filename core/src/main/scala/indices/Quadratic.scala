package indices

import domain.{Point, Rule}
import zio.Task
import fs2.Stream
import zio.interop.catz._

final class Quadratic(rules: List[Rule]) extends Index {
  def ruleSatisfied(point: Point, rule: Rule): Boolean =
    rule.zip(point.values).forall {
      case (restriction, feature) => restriction.satisfied(feature)
    }

  def findRules(points: Stream[Task, Point]): Stream[Task, (Point, Rule)] =
    points.flatMap { point =>
      Stream.fromIterator[Task](rules.collect { case rule if ruleSatisfied(point, rule) => (point, rule) }.iterator)
    }
}