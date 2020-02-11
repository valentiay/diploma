package indices

import domain.{Point, Rule}

final class Quadratic(rules: List[Rule]) extends Index {
  def findRules(point: Point): List[Rule] =
    rules.filter { rule =>
      rule.zip(point).forall { case (restriction, feature) => restriction.satisfied(feature) }
    }
}