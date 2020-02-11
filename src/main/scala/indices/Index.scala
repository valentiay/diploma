package indices

import domain.{Point, Rule}

trait Index {
  def findRules(point: Point): List[Rule]
}
