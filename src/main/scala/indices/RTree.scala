package indices

import domain.{Point, Rule}

final class RTree extends Index {
  def findRules(point: Point): List[Rule] = ???
}

object RTree {
  def fromRules(rules: List[Rule]): RTree = ???
}

final class RTreeBuilder {
//  def
}