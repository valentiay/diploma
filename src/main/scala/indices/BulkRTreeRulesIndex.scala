package indices
import domain.{Point, Rule}
import zio.Task
import fs2.Stream
import indices.XTreeBuilder.XTreeConfig

class BulkRTreeRulesIndex(rules: List[Rule], config: XTreeConfig) extends Index {

  val index = BulkRTreeVertex.build(config.dimensions, config.maxChildren, rules.map(MBR.fromRule))

  def findRules(points: Stream[Task, Point]): Stream[Task, (Point, Rule)] = {
    points.map { point =>
      index.findPoint(point)
      (Point(Array()), Vector())
    }
  }
}
