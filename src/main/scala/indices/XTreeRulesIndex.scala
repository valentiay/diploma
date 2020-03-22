package indices
import domain.{Point, Rule}
import zio.Task
import fs2.Stream
import indices.XTreeBuilder.XTreeConfig

class XTreeRulesIndex(rules: List[Rule], config: XTreeConfig) extends Index {

  val index = XTreeBuilder.fromRules(config, rules)

  def findRules(points: Stream[Task, Point]): Stream[Task, (Point, Rule)] = {
    points.map(point => ???/*index.findPoint(point)*/)
  }
}
