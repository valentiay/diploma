package indices

import domain.{Point, Rule}
import zio.Task
import fs2.Stream

trait Index {
  def findRules(points: Stream[Task, Point]): Stream[Task, (Point, Rule)]
}
