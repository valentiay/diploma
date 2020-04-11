package indices

import domain.{Match, Point}
import fs2.Stream

trait Index {
  def findRules(points: Stream[ERIO, Point]): Stream[ERIO, Match]
}
