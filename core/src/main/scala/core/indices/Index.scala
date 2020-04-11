package core.indices

import core.domain.{Match, Point}
import fs2.Stream

trait Index {
  def findRules(points: Stream[ERIO, Point]): Stream[ERIO, Match]
}
