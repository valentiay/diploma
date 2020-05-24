package core.generation

import core.domain.{Interval, Rule}
import zio.UIO

import scala.util.Random

object UniformDiscrete {
  val intervals =
    Vector(
      Interval(0.0, 0.1),
      Interval(0.1, 0.2),
      Interval(0.2, 0.3),
      Interval(0.3, 0.4),
      Interval(0.4, 0.5),
      Interval(0.5, 0.6),
      Interval(0.6, 0.7),
      Interval(0.7, 0.8),
      Interval(0.8, 0.9),
      Interval(0.9, 1.0)
    )

  def genRuleUnsafe(dimensions: Int): Rule =
    Vector.fill(dimensions)(intervals(Random.nextInt(intervals.size)))

  def genRule(dimensions: Int): UIO[Rule] =
    UIO.apply(genRuleUnsafe(dimensions))
}
