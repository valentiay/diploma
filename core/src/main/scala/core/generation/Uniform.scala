package core.generation

import core.domain.{Every, Exact, Greater, Interval, Less, Point, Rule}
import zio.UIO

import scala.util.Random

object Uniform {
  def genRuleUnsafe(dimensions: Int): Rule =
    Vector.fill(dimensions) {
      Random.nextInt(10) match {
        case 0 =>
          Exact(Random.nextDouble())
        case 1 =>
          Less(Random.nextDouble())
        case 2 =>
          Greater(Random.nextDouble())
        case 3 =>
          Every
        case _ =>
          val center = Random.nextDouble()
          val delta = Random.nextDouble()
          Interval(center - delta, center + delta)
      }
    }

  def genRule(dimensions: Int): UIO[Rule] =
    UIO.apply(genRuleUnsafe(dimensions))

  def genPointUnsafe(dimensions: Int): Point =
    Point(Array.fill(dimensions)(Random.nextDouble()))

  def genPoint(dimensions: Int): UIO[Point] =
    UIO.apply(genPointUnsafe(dimensions))
}