package core.generation

import core.domain.{Interval, Point, Rule}
import zio.UIO

import scala.util.Random

object Gaussian {
  def genRule(dimensions: Int): UIO[Rule] =
    UIO.apply(
      Vector.fill(dimensions) {
        val gaussian = Random.nextGaussian() / 3
        if (gaussian < 0) {
          Interval(1 + gaussian, 1)
        } else {
          Interval(0, gaussian)
        }
      }
    )

  def genPoint(dimensions: Int): UIO[Point] =
    UIO.apply(Point(Array.fill(dimensions)(Random.nextGaussian() / 3 + 0.5)))
}
