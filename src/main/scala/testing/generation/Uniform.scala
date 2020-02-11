package testing.generation

import domain.{Every, Exact, Greater, Interval, Less, Point, Rule}

import scala.util.Random

object Uniform {
  def generateRule(dimensions: Int): Rule =
    Vector.fill(dimensions) {
      Random.nextInt(5) match {
        case 0 =>
          Exact(Random.nextDouble())
        case 1 =>
          val center = Random.nextDouble()
          val delta = Random.nextDouble()
          Interval(center - delta, center + delta)
        case 2 =>
          Less(Random.nextDouble())
        case 3 =>
          Greater(Random.nextDouble())
        case 4 =>
          Every
      }
    }

  def generateRules(dimensions: Int, size: Int): List[Rule] =
    List.fill(size)(generateRule(dimensions))

  def generatePoints(dimesnsions: Int, size: Int): List[Point] =
    List.fill(size)(Vector.fill(dimesnsions)(Random.nextDouble()))
}