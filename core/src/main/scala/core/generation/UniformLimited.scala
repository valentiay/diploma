package core.generation

import core.domain.{Interval, Rule}
import zio.UIO

import scala.util.Random

object UniformLimited {
  def genRuleUnsafe(dimensions: Int): Rule =
    Vector.fill(dimensions){
      val center = Random.nextDouble()
      val delta = Random.nextDouble() * dimensions / 32
      Interval(center - delta, center + delta)
    }

  def genRule(dimensions: Int): UIO[Rule] =
    UIO.apply(genRuleUnsafe(dimensions))
}
