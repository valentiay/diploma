package core.generation

import core.domain.{Interval, Rule}
import zio.UIO

import scala.util.Random

object UniformLimited {
  def genRuleUnsafe(dimensions: Int): Rule =
    Vector.fill(dimensions){
      val center = Random.nextDouble()
      val delta = Random.nextDouble() / 20
      val a = Interval(center - delta, center + delta)
//      println(a)
      a
    }

  def genRule(dimensions: Int): UIO[Rule] =
    UIO.apply(genRuleUnsafe(dimensions))
}
