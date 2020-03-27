package benchmarking.generation

import domain.{Interval, Rule}
import zio.UIO

import scala.util.Random

object UniformDiscrete {
  def genRuleUnsafe(dimensions: Int): Rule =
    Vector.fill(dimensions){
      val center = (Random.nextInt(10).toDouble + 0.5) / 10
      val delta = Random.nextDouble() / 20
      val a = Interval(center - delta, center + delta)
//      println(a)
      a
    }

  def genRule(dimensions: Int): UIO[Rule] =
    UIO.apply(genRuleUnsafe(dimensions))
}
