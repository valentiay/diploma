package core.domain

sealed trait Restriction {
  def satisfied(a: Double): Boolean
}

final case class Exact(value: Double) extends Restriction {
  def satisfied(a: Double): Boolean =
    Math.abs(value - a) < 0.00001
}

final case class Interval(from: Double, to: Double) extends Restriction {
  def satisfied(a: Double): Boolean = from < a && a < to
}

final case class Less(to: Double) extends Restriction {
  def satisfied(a: Double): Boolean = a < to
}

final case class Greater(from: Double) extends Restriction {
  def satisfied(a: Double): Boolean = from < a
}

object Every extends Restriction {
  def satisfied(a: Double): Boolean = true
}