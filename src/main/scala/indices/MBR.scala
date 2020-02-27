package indices

import domain.{Every, Exact, Greater, Interval, Less, Point, Rule}
import cats.instances.vector._
import cats.instances.option._
import cats.syntax.traverse._

class MBR private(val segments: Vector[(Double, Double)]) extends AnyVal {
  def plusPoint(point: Point): MBR =
    new MBR(segments.zip(point).map { case ((a, b), x) => (Math.min(a, x), Math.max(b, x)) })

  def volume: Double = {
    val volume = segments.foldLeft(1.0) { case (acc, (a, b)) => acc * (b - a) }
    //    println(s"Volume: $volume")
    volume
  }

  def intersect(other: MBR): Option[MBR] =
    segments.zip(other.segments)
      .traverse { case ((a1, b1), (a2, b2)) =>
        Some((Math.max(a1, a2), Math.min(b1, b2)))
          .filter { case (a, b) => a <= b }
      }.map(new MBR(_))

  def plusMBR(other: MBR): MBR =
    new MBR(segments.zip(other.segments).map { case ((a1, b1), (a2, b2)) => (Math.min(a1, a2), Math.max(b1, b2)) })

  def contains(point: Point): Boolean =
    segments.zip(point).forall { case ((a, b), x) => a - 0.00001 < x && x < b + 0.00001 }

  def isContainedBy(other: MBR): Boolean =
    segments.zip(other.segments).forall { case ((a1, b1), (a2, b2)) => a2 < a1 && b1 < b2 }

  def margin: Double =
    segments.foldLeft(0d) { case (acc, (a, b)) => acc + b - a }
}

object MBR {
  def apply(points: Iterable[Point]): MBR = {
    points.tail.foldLeft(MBR.single(points.head))((mbr, point) => mbr.plusPoint(point))
  }

  def fromMBRs(mbrs: Iterable[MBR]): Option[MBR] =
    mbrs.reduceOption(_ plusMBR _)

  def single(point: Point): MBR =
    new MBR(point.map(x => (x, x)))

  def fromRule(rule: Rule): MBR =
    new MBR(rule.map({
      case Exact(value) => (value, value)
      case Interval(from, to) => (from, to)
      case Less(to) => (0, to)
      case Greater(from) => (from, 1)
      case Every => (0, 1)
    }))
}