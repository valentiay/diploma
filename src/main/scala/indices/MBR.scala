package indices

import domain.Point
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
}

object MBR {
  def apply(points: Iterable[Point]): MBR = {
    points.tail.foldLeft(new MBR(points.head.map(x => (x, x))))((mbr, point) => mbr.plusPoint(point))
  }

  def fromMBRs(mbrs: Iterable[MBR]): Option[MBR] =
    mbrs.reduceOption(_ plusMBR _)
}