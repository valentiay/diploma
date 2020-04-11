package indices

import java.util.UUID

import domain.{Every, Exact, Greater, Interval, Less, Point, Rule}
import cats.instances.vector._
import cats.instances.option._
import cats.syntax.traverse._

class MBR private(val starts: Array[Double], val ends: Array[Double]) {

  def plusPoint(point: Point): MBR = {
    val newStarts = new Array[Double](starts.length)
    val newEnds = new Array[Double](ends.length)
    var i = 0;
    while (i < starts.length) {
      val x = point.values(i)
      newStarts.update(i, Math.min(starts(i), x))
      newEnds.update(i, Math.max(ends(i), x))
      i += 1
    }
    new MBR(newStarts, newEnds)
  }

  def intersect(other: MBR): Option[MBR] = {
    val newStarts = new Array[Double](starts.length)
    val newEnds = new Array[Double](ends.length)
    var i = 0
    var intersects = true
    while (i < starts.length) {
      val newStart = Math.max(starts(i), other.starts(i))
      val newEnd = Math.min(ends(i), other.ends(i))
      if (newStart > newEnd) intersects = false
      newStarts.update(i, newStart)
      newEnds.update(i, newEnd)
      i += 1
    }
    if (intersects) {
      Some(new MBR(newStarts, newEnds))
    } else {
      None
    }
  }

  def plusMBR(other: MBR): MBR = {
    val newStarts = new Array[Double](starts.length)
    val newEnds = new Array[Double](ends.length)
    var i = 0
    while (i < starts.length) {
      newStarts.update(i, Math.min(starts(i), other.starts(i)))
      newEnds.update(i, Math.max(ends(i), other.ends(i)))
      i += 1
    }
    new MBR(newStarts, newEnds)
  }

  def containsPoint(point: Point): Boolean = {
    var i = 0
    var contains = true
    while (i < starts.length) {
      val x = point.values(i)
      contains = contains && starts(i) - 0.00001 < x && x < ends(i) + 0.00001
      i += 1
    }
    contains
  }

  def isContainedByMBR(other: MBR): Boolean = {
    var i = 0
    var isContained = true
    while (i < starts.length) {
      isContained = isContained && other.starts(i) < starts(i) && ends(i) < other.ends(i)
      i += 1
    }
    isContained
  }

  def volume: Double = {
    var i = 0
    var volume = 1d
    while (i < starts.length) {
      volume *= ends(i) - starts(i)
      i += 1
    }
    volume
  }

  def margin: Double = {
    var i = 0
    var margin = 0d
    while (i < starts.length) {
      margin += ends(i) - starts(i)
      i += 1
    }
    margin
  }

  def toPoint: Point =
    Point(starts)

  override def toString: String = {
    (for (i <- starts.indices) yield (starts(i).formatted("%1.3f"), ends(i).formatted("%1.3f"))).mkString("[", ",", "]")
  }
}

object MBR {
  def fromPoints(points: Iterable[Point]): MBR = {
    val head = points.head
    val starts = new Array[Double](head.values.length)
    val ends = new Array[Double](head.values.length)

    var i = 0
    while (i < head.values.length) {
      starts.update(i, head.values(i))
      ends.update(i, head.values(i))
      i += 1
    }

    points.tail.foreach { point =>
      var j = 0
      while (j < point.values.length) {
        starts.update(j, Math.min(starts(i), point.values(j)))
        ends.update(j, Math.max(ends(i), point.values(j)))
        j += 1
      }
    }
    points.tail.foldLeft(MBR.single(points.head))((mbr, point) => mbr.plusPoint(point))
  }

  def fromMBRs(mbrs: Iterable[MBR]): Option[MBR] =
    mbrs.reduceOption(_ plusMBR _)

  def fromMBRsUnsafe(mbrs: Iterable[MBR]): MBR =
    mbrs.reduce(_ plusMBR _)

  def single(point: Point): MBR =
    new MBR(point.values, point.values)

  def fromRule(rule: Rule): MBR = {
    val starts = new Array[Double](rule.size)
    val ends = new Array[Double](rule.size)
    var i = 0
    while (i < rule.size) {
      rule(i) match {
        case Exact(value) =>
          starts.update(i, value)
          ends.update(i, value)

        case Interval(from, to) =>
          starts.update(i, from)
          ends.update(i, to)

        case Less(to) =>
          starts.update(i, 0)
          ends.update(i, to)

        case Greater(from) =>
          starts.update(i, from)
          ends.update(i, 1)

        case Every =>
          starts.update(i, 0)
          ends.update(i, 1)
      }
      i += 1
    }
    new MBR(starts, ends)
  }
}