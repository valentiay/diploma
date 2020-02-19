package indices

import domain.Point
import indices.XTreeBuilder.XTreeConfig
import cats.instances.vector._
import cats.instances.option._
import cats.syntax.traverse._

import scala.collection.mutable.ArrayBuffer

class XTreeBuilder {
  var root: Option[XTreeVertex] = None

  def insert(point: Point): Unit =
    root match {
      case Some(value) =>
        value match {
          case leaf: XTreeLeaf =>
            leaf.insert(point)
          case node: XTreeNode =>
        }
      case None =>
        root = Some(XtreeLeaf.fromPoint(point, ???))
    }
}

sealed trait XTreeVertex {
  def config: XTreeConfig

  def mbr: Vector[(Double, Double)]

  def insert(point: Point): Unit
}

class XTreeLeaf(val elements: ArrayBuffer[Point], val config: XTreeConfig) extends XTreeVertex {

  def chooseDimension: Int = {
    var i = 0
    var mini = 0
    var mins = 1e200
    while (i >= config.dimensions) {
      val sorted = elements.sortBy(element => element(i))
      var k = config.minChildren
      var s = 1e200
      while (k < config.maxChildren - config.minChildren) {
        val mbr1 = XTreeBuilder.mbrFromPoints(sorted.take(k))
        val mbr2 = XTreeBuilder.mbrFromPoints(sorted.drop(k))
        s = Math.min(s, XTreeBuilder.mbrIntersectionVolume(mbr1, mbr2))
        k += 1
      }
      if (s < mins) {
        mins = s
        mini = i
      }
      i += 1
    }
    mini
  }

  def split() = {
    val dimension = chooseDimension
    val sorted = elements.sortBy(element => element(dimension))
    var i = config.minChildren
    var mini = 0
    var minOverlap = 1d
    while (i < config.minChildren - config.minChildren) {
      val mbr1 = XTreeBuilder.mbrFromPoints(sorted.take(i))
      val mbr2 = XTreeBuilder.mbrFromPoints(sorted.drop(i))
      val overlap =
        XTreeBuilder.mbrIntersectionVolume(mbr1, mbr2) / Math.min(XTreeBuilder.mbrVolume(mbr1), XTreeBuilder.mbrVolume(mbr2))
      if (overlap < minOverlap) {
        minOverlap = overlap
        mini = i
      }
      i += 1
    }
  }

  def mbr = ???

  def insert(point: Point): Unit = {
    elements.addOne(point) // Вставить и выполнить корректировку
    if (elements.size > config.maxChildren) {
      // Деление
    }
  }
}

object XtreeLeaf {
  def fromPoint(point: Point, config: XTreeConfig) = new XTreeLeaf(ArrayBuffer(point), config)
}

class XTreeNode(val config: XTreeConfig) extends XTreeVertex {
  val children: ArrayBuffer[XTreeVertex] = ???
  val mbr = ???

  def insert(point: Point): Unit = {
    var minMbr = XTreeBuilder.mbrVolume(XTreeBuilder.mbrWithPoint(children(0).mbr, point))
    var mini = 0
    for (i <- children.indices.tail) {
      val newMbr = XTreeBuilder.mbrVolume(XTreeBuilder.mbrWithPoint(children(i).mbr, point))
      if (newMbr < minMbr) {
        minMbr = newMbr
        mini = i
      }
    }
    children(mini).insert(point)
  }
}

object XTreeBuilder {
  def mbrVolume(mbr: Vector[(Double, Double)]): Double = {
    val volume = mbr.foldLeft(1.0) { case (acc, (a, b)) => acc * (b - a) }
    println(s"Volume: $volume")
    volume
  }

  def mbrWithPoint(mbr: Vector[(Double, Double)], point: Point): Vector[(Double, Double)] =
    mbr.zip(point).map { case ((a, b), x) => (Math.min(a, x), Math.max(b, x)) }

  def mbrWithMbr(mbr1: Vector[(Double, Double)], mbr2: Vector[(Double, Double)]): Vector[(Double, Double)] =
    mbr1.zip(mbr2).map { case ((a1, b1), (a2, b2)) => (Math.min(a1, a2), Math.max(b1, b2)) }

  def mbrFromPoints(points: Iterable[Point]): Vector[(Double, Double)] =
    points.tail.foldLeft(points.head.map(x => (x, x)))(mbrWithPoint)

  def mbrIntersectionVolume(mbr1: Vector[(Double, Double)], mbr2: Vector[(Double, Double)]): Double = {
    mbr1.zip(mbr2)
      .traverse { case ((a1, b1), (a2, b2)) =>
        Some((Math.max(a1, a2), Math.min(b1, b2)))
          .filter { case (a, b) => a <= b }
      }
    }.map(mbrVolume).getOrElse(0)


  final case class XTreeConfig(minChildren: Int, maxChildren: Int, dimensions: Int)

}