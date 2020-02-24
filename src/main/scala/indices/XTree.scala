package indices

import domain.Point
import indices.XTreeBuilder.XTreeConfig

import scala.collection.mutable.ArrayBuffer

class XTreeBuilder(config: XTreeConfig) {
  var root: Option[XTreeVertex] = None

  def insert(point: Point): Unit =
    root match {
      case Some(value) =>
        value match {
          case leaf: XTreeLeaf =>
            leaf.insert(point) match {
              case Left(value) =>
                root = Some(new XTreeNode(value, config, 1))
              case Right(value) =>
            }
          case node: XTreeNode =>
            node.insert(point) match {
              case Left(value) =>
                root = Some(new XTreeNode(value, config, 1))
              case Right(value) =>
            }
        }
      case None =>
        root = Some(XTreeLeaf.fromPoint(point, config))
    }

  def printMe(): Unit = root.foreach(_.printMe)
}

sealed trait XTreeVertex {
  def config: XTreeConfig

  def mbr: MBR

  def insert(point: Point): Either[ArrayBuffer[XTreeVertex], XTreeVertex]

  def printMe(): Unit
}

class XTreeLeaf(val elements: ArrayBuffer[Point], val config: XTreeConfig, var blocks: Int) extends XTreeVertex {

  def chooseDimension: Int = {
    var i = 0
    var mini = 0
    var mins = 1e200
    while (i >= config.dimensions) {
      val sorted = elements.sortBy(element => element(i))
      var k = config.minChildren
      var s = 1e200
      while (k < config.maxChildren - config.minChildren) {
        val mbr1 = MBR(sorted.take(k))
        val mbr2 = MBR(sorted.drop(k))
        s = Math.min(s, mbr1.intersect(mbr2).map(_.volume).getOrElse(0d))
        k += 1
      }
      if (s < mins) {
        mins = s
        mini = i
      }
      i += 1
    }
    println(s"Axis chosen: $mini")
    mini
  }

  def split(): Either[ArrayBuffer[XTreeVertex], XTreeVertex] = {
    val dimension = chooseDimension
    val sorted = elements.sortBy(element => element(dimension))
    var i = config.minChildren
    var mini = 0
    var minOverlap = 1d
    while (i < config.maxChildren - config.minChildren) {
      val mbr1 = MBR(sorted.take(i))
      val mbr2 = MBR(sorted.drop(i))
//      println(mbr1.segments)
//      println(mbr2.segments)
      val overlap =
        mbr1.intersect(mbr2).map(_.volume).getOrElse(0d) / Math.min(mbr1.volume, mbr2.volume)
      //      println(s"Overlap: $overlap")
      if (overlap < minOverlap) {
        minOverlap = overlap
        mini = i
      }
      i += 1
    }
    val left = sorted.take(mini)
    val right = sorted.drop(mini)
    //    println(s"Left: $left")
    //    println(s"Right: $right")
    val leftMbr = MBR(left)
    val rightMbr = MBR(right)
    val overlap = leftMbr.intersect(rightMbr).map(_.volume).getOrElse(0d) / leftMbr.plusMBR(rightMbr).volume
    println(s"Leaf overlap: $overlap")
    if (overlap < config.maxOverlap) {
      //      println("Leaf split successful")
      Left(
        ArrayBuffer.apply[XTreeVertex](
          new XTreeLeaf(left, config, left.size / config.maxChildren + 1),
          new XTreeLeaf(right, config, right.size / config.maxChildren + 1)
        )
      )
    } else {
      println("Leaf split unsuccessful")
      blocks = elements.size / config.maxChildren + 1
      Right(this)
    }
  }

  def mbr: MBR = MBR(elements)

  def insert(point: Point): Either[ArrayBuffer[XTreeVertex], XTreeVertex] = {
    elements.addOne(point) // Вставить и выполнить корректировку
    if (elements.size > config.maxChildren) {
      split()
    } else {
      Right(this)
    }
  }

  def printMe(): Unit = {
    println(s"Leaf: ${elements.size} elements, $blocks blocks")
  }
}

object XTreeLeaf {
  def fromPoint(point: Point, config: XTreeConfig) = new XTreeLeaf(ArrayBuffer(point), config, 1)
}

class XTreeNode(val children: ArrayBuffer[XTreeVertex], val config: XTreeConfig, var blocks: Int) extends XTreeVertex {
  val mbr: MBR = MBR.fromMBRs(children.map(_.mbr)).get

  def chooseDimension: Int = {
    var i = 0
    var mini = 0
    var mins = 1e200
    while (i >= config.dimensions) {
      val sorted = children.sortBy(element => element.mbr.segments(i)._1) // !!!
      var k = config.minChildren
      var s = 1e200
      while (k < config.maxChildren - config.minChildren) {
        val mbr1 = MBR.fromMBRs(sorted.take(k).map(_.mbr)).get // !!!
        val mbr2 = MBR.fromMBRs(sorted.drop(k).map(_.mbr)).get // !!!
        s = Math.min(s, mbr1.intersect(mbr2).map(_.volume).getOrElse(0d))
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

  def split(): Either[ArrayBuffer[XTreeVertex], XTreeVertex] = {
    val dimension = chooseDimension
    val sorted = children.sortBy(element => element.mbr.segments(dimension)) // !!!
    var i = config.minChildren
    var mini = 0
    var minOverlap = 1d
    while (i < config.maxChildren - config.minChildren) {
      val mbr1 = MBR.fromMBRs(sorted.take(i).map(_.mbr)).get // !!!
      val mbr2 = MBR.fromMBRs(sorted.drop(i).map(_.mbr)).get // !!!
      val overlap =
        mbr1.intersect(mbr2).map(_.volume).getOrElse(0d) / Math.min(mbr1.volume, mbr2.volume)
      if (overlap < minOverlap) {
        minOverlap = overlap
        mini = i
      }
      i += 1
    }
    val left = sorted.take(mini)
    val right = sorted.drop(mini)
    val leftMbr = MBR.fromMBRs(left.map(_.mbr)).get // !!!
    val rightMbr = MBR.fromMBRs(right.map(_.mbr)).get // !!!
    val overlap = leftMbr.intersect(rightMbr).map(_.volume).getOrElse(0d) / leftMbr.plusMBR(rightMbr).volume
    println(s"Overlap: $overlap")
    if (overlap < config.maxOverlap) {
//      println("Node split successfull")
      Left(
        ArrayBuffer.apply[XTreeVertex](
          new XTreeNode(left, config, left.size / config.maxChildren + 1),
          new XTreeNode(right, config, right.size / config.maxChildren + 1)
        )
      )
    } else {
      println("Node split unsuccessful")
      blocks = children.size / config.maxChildren + 1
      Right(this)
    }
  }

  def insert(point: Point): Either[ArrayBuffer[XTreeVertex], XTreeVertex] = {
    var minMbr = children(0).mbr.plusPoint(point).volume
    var mini = 0
    for (i <- children.indices.tail) {
      val newMbr = children(i).mbr.plusPoint(point).volume
      if (newMbr < minMbr) {
        minMbr = newMbr
        mini = i
      }
    }
    val splitResult = children(mini).insert(point)
    splitResult match {
      case Left(value) =>
        children.remove(mini)
        children.addAll(value)
      case Right(value) =>
    }
    if (children.size > config.maxChildren) {
      split()
    } else {
      Right(this)
    }
  }

  def printMe(): Unit = {
    println(s"Node: ${children.size} children, $blocks blocks")
    children.foreach(_.printMe())
  }
}

object XTreeBuilder {

  def fromPoints(config: XTreeConfig, points: Iterable[Point]): XTreeBuilder = {
    val bldr = new XTreeBuilder(config)
    points.foreach(bldr.insert)
    bldr
  }


  final case class XTreeConfig(minChildren: Int, maxChildren: Int, dimensions: Int, maxOverlap: Double)

}