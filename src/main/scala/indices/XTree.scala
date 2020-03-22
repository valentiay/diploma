package indices

import domain.{Point, Rule}
import indices.XTreeBuilder.XTreeConfig

import scala.collection.immutable.BitSet
import scala.collection.mutable.ArrayBuffer

class XTreeBuilder(config: XTreeConfig) {
  var root: Option[XTreeVertex] = None

  def insert(point: Point): Unit =
    root match {
      case Some(value) =>
        value.insert(point).foreach { case (a, b, dimension) =>
          root = Some(new XTreeNode(ArrayBuffer(a, b), config, 1, a.mbr.plusMBR(b.mbr), BitSet(dimension)))
        }
      case None =>
        root = Some(XTreeLeaf.fromPoint(point, config))
    }

  def find(rule: Rule): List[Point] =
    root.map(_.find(MBR.fromRule(rule)).toList).getOrElse(Nil)

  def printMe(): Unit = root.foreach(_.printMe())
}

sealed trait XTreeVertex {
  def config: XTreeConfig

  def mbr: MBR

  def insert(point: Point): Option[(XTreeVertex, XTreeVertex, Int)]

  def printMe(offset: Int = 0): Unit

  var isLeft: Option[Boolean] = None

  def find(rule: MBR): ArrayBuffer[Point]

  def history: BitSet
}

object XTreeVertex {
  def chooseDimension[T](config: XTreeConfig, elements: ArrayBuffer[T], toMbr: T => MBR): Int = {
    var i = 0
    var mini = 0
    var mins = 1e200
    while (i < config.dimensions) {
      val sorted = elements.sortBy(element => toMbr(element).segments(i)._1)
      var k = config.minChildren
      var s = 0d
      while (k < config.maxChildren - config.minChildren) {
        val mbr1 = MBR.fromMBRs(sorted.take(k).map(toMbr)).get.margin
        val mbr2 = MBR.fromMBRs(sorted.drop(k).map(toMbr)).get.margin
        s += mbr1 + mbr2
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

  def splitByAxis[T](config: XTreeConfig, elements: ArrayBuffer[T], toMbr: T => MBR)(dimension: Int): Option[(ArrayBuffer[T], ArrayBuffer[T], Int)] = {
    val sorted = elements.sortBy(element => toMbr(element).segments(dimension)._1)
    var i = config.minChildren
    var mini = 0
    var minOverlap = 1d
    while (i < config.maxChildren - config.minChildren) {
      val mbr1 = MBR.fromMBRs(sorted.take(i).map(toMbr)).get
      val mbr2 = MBR.fromMBRs(sorted.drop(i).map(toMbr)).get
      val overlap = mbr1.intersect(mbr2).map(_.volume).getOrElse(0d) / Math.min(mbr1.volume, mbr2.volume)
      if (overlap < minOverlap) {
        minOverlap = overlap
        mini = i
      }
      i += 1
    }
    val left = sorted.take(mini)
    val right = sorted.drop(mini)
    if (minOverlap < config.maxOverlap) {
      Some((left, right, dimension))
    } else {
      None
    }
  }


  def topologicalSplit[T](config: XTreeConfig, elements: ArrayBuffer[T], toMbr: T => MBR): Option[(ArrayBuffer[T], ArrayBuffer[T], Int)] =
    splitByAxis(config, elements, toMbr)(chooseDimension(config, elements, toMbr))
}

class XTreeLeaf(val elements: ArrayBuffer[Point], val config: XTreeConfig, var blocks: Int, var mbr: MBR, val history: BitSet) extends XTreeVertex {

  def split(): Option[(XTreeVertex, XTreeVertex, Int)] = {
    XTreeVertex.topologicalSplit(config, elements, point => MBR.single(point)) match {
      case Some((left, right, dimension)) =>
        Some(
          (
            new XTreeLeaf(left, config, left.size / config.maxChildren + 1, MBR.apply(left), history + dimension),
            new XTreeLeaf(right, config, right.size / config.maxChildren + 1, MBR.apply(right), history + dimension),
            dimension
          )
        )

      case None =>
        blocks = elements.size / config.maxChildren + 1
        None
    }
  }

  def insert(point: Point): Option[(XTreeVertex, XTreeVertex, Int)] = {
    elements.addOne(point) // Вставить и выполнить корректировку
    mbr = mbr.plusPoint(point)
    if (elements.size > config.maxChildren * blocks) {
      split()
    } else {
      None
    }
  }

  def printMe(offset: Int = 0): Unit = {
    if (MBR.apply(elements).volume != mbr.volume) println(s"NE: ${MBR.apply(elements).volume}, ${mbr.volume}")
    println(" " * offset + s"Leaf: ${elements.size} elements, $blocks blocks, ${mbr.segments.map { case (a, b) => (a.formatted("%1.3f"), b.formatted("%1.3f")) }.mkString("[", ",", "]")}")
  }

  def find(rule: MBR): ArrayBuffer[Point] = {
    elements.flatMap(point =>
      Some(point).filter(rule.contains)
    )
  }
}

object XTreeLeaf {
  def fromPoint(point: Point, config: XTreeConfig) = new XTreeLeaf(ArrayBuffer(point), config, 1, MBR.single(point), BitSet.empty)
}

class XTreeNode(val children: ArrayBuffer[XTreeVertex], val config: XTreeConfig, var blocks: Int, var mbr: MBR, val history: BitSet) extends XTreeVertex {

  def minOverlapSplit: Option[(ArrayBuffer[XTreeVertex], ArrayBuffer[XTreeVertex], Int)] =
  {
    val possibleAxes = children.map(_.history).reduce(_ intersect _)
    possibleAxes.foldLeft[Option[(ArrayBuffer[XTreeVertex], ArrayBuffer[XTreeVertex], Int)]](None){
      case (None, dimension) => XTreeVertex.splitByAxis[XTreeVertex](config, children, _.mbr)(dimension)
      case (res, _) => res
    }
  }

  def split(): Option[(XTreeVertex, XTreeVertex, Int)] = {
    XTreeVertex.topologicalSplit[XTreeVertex](config, children, _.mbr).orElse(minOverlapSplit) match {
      case Some((left, right, dimension)) =>
        Some(
          (
            new XTreeNode(left, config, left.size / config.maxChildren + 1, MBR.fromMBRs(left.map(_.mbr)).get, history + dimension),
            new XTreeNode(right, config, right.size / config.maxChildren + 1, MBR.fromMBRs(right.map(_.mbr)).get, history + dimension),
            dimension
          )
        )
      case None =>
        blocks = children.size / config.maxChildren + 1
        None
    }
  }

  def insert(point: Point): Option[(XTreeVertex, XTreeVertex, Int)] = {
    var minEnlargment = children(0).mbr.plusPoint(point).volume - children(0).mbr.volume
    var mini = 0
    for (i <- children.indices.tail) {
      val newEnlargment = children(i).mbr.plusPoint(point).volume - children(i).mbr.volume
      if (newEnlargment < minEnlargment) {
        minEnlargment = newEnlargment
        mini = i
      }
    }
    mbr = mbr.plusPoint(point)
    children(mini).insert(point).foreach {
      case (a, b, _) =>
        if (children.size == 2) {
          children(0).isLeft = Some(true)
          children(1).isLeft = Some(false)
        }
        a.isLeft = children(mini).isLeft
        b.isLeft = children(mini).isLeft
        children.remove(mini)
        children.addOne(a)
        children.addOne(b)

    }
    if (children.size > config.maxChildren * blocks) {
      split()
    } else {
      None
    }
  }

  def printMe(offset: Int = 0): Unit = {
    println(" " * offset + s"Node: ${children.size} children, $blocks blocks, ${mbr.segments.map { case (a, b) => (a.formatted("%1.3f"), b.formatted("%1.3f")) }.mkString("[", ",", "]")}")
    children.sortBy(_.mbr.segments.head._1).foreach(_.printMe(offset + 4))
  }

  def find(rule: MBR): ArrayBuffer[Point] = {
    children.flatMap(vertex =>
      if (vertex.mbr.intersect(rule).nonEmpty) {
        vertex.find(rule)
      } else {
        ArrayBuffer.empty
      }
    )
  }
}

object XTreeBuilder {

  def fromPoints(config: XTreeConfig, points: Iterable[Point]): XTreeBuilder = {
    val bldr = new XTreeBuilder(config)
    points.foreach(bldr.insert)
    bldr
  }

  def fromChunk(config: XTreeConfig, points: fs2.Chunk[Point]): XTreeBuilder = {
    val bldr = new XTreeBuilder(config)
    points.foreach(bldr.insert)
    bldr
  }


  final case class XTreeConfig(minChildren: Int, maxChildren: Int, dimensions: Int, maxOverlap: Double)

}