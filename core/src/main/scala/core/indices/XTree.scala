package core.indices

import java.util.UUID

import cats.data.Chain
import core.domain.{Point, Rule}
import core.indices.XTreeBuilder.XTreeConfig

import scala.collection.immutable.BitSet
import scala.collection.mutable.ArrayBuffer

class XTreeBuilder(config: XTreeConfig) {
  var root: Option[XTreeVertex] = None

  private val uuidStub = UUID.randomUUID()

  def insertMBR(mbr: (MBR, UUID)): Unit =
    root match {
      case Some(value) =>
        value.insert(mbr).foreach { case (a, b, dimension) =>
          root = Some(new XTreeNode(ArrayBuffer(a, b), config, 1, a.vertexMBR.plusMBR(b.vertexMBR), BitSet(dimension)))
        }
      case None =>
        root = Some(XTreeLeaf.fromMBR(mbr, config))
    }

  def insertPoint(point: Point): Unit =
    insertMBR((MBR.single(point), uuidStub))

  def findMBR(mbr: MBR): Chain[(MBR, UUID)] =
    root.map(_.find(mbr)).getOrElse(Chain.empty)

  def findPoint(point: Point): Chain[(MBR, UUID)] =
    root.map(_.findPoint(point)).getOrElse(Chain.empty)

  def printMe(): Unit = root.foreach(_.printMe())
}

sealed trait XTreeVertex {
  def config: XTreeConfig

  def vertexMBR: MBR

  def insert(point: (MBR, UUID)): Option[(XTreeVertex, XTreeVertex, Int)]

  def printMe(offset: Int = 0): Unit

  var isLeft: Option[Boolean] = None

  def find(mbr: MBR): Chain[(MBR, UUID)]

  def findPoint(point: Point): Chain[(MBR, UUID)]

  def history: BitSet
}

object XTreeVertex {
  def chooseDimension[T](config: XTreeConfig, elements: ArrayBuffer[T], toMbr: T => MBR): Int = {
    var i = 0
    var mini = 0
    var mins = 1e200
    while (i < config.dimensions) {
      val sorted = elements.sortBy(element => toMbr(element).starts(i))
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
    val sorted = elements.sortBy(element => toMbr(element).starts(dimension))
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

class XTreeLeaf(val elements: ArrayBuffer[(MBR, UUID)], val config: XTreeConfig, var blocks: Int, var vertexMBR: MBR, val history: BitSet) extends XTreeVertex {

  def split(): Option[(XTreeVertex, XTreeVertex, Int)] = {
    XTreeVertex.topologicalSplit[(MBR, UUID)](config, elements, _._1) match {
      case Some((left, right, dimension)) =>
        Some(
          (
            new XTreeLeaf(left, config, left.size / config.maxChildren + 1, MBR.fromMBRsUnsafe(left.map(_._1)), history + dimension),
            new XTreeLeaf(right, config, right.size / config.maxChildren + 1, MBR.fromMBRsUnsafe(right.map(_._1)), history + dimension),
            dimension
          )
        )

      case None =>
        blocks = elements.size / config.maxChildren + 1
        None
    }
  }

  def insert(point: (MBR, UUID)): Option[(XTreeVertex, XTreeVertex, Int)] = {
    elements.addOne(point) // Вставить и выполнить корректировку
    vertexMBR = vertexMBR.plusMBR(point._1)
    if (elements.size > config.maxChildren * blocks) {
      split()
    } else {
      None
    }
  }

  def printMe(offset: Int = 0): Unit = {
    if (MBR.fromMBRsUnsafe(elements.map(_._1)).volume != vertexMBR.volume) println(s"NE: ${MBR.fromMBRsUnsafe(elements.map(_._1)).volume}, ${vertexMBR.volume}")
    println(" " * offset + s"Leaf: ${elements.size} elements, $blocks blocks, $vertexMBR")
  }

  def find(mbr: MBR): Chain[(MBR, UUID)] = {
    Chain.fromSeq(elements.view.filter(_._1.isContainedByMBR(mbr)).toSeq)
  }

  def findPoint(point: Point): Chain[(MBR, UUID)] = {
    Chain.fromSeq(elements.view.filter(_._1.containsPoint(point)).toSeq)
  }
}

object XTreeLeaf {
  def fromMBR(point: (MBR, UUID), config: XTreeConfig) = new XTreeLeaf(ArrayBuffer(point), config, 1, point._1, BitSet.empty)
}

class XTreeNode(val children: ArrayBuffer[XTreeVertex], val config: XTreeConfig, var blocks: Int, var vertexMBR: MBR, val history: BitSet) extends XTreeVertex {

  def minOverlapSplit: Option[(ArrayBuffer[XTreeVertex], ArrayBuffer[XTreeVertex], Int)] = {
    val possibleAxes = children.map(_.history).reduce(_ intersect _)
    possibleAxes.foldLeft[Option[(ArrayBuffer[XTreeVertex], ArrayBuffer[XTreeVertex], Int)]](None) {
      case (None, dimension) => XTreeVertex.splitByAxis[XTreeVertex](config, children, _.vertexMBR)(dimension)
      case (res, _) => res
    }
  }

  def split(): Option[(XTreeVertex, XTreeVertex, Int)] = {
    XTreeVertex.topologicalSplit[XTreeVertex](config, children, _.vertexMBR).orElse(minOverlapSplit) match {
      case Some((left, right, dimension)) =>
        Some(
          (
            new XTreeNode(left, config, left.size / config.maxChildren + 1, MBR.fromMBRs(left.map(_.vertexMBR)).get, history + dimension),
            new XTreeNode(right, config, right.size / config.maxChildren + 1, MBR.fromMBRs(right.map(_.vertexMBR)).get, history + dimension),
            dimension
          )
        )
      case None =>
        blocks = children.size / config.maxChildren + 1
        None
    }
  }

  def insert(point: (MBR, UUID)): Option[(XTreeVertex, XTreeVertex, Int)] = {
    var minEnlargment = children(0).vertexMBR.plusMBR(point._1).volume - children(0).vertexMBR.volume
    var mini = 0
    for (i <- children.indices.tail) {
      val newEnlargment = children(i).vertexMBR.plusMBR(point._1).volume - children(i).vertexMBR.volume
      if (newEnlargment < minEnlargment) {
        minEnlargment = newEnlargment
        mini = i
      }
    }
    vertexMBR = vertexMBR.plusMBR(point._1)
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
    println(" " * offset + s"Node: ${children.size} children, $blocks blocks, $vertexMBR")
    children.sortBy(_.vertexMBR.starts.head).foreach(_.printMe(offset + 4))
  }

  def find(mbr: MBR): Chain[(MBR, UUID)] = {
    children.foldLeft(Chain.empty[(MBR, UUID)])((acc, vertex) =>
      if (vertex.vertexMBR.intersect(mbr).nonEmpty) {
        acc.concat(vertex.find(mbr))
      } else {
        acc
      }
    )
  }

  def findPoint(point: Point): Chain[(MBR, UUID)] = {
    children.foldLeft(Chain.empty[(MBR, UUID)])((acc, vertex) =>
      if (vertex.vertexMBR.containsPoint(point)) {
        acc.concat(vertex.findPoint(point))
      } else {
        acc
      }
    )
  }
}

object XTreeBuilder {

  def fromPoints(config: XTreeConfig, points: Iterable[Point]): XTreeBuilder = {
    val bldr = new XTreeBuilder(config)
    points.foreach(bldr.insertPoint)
    bldr
  }

  def fromRules(config: XTreeConfig, rules: Iterable[(Rule, UUID)]): XTreeBuilder = {
    val bldr = new XTreeBuilder(config)
    rules.foreach(rule => bldr.insertMBR((MBR.fromRule(rule._1), rule._2)))
    bldr
  }

  def fromChunk(config: XTreeConfig, points: fs2.Chunk[Point]): XTreeBuilder = {
    val bldr = new XTreeBuilder(config)
    points.foreach(bldr.insertPoint)
    bldr
  }


  final case class XTreeConfig(minChildren: Int, maxChildren: Int, dimensions: Int, maxOverlap: Double)

}