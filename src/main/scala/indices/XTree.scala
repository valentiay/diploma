package indices

import domain.{Point, Rule}
import indices.XTreeBuilder.XTreeConfig

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class XTreeBuilder(config: XTreeConfig) {
  var root: Option[XTreeVertex] = None

  def insert(point: Point): Unit =
    root match {
      case Some(value) =>
        value.insert(point).foreach { case (a, b) =>
          root = Some(new XTreeNode(ArrayBuffer(a, b), config, 1, a.mbr.plusMBR(b.mbr)))
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

  def insert(point: Point): Option[(XTreeVertex, XTreeVertex)]

  def printMe(offset: Int = 0): Unit

  var isLeft: Option[Boolean] = None

  def find(rule: MBR): ArrayBuffer[Point]
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
      //      println(s"S: $s")
      if (s < mins) {
        mins = s
        mini = i
      }
      i += 1
    }
    //    println(s"Axis chosen: $mini, $mins")
    mini
  }

  def topologicalSplit[T](config: XTreeConfig, elements: ArrayBuffer[T], toMbr: T => MBR): Option[(ArrayBuffer[T], ArrayBuffer[T])] = {
    val dimension = chooseDimension(config, elements, toMbr)
    val sorted = elements.sortBy(element => toMbr(element).segments(dimension)._1)
    var i = config.minChildren
    var mini = 0
    var minOverlap = 1d
    while (i < config.maxChildren - config.minChildren) {
      val mbr1 = MBR.fromMBRs(sorted.take(i).map(toMbr)).get
      val mbr2 = MBR.fromMBRs(sorted.drop(i).map(toMbr)).get
      //      println(mbr1.segments)
      //      println(mbr2.segments)
      val overlap = mbr1.intersect(mbr2).map(_.volume).getOrElse(0d) / Math.min(mbr1.volume, mbr2.volume)
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
//        println(s"Overlap: $minOverlap")
    if (minOverlap < config.maxOverlap) {
      Some((left, right))
    } else {
      println("Topological split failed")
      None
    }
  }
}

class XTreeLeaf(val elements: ArrayBuffer[Point], val config: XTreeConfig, var blocks: Int, var mbr: MBR) extends XTreeVertex {

  def split(): Option[(XTreeVertex, XTreeVertex)] = {
    //    println("Leaf")
    XTreeVertex.topologicalSplit(config, elements, point => MBR.single(point)) match {
      case Some((left, right)) =>
        Some(
          (
            new XTreeLeaf(left, config, left.size / config.maxChildren + 1, MBR.apply(left)),
            new XTreeLeaf(right, config, right.size / config.maxChildren + 1, MBR.apply(right))
          )
        )

      case None =>
        //        println("Leaf split unsuccessful")
        blocks = elements.size / config.maxChildren + 1
        None
    }
  }

  //  def mbr: MBR = MBR(elements)

  def insert(point: Point): Option[(XTreeVertex, XTreeVertex)] = {
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
    println(" " * offset + s"Leaf: ${elements.size} elements, $blocks blocks, ${mbr.segments}")
  }

  def find(rule: MBR): ArrayBuffer[Point] = {
    elements.flatMap(point =>
      Some(point).filter(rule.contains)
    )
  }
}

object XTreeLeaf {
  def fromPoint(point: Point, config: XTreeConfig) = new XTreeLeaf(ArrayBuffer(point), config, 1, MBR.single(point))
}

class XTreeNode(val children: ArrayBuffer[XTreeVertex], val config: XTreeConfig, var blocks: Int, var mbr: MBR) extends XTreeVertex {

  def minOverlapSplit: Option[(ArrayBuffer[XTreeVertex], ArrayBuffer[XTreeVertex])] = {
    val grouped = children.groupBy(_.isLeft)
    //    println(grouped)
    val maybeLeft = grouped.get(Some(true))
    val maybeRight = grouped.get(Some(false))
    (maybeLeft, maybeRight) match {
      case (Some(left), Some(right)) =>
        if (
          config.minChildren <= left.size && left.size <= config.maxChildren &&
            config.minChildren <= right.size && right.size <= config.maxChildren
        ) {
          Some(left, right)
        } else {
          println("Min overlap split failed")
          None
        }
      case _ =>
        println("Min overlap completely failed")
        None
    }
  }

  def split(): Option[(XTreeVertex, XTreeVertex)] = {
    //    println("Node")
    XTreeVertex.topologicalSplit[XTreeVertex](config, children, _.mbr).orElse(minOverlapSplit) match {
      case Some((left, right)) =>
        Some(
          (
            new XTreeNode(left, config, left.size / config.maxChildren + 1, MBR.fromMBRs(left.map(_.mbr)).get),
            new XTreeNode(right, config, right.size / config.maxChildren + 1, MBR.fromMBRs(right.map(_.mbr)).get)
          )
        )
      case None =>
        //        println("Node split unsuccessful")
        blocks = children.size / config.maxChildren + 1
        None
    }
  }

  def insert(point: Point): Option[(XTreeVertex, XTreeVertex)] = {
    var minEnlargment = children(0).mbr.plusPoint(point).volume - children(0).mbr.volume
    var mini = 0
    for (i <- children.indices.tail) {
      val newEnlargment = children(i).mbr.plusPoint(point).volume - children(i).mbr.volume
      if (newEnlargment < minEnlargment) {
        //        println("Really is contained")
        minEnlargment = newEnlargment
        mini = i
      }
    }
    //    println(s"Chosen: $mini")
    mbr = mbr.plusPoint(point)
    children(mini).insert(point).foreach {
      case (a, b) =>
        //        println(children.map(_.isLeft))
        if (children.size == 2) {
          children(0).isLeft = Some(true)
          children(1).isLeft = Some(false)
        }
        a.isLeft = children(mini).isLeft
        b.isLeft = children(mini).isLeft
        //        println(a.isLeft)
        //        println(b.isLeft)
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
//    val overlaps = (for {
//      a <- children
//      b <- children if a != b
//    } yield a.mbr.intersect(b.mbr).fold(0d)(_.volume)).filter(_ != 0)
//
//    val avgOverlap = overlaps.sum / overlaps.size

    println(" " * offset + s"Node: ${children.size} children, $blocks blocks, ${mbr.segments}")
    children.sortBy(_.mbr.segments.head._1).foreach(_.printMe(offset + 4))
  }

  def find(rule: MBR): ArrayBuffer[Point] = {
//    val matchingNodes = children.count(_.mbr.intersect(rule).nonEmpty)
//    println(s"Visited ${(matchingNodes.toDouble / children.size * 100).formatted("%3.0f")}%")
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


  final case class XTreeConfig(minChildren: Int, maxChildren: Int, dimensions: Int, maxOverlap: Double)

}