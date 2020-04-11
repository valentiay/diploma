package core.indices

import java.util.UUID

import core.domain.Point

trait BulkRTreeVertex {
  def mbr: MBR

  def findMBR(mbr: MBR): Seq[(MBR, UUID)]

  def findPoint(point: Point): Seq[(MBR, UUID)]

  def printMe(offset: Int = 0): Unit
}

object BulkRTreeVertex {

  final case class BulkRTreeLeaf(elements: Seq[(MBR, UUID)], mbr: MBR) extends BulkRTreeVertex {
    def printMe(offset: Int = 0): Unit =
      println(" " * offset + s"Leaf: ${elements.size} elements, $mbr")

    def findMBR(rule: MBR): Seq[(MBR, UUID)] =
      elements.filter(_._1.isContainedByMBR(rule))

    def findPoint(point: Point): Seq[(MBR, UUID)] =
      elements.filter(_._1.containsPoint(point))
  }

  final case class BulkRTreeNode(children: Seq[BulkRTreeVertex], mbr: MBR) extends BulkRTreeVertex {
    def printMe(offset: Int = 0): Unit = {
      println(" " * offset + s"Node: ${children.size} children, $mbr")
      children.sortBy(_.mbr.starts.head).foreach(_.printMe(offset + 4))
    }

    def findMBR(rule: MBR): Seq[(MBR, UUID)] = {
      children.flatMap(vertex =>
        if (vertex.mbr.intersect(rule).nonEmpty) {
          vertex.findMBR(rule)
        } else {
          Seq()
        }
      )
    }

    def findPoint(point: Point): Seq[(MBR, UUID)] = {
      children.flatMap(vertex =>
        if (vertex.mbr.containsPoint(point)) {
          vertex.findPoint(point)
        } else {
          Seq()
        }
      )
    }
  }

  def build(dimensions: Int, maxChildren: Int, mbrs: Seq[(MBR, UUID)]): BulkRTreeVertex = {
    val n = mbrs.size
    val h = Math.ceil(Math.log10(mbrs.size) / Math.log10(maxChildren)).toInt
    val s = (Math.pow(maxChildren, h) - mbrs.size).toInt
    val nSubtree = Math.pow(maxChildren, h - 1).toInt

    def go(dimension: Int, values: Seq[(MBR, UUID)], overrideSize: Option[Int] = None): BulkRTreeVertex = {
      if (values.size <= maxChildren) {
        BulkRTreeLeaf(values, MBR.fromMBRsUnsafe(values.map(_._1)))
      } else {
        val children =
          values.sortBy(_._1.starts(dimension)).grouped(overrideSize.getOrElse(values.size / maxChildren)).map { slice =>
            go((dimension + 1) % dimensions, slice)
          }.toSeq
        BulkRTreeNode(children, MBR.fromMBRsUnsafe(children.map(_.mbr)))
      }
    }

    go(0, mbrs, Some(nSubtree))
  }
}
