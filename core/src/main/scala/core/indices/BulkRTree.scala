package core.indices

import java.util.UUID

import cats.data.Chain
import core.domain.Point

trait BulkRTree {
  def mbr: MBR

  def findMBR(mbr: MBR): Chain[(MBR, UUID)]

  def findPoint(point: Point): Chain[(MBR, UUID)]

  def printMe(offset: Int = 0): Unit
}

object BulkRTree {

  final case class BulkRTreeLeaf(elements: Seq[(MBR, UUID)], mbr: MBR) extends BulkRTree {
    def printMe(offset: Int = 0): Unit =
      println(" " * offset + s"Leaf: ${elements.size} elements, $mbr")

    def findMBR(rule: MBR): Chain[(MBR, UUID)] =
      Chain.fromSeq(elements.filter(_._1.isContainedByMBR(rule)))

    def findPoint(point: Point): Chain[(MBR, UUID)] =
      Chain.fromSeq(elements.filter(_._1.containsPoint(point)))
  }

  final case class BulkRTreeNode(children: Seq[BulkRTree], mbr: MBR) extends BulkRTree {
    def printMe(offset: Int = 0): Unit = {
      println(" " * offset + s"Node: ${children.size} children, $mbr")
      children.foreach(_.printMe(offset + 4))
    }

    def findMBR(rule: MBR): Chain[(MBR, UUID)] = {
      children.foldLeft(Chain.empty[(MBR, UUID)])((acc, vertex) =>
        if (vertex.mbr.intersect(rule).nonEmpty) {
          acc.concat(vertex.findMBR(rule))
        } else {
          acc
        }
      )
    }

    def findPoint(point: Point): Chain[(MBR, UUID)] = {
      children.foldLeft(Chain.empty[(MBR, UUID)])((acc, vertex) =>
        if (vertex.mbr.containsPoint(point)) {
          acc.concat(vertex.findPoint(point))
        } else {
          acc
        }
      )
    }
  }

  def build(dimensions: Int, maxChildren: Int, mbrs: Seq[(MBR, UUID)]): BulkRTree = {
    val h = Math.ceil(Math.log10(mbrs.size) / Math.log10(maxChildren)).toInt
    val nSubtree = Math.pow(maxChildren, h - 1).toInt

    def go(dimension: Int, values: Seq[(MBR, UUID)], overrideSize: Option[Int] = None): BulkRTree = {
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
