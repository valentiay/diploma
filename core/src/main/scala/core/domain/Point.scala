package core.domain

import core.domain.protobuf.models.{Point => ProtoPoint}

final case class Point(values: Array[Double]) extends AnyVal with Serializable {
  override def toString: String =
    values.map(_.formatted("%1.3f")).mkString("[",",","]")

  def toProto: ProtoPoint =
    ProtoPoint(values)
}

object Point {
  def fromProto(point: ProtoPoint): Point =
    Point(point.values.toArray)
}