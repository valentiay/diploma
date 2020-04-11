package domain

import cats.syntax.option._
import domain.protobuf.models.{Match => ProtoMatch}
import java.util.UUID

final case class Match(point: Point, ruleId: UUID) {
  def toProto: ProtoMatch =
    ProtoMatch(point.toProto.some, uuidAsByteString(ruleId))
}

object Match {
  def fromProto(mat: ProtoMatch): Match =
    Match(Point.fromProto(mat.point.get), byteStringAsUuid(mat.ruleId))
}