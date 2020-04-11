package domain

import fs2.kafka.{Deserializer, Serializer}
import domain.protobuf.models.{Point => ProtoPoint, Match => ProtoMatch}
import cats.effect.Sync

object serde {

  def pointSerializer[F[_] : Sync]: Serializer[F, Point] =
    Serializer.lift[F, Point] { point =>
      Sync[F].delay {
        point.toProto.toByteArray
      }
    }

  def pointDeserializer[F[_] : Sync]: Deserializer[F, Point] =
    Deserializer.lift[F, Point] { bytes =>
      Sync[F].delay {
        Point.fromProto(ProtoPoint.parseFrom(bytes))
      }
    }

  def matchSerializer[F[_] : Sync]: Serializer[F, Match] =
    Serializer.lift[F, Match] { mat =>
      Sync[F].delay {
        mat.toProto.toByteArray
      }
    }

  def matchDeserializer[F[_] : Sync]: Deserializer[F, Match] =
    Deserializer.lift[F, Match] { bytes =>
      Sync[F].delay {
        Match.fromProto(ProtoMatch.parseFrom(bytes))
      }
    }
}
