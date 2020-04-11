import java.util.UUID

import com.google.protobuf.ByteString
import java.nio.ByteBuffer

package object domain {
  type Rule = Vector[Restriction]

  def convertBytesToHex(bytes: Seq[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append(String.format("%02x", Byte.box(b)))
    }
    sb.toString
  }

  def byteStringAsUuid(byteString: ByteString): UUID = {
    val bb = ByteBuffer.wrap(byteString.toByteArray)
    val firstLong = bb.getLong
    val secondLong = bb.getLong
    new UUID(firstLong, secondLong)
  }

  def uuidAsByteString(uuid: UUID): ByteString = {
    val bb = ByteBuffer.wrap(new Array[Byte](16))
    bb.putLong(uuid.getMostSignificantBits)
    bb.putLong(uuid.getLeastSignificantBits)
    ByteString.copyFrom(bb.array())
  }
}
