package yams.util.encode

/** Tests for [[UriEncoder]]
  * 
  * @author Leejun Choi
  * @since 0.1
  */
class UriEncoderTest extends org.scalatest.FreeSpec {
  "encoding test" in {
    assertResult("Acad%C3%A9mico")(UriEncoder.encode("Académico"))
    assertResult("[]")(UriEncoder.encode("[]"))
  }

  "decoding test" in {
    val buffer = java.nio.ByteBuffer.allocate(10)
    buffer.put(0x34.toByte)
    buffer.put(0x35.toByte)
    buffer.flip
    assertResult("45")(UriEncoder.decode(buffer))
  }
}
