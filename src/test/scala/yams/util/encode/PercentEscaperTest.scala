package yams.util.encode

/** Tests for [[PercentEscaper]]
  * 
  * @author Leejun Choi
  * @since 0.1
  */
class PercentEscaperTest extends org.scalatest.FreeSpec {
  implicit val escaper: PercentEscaper = PercentEscaper("", plusForSpace = false)

  "simple escaper" in {
    (0.asInstanceOf[Char] until 128).foreach(c => {
      if ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
        assertNotEscaped(c)
      } else {
        assertEscaped(c, escapeAscii(c))
      }
    })

    assertEscaped('\u0000', "%00") // NUL
    assertEscaped('\u007f', "%7F") // DEL
    assertEscaped('\u0080', "%C2%80") // xx-00010,x-000000
    assertEscaped('\u07ff', "%DF%BF") // xx-11111,x-111111
    assertEscaped('\u0800', "%E0%A0%80") // xxx-0000,x-100000,x-00,0000
    assertEscaped('\uffff', "%EF%BF%BF") // xxx-1111,x-111111,x-11,1111
    assertUnicodeEscaped('\uD800', '\uDC00', "%F0%90%80%80")
    assertUnicodeEscaped('\uDBFF', '\uDFFF', "%F4%8F%BF%BF")

    assertEscaped("", "")
    assertEscaped("safestring", "safestring")
    assertEscaped("embedded\u0000null", "embedded%00null")
    assertEscaped("max\uffffchar", "max%EF%BF%BFchar")
  }

  "plus for space" in {
    val plusForSpaceEscaper = new PercentEscaper("", true)
    val spaceEscaper = new PercentEscaper(" ", false)

    assertEscaped("string with spaces", "string%20with%20spaces")
    assertEscaped("string with spaces", "string+with+spaces")(plusForSpaceEscaper)
    assertEscaped("string with spaces", "string with spaces")(spaceEscaper)
  }

  "custom escaper" in {
    val customEscaper = PercentEscaper("+*/-", plusForSpace = false)

    (0.asInstanceOf[Char] until 128).foreach(c => {
      if ((c >= '0' && c <= '9') ||
        (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        "+*/-".indexOf(c) >= 0)
        assertNotEscaped(c)(customEscaper)
      else
        assertEscaped(c, escapeAscii(c))
    })
  }

  "custom escaper with percent" in {
    val escaperWithPercent = PercentEscaper("%", plusForSpace = false)

    assertEscaped("foo|bar", "foo%7Cbar")(escaperWithPercent)
    assertEscaped("foo%7Cbar", "foo%7Cbar")(escaperWithPercent) // idempotent
  }

  "null is the bad argument" in {
    assertThrows[NullPointerException] { PercentEscaper(null, plusForSpace = false) }
  }

  "alphanumeric characters is the bad argument" in {
    assertThrows[IllegalArgumentException] { PercentEscaper("-+#abc.!", plusForSpace = false) }
  }

  "exceptions related to plus for space" in {
    try {
      PercentEscaper(" ", plusForSpace = false)
    } catch {
      case _: Throwable => fail("Space can be a 'safe' character if plusForSpace is false")
    }
    assertThrows[IllegalArgumentException] { PercentEscaper(" ", plusForSpace = true) }
  }

  private def assertEscaped(s: String, expected: String)(implicit escaper: UnicodeEscaper) =
    assertResult(expected)(escaper.escape(s))

  import org.scalatest.Failed

  private def assertEscaped(codePoint: Int, expected: String)(implicit escaper: UnicodeEscaper) =
    escapeCodePoint(codePoint, escaper) match {
      case Some(x) => assertResult(expected)(x)
      case _ => Failed("")
    }
  private def assertUnicodeEscaped(hi: Char, lo: Char, expected: String)(implicit escaper: UnicodeEscaper) =
    escapeCodePoint(Character.toCodePoint(hi, lo), escaper) match {
      case Some(x) => assertResult(expected)(x)
      case _ => Failed("")
    }
  
  private def assertNotEscaped(codePoint: Int)(implicit escaper: UnicodeEscaper) =
    assert(escapeCodePoint(codePoint, escaper).isEmpty)

  private def escapeCodePoint(codePoint: Int, escaper: UnicodeEscaper): Option[String] =
    escaper.escape(codePoint) match {
      case Some(x) => Some(new String(x))
      case _ => Option.empty
    }

  private def escapeAscii(codePoint: Int): String = {
    if (codePoint >= 128) throw new IllegalArgumentException
    val hex = "0123456789ABCDEF"
    s"%${hex.charAt((codePoint >> 4) & 0xF)}${hex.charAt(codePoint & 0xF)}"
  }
}
