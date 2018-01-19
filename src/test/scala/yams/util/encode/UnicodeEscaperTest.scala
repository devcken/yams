package yams.util.encode

/** Tests for [[UnicodeEscaper]]
  *
  * @author Leejun Choi
  * @since 0.1
  */
class UnicodeEscaperTest extends org.scalatest.FreeSpec {
  private val SmallestSurrogate = s"${Character.MIN_HIGH_SURROGATE}${Character.MIN_LOW_SURROGATE}"
  private val LargestSurrogate = s"${Character.MAX_HIGH_SURROGATE}${Character.MAX_LOW_SURROGATE}"
  private val TestString = s"\0abyz\u0080\u0100\u0800\u1000ABYZ\uffff${SmallestSurrogate}0189$LargestSurrogate"

  private def isLowerCase(codePoint: Int): Boolean = 'a' <= codePoint && codePoint <= 'z'
  private def isUpperCase(codePoint: Int): Boolean = 'A' <= codePoint && codePoint <= 'Z'
  private def isDigit(codePoint: Int): Boolean = '0' <= codePoint && codePoint <= '9'

  private val NopEscaper = new UnicodeEscaper {
    override protected[encode] def escape(codePoint: Int): Option[Array[Char]] = Option.empty
  }

  private val SimpleEscaper = new UnicodeEscaper {
    override protected[encode] def escape(codePoint: Int): Option[Array[Char]] = {
      if (isLowerCase(codePoint) || isUpperCase(codePoint) || isDigit(codePoint)) Option.empty
      else Some(s"[$codePoint]".toCharArray)
    }
  }

  "nop escaper" in {
    assert(TestString == NopEscaper.escape(TestString))
  }

  "simple escaper" in {
    val actual = SimpleEscaper.escape(TestString)
    val expected = s"[0]abyz[128][256][2048][4096]ABYZ[65535][${Character.MIN_SUPPLEMENTARY_CODE_POINT}]0189[${Character.MAX_CODE_POINT}]"

    assert(actual == expected)
  }

  "surrogate pairs" in {
    val min = Character.MIN_SUPPLEMENTARY_CODE_POINT
    val max = Character.MAX_CODE_POINT
    val range = max - min
    val s1 = min + range / 4
    val s2 = min + (2 * range) / 4
    val s3 = min + (3 * range) / 4
    val buffer = collection.mutable.ArrayBuffer.empty[Char]
    buffer += 'x'
    buffer ++= Character.toChars(min)
    buffer ++= Character.toChars(s1)
    buffer ++= Character.toChars(s2)
    buffer ++= Character.toChars(s3)
    buffer ++= Character.toChars(max)
    buffer += 'x'

    val actual = SimpleEscaper.escape(buffer.mkString)
    val expected = s"x[$min][$s1][$s2][$s3][$max]x"

    assert(actual == expected)
  }

  "trailing high surrogate" in {
    val TestTarget = s"abc${Character.MIN_HIGH_SURROGATE}"

    assertThrows[IllegalArgumentException](NopEscaper.escape(TestTarget))
    assertThrows[IllegalArgumentException](SimpleEscaper.escape(TestTarget))
  }

  "null input" in {
    assertThrows[NullPointerException](SimpleEscaper.escape(null))
  }

  "bad strings" in {
    val BadStrings = Array(
      String.valueOf(Character.MIN_LOW_SURROGATE),
      s"${Character.MIN_LOW_SURROGATE}xyz",
      s"abc${Character.MIN_LOW_SURROGATE}",
      s"abc${Character.MIN_LOW_SURROGATE}xyz",
      String.valueOf(Character.MAX_LOW_SURROGATE),
      s"${Character.MAX_LOW_SURROGATE}xyz",
      s"abc${Character.MAX_LOW_SURROGATE}",
      s"abc${Character.MAX_LOW_SURROGATE}xyz"
    )

    BadStrings.foreach(s => assertThrows[IllegalArgumentException](SimpleEscaper.escape(s)))
  }

  "false positives for nextEscapeIndex" in {
    val escaper = new UnicodeEscaper() {
      // Canonical escaper method that only escapes lower case ASCII letters.
      override protected[encode] def escape(codePoint: Int): Option[Array[Char]] =
        if (isLowerCase(codePoint)) Option(Array[Char](Character.toUpperCase(codePoint.toChar)))
        else Option.empty

      // Inefficient implementation that defines all letters as escapable.
      override protected def nextEscapeIndex(cs: CharSequence, index: Int, end: Int): Int = {
        if (index < end && !Character.isLetter(cs.charAt(index))) nextEscapeIndex(cs, index + 1, end)
        else index
      }
    }
    assert("\u0000HELLO \uD800\uDC00 WORLD!\n" == escaper.escape("\u0000HeLLo \uD800\uDC00 WorlD!\n"))
  }

  "index out of bounds when invoking codePointAt" in {
    assertThrows[IndexOutOfBoundsException](SimpleEscaper.codePointAt("Testing", 4, 2))
  }
}
