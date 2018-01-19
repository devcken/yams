package yams.util.encode

/** A companion object of [[PercentEscaper]]. 
  *
  * @author Leejun Choi
  * @since 0.1 
  */
object PercentEscaper {
  def apply(safeChars: String, plusForSpace: Boolean): PercentEscaper =
    new PercentEscaper(safeChars, plusForSpace)
}

/** A [[UnicodeEscaper]] that escapes some set of Java characters using a UTF-8 based percent encoding scheme.
  *
  * For performance reason, only UTF-8 is supported currently.
  *
  * @param safeChars a string for additional safe characters(DO NOT provide alphanumeric characters)
  * @param plusForSpace true if SPACE character should be escaped to `+` rather than `%20`
  */
protected[encode] class PercentEscaper(safeChars: String = "", plusForSpace: Boolean) extends UnicodeEscaper {
  /**
    * In [[PercentEscaper.escape(Int):Option[Array[Char]]* escape], this is returned 
    * if a given `codePoint` is SPACE character and `plusForSpace` is `true`.
    */
  private val PlusSign: Option[Array[Char]] = Some(Array('+'))

  /**
    * Characters for the upper-case hex digits. Percent escaper escapes code point using them.
    */
  private val UpperHexDigits = 
    Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')

  /**
    * Regular expression for finding out if some safe characters in a given `safeChars`.
    */
  private val DefaultSafeCharsRegex = ".*[0-9A-Za-z].*"

  if (safeChars.matches(DefaultSafeCharsRegex))
    throw new IllegalArgumentException("Alphanumeric characters are always 'safe'" +
      " thus they should not be explicitly specified.")

  if (safeChars.contains(" ") && plusForSpace)
    throw new IllegalArgumentException("If the space is specified in safe characters," +
      " plusForSpace cannot be true.")

  /**
    * An array of flags for finding out if a particular character should be remained 
    * unmodified in the output.
    */
  private val skipFlags: Array[Boolean] = buildSafeFlags((safeChars +
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789").toCharArray)

  /** Returns a boolean array with entries which set to true at the index equalled to safe character.
    *
    * @param safeChars safe characters to be skipped while escaping.
    * @return boolean flags to be used for skipping if entry corresponding to a particular character is true.
    */
  private def buildSafeFlags(safeChars: Array[Char]): Array[Boolean] =
    (0 to safeChars.max).map(safeChars.contains).toArray

  /** Escapes a given Unicode code point in UTF-8.
    *
    * @param codePoint the Unicode code point to be escaped
    * @return [[Option]] of the supplementary characters, or [[None]] if no escaped
    */
  override protected[encode] def escape(codePoint: Int): Option[Array[Char]] = {
    if (codePoint < skipFlags.length && skipFlags(codePoint)) Option.empty
    else if (codePoint == ' ' && plusForSpace) PlusSign
    else if (codePoint <= 0x7F) 
      Some(Array('%', UpperHexDigits(codePoint >>> 4), UpperHexDigits(codePoint & 0xF)))
    else if (codePoint <= 0x7FF) Some {
      val c5 = UpperHexDigits(codePoint & 0xF)
      val codePoint5 = codePoint >>> 4
      val c4 = UpperHexDigits(0x8 | (codePoint5 & 0x3))
      val codePoint4 = codePoint5 >>> 2
      val c2 = UpperHexDigits(codePoint4 & 0xF)
      val codePoint2 = codePoint4 >>> 4
      val c1 = UpperHexDigits(0xC | codePoint2)
      Array('%', c1, c2, '%', c4, c5)
    } else if (codePoint <= 0xFFFF) Some {
      val c8 = UpperHexDigits(codePoint & 0xF)
      val codePoint8 = codePoint >>> 4
      val c7 = UpperHexDigits(0x8 | (codePoint8 & 0x3))
      val codePoint7 = codePoint8 >>> 2
      val c5 = UpperHexDigits(codePoint7 & 0xF)
      val codePoint5 = codePoint7 >>> 4
      val c4 = UpperHexDigits(0x8 | (codePoint5 & 0x3))
      val codePoint4 = codePoint5 >>> 2
      val c2 = UpperHexDigits(codePoint4)
      Array('%', 'E', c2, '%', c4, c5, '%', c7, c8)
    } else if (codePoint <= 0x10FFFF) Some {
      val c11 = UpperHexDigits(codePoint & 0xF)
      val codePoint11 = codePoint >>> 4
      val c10 = UpperHexDigits(0x8 | (codePoint11 & 0x3))
      val codePoint10 = codePoint11 >>> 2
      val c8 = UpperHexDigits(codePoint10 & 0xF)
      val codePoint8 = codePoint10 >>> 4
      val c7 = UpperHexDigits(0x8 | (codePoint8 & 0x3))
      val codePoint7 = codePoint8 >>> 2
      val c5 = UpperHexDigits(codePoint7 & 0xF)
      val codePoint5 = codePoint7 >>> 4
      val c4 = UpperHexDigits(0x8 | (codePoint5 & 0x3))
      val codePoint4 = codePoint5 >>> 2
      val c2 = UpperHexDigits(codePoint4 & 0x7)
      Array('%', 'F', c2, '%', c4, c5, '%', c7, c8, '%', c10, c11)
    }
    else throw new IllegalArgumentException(s"Invalid unicode character value $codePoint")
  }

  /**
    * @inheritdoc
    *
    * Overridden for performance.
    */
  override def escape(s: String): String = {
    (0 until s.length).find(n => {
      val c = s.charAt(n)
      c >= skipFlags.length || !skipFlags(c)
    }) match {
      case Some(x) => escapeSlow(s, x)
      case _ => s
    }
  }

  /**
    * @inheritdoc
    *
    * Overridden for performance.
    */
  override protected def nextEscapeIndex(cs: CharSequence, start: Int, end: Int): Int = {
    (start until end).find(n => {
      val c = cs.charAt(n)
      c >= skipFlags.length || !skipFlags(c)
    }) match {
      case Some(x) => x
      case _ => start
    }
  }
}
