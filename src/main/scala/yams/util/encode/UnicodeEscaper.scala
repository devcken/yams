package yams.util.encode

/** An escaper that Converts literal text into a format safe for inclusion in some particular 
  * context(such as an XML document).
  *
  * Extend this class and implement method.
  *
  * @note This class is inspired by
  *       [[https://github.com/google/guava/blob/master/guava/src/com/google/common/escape/UnicodeEscaper.java
  *       com.google.common.escape.UnicodeEscaper]] of Guava. All things from this class follows 
  *       the license for Guava.
  *
  * @author Leejun Choi
  * @since 0.1
  */
trait UnicodeEscaper {
  import collection.mutable.ArrayBuffer
  
  private val DynamicVariable = new util.DynamicVariable[ArrayBuffer[Char]](ArrayBuffer.empty)

  /** Returns [[Option]] of result that encode a given `codePoint`.
    *
    * For the class that extend this trait, guarantee for a given `codePoint` to be
    * in the range `0 <= codePoint <= Character.MAX_CODE_POINT`.
    *
    * If escaping is unnecessary, returns [[None]]. This allows the escaping algorithm to be 
    * performed effectively.
    *
    * Implement to either throw an appropriate runtime exception or return a suitable supplementary 
    * character if a particular code point cannot be handled.
    *
    * @param codePoint the Unicode code point to be escaped
    * @return [[Option]] of the supplementary characters, or [[None]] if no escaped
    */
  protected[encode] def escape(codePoint: Int): Option[Array[Char]]

  /** Returns the escaping result of a given string.
    *
    * @note When escaping some input in arbitrary successive chunks, then it is not generally safe to 
    *       use this method. If an input string ends with an high surrogate character, then this method 
    *       will throw [[IllegalArgumentException]].
    *       Ensure the input is valid [[http://en.wikipedia.org/wiki/UTF-16 UTF-16]] before calling 
    *       this method.
    * @note When implementing own escaper it is good idea to override this method for efficiency by 
    *       inlining the implementation of [[nextEscapeIndex]] directly.
    *
    * @param s the literal string to be escaped
    * @return the escaping result of a given string
    */
  def escape(s: String): String = {
    val end: Int = s.length
    val index: Int = nextEscapeIndex(s, 0, end)
    if (index == end) s else escapeSlow(s, index)
  }

  /** Scans a sub-sequence of characters from a given [[java.lang.CharSequence]], and returns the index 
    * of the next character that requires escaping.
    *
    * @param cs the sequence of characters to be scanned
    * @param offset the offset of the first character to be scanned
    * @param end the opened position for the last character to be scanned
    * @return the index of the next character that requires escaping
    */
  protected def nextEscapeIndex(cs: CharSequence, offset: Int, end: Int): Int = {
    if (offset >= end) offset
    else {
      val codePoint: Int = this.codePointAt(cs, offset, end)
      if (codePoint < 0 || this.escape(codePoint).isDefined) offset
      else nextEscapeIndex(cs, offset + Character.charCount(codePoint), end)
    }
  }

  /** Returns the result of escaping for a given string, starting from a given index.
    *
    * This method is called by [[UnicodeEscaper.escape(s:String):String* escape]] when it find the next 
    * escaping index.
    *
    * @note This method is not reentrant. It is intended to be invoked by the top level method, 
    *       i.e. [[escape]].
    *
    * @param s the literal string to be escaped
    * @param index the starting position to escape
    * @throws IllegalArgumentException if invalid surrogate characters are encountered,
    *                                  i.e. the particular code point is less than 0
    * @return the escaping result of a given string
    */
  protected final def escapeSlow(s: String, index: Int): String = {
    val buffer = DynamicVariable.value
    val end = s.length

    def fill(s: String, index: Int, end: Int, unescapedChunkStart: Int = 0): Int = {
      if (index == end) unescapedChunkStart
      else {
        val codePoint: Int = this.codePointAt(s, index, end)
        if (codePoint < 0) throw new IllegalArgumentException("Trailing high surrogate at end of input")

        val nextIndex = index + Character.charCount(codePoint)
        escape(codePoint) match {
          case Some(x) =>
            //val skippedLength = index - unescapedChunkStart
            if (index - unescapedChunkStart > 0) buffer ++= s.slice(unescapedChunkStart, index).toCharArray
            if (x.length > 0) buffer ++= x
            fill(s, nextEscapeIndex(s, nextIndex, end), end, nextIndex)
          case _ => fill(s, nextEscapeIndex(s, nextIndex, end), end, unescapedChunkStart)
        }
      }
    }

    val unescapedChunkStart = fill(s, index, end)
    val skippedLength = end - unescapedChunkStart
    if (skippedLength > 0) buffer ++= s.slice(unescapedChunkStart, end).toCharArray

    val escaped = buffer.mkString
    buffer.clear
    escaped
  }

  /** Returns the Unicode code point of the character at the given index.
    *
    * @param cs the sequence of characters from which to decode the code point
    * @param offset the offset of the first character to decode
    * @param end the opened position for the last character to decode
    * @throws IndexOutOfBoundsException if a given `offset` is greater than `end`
    * @throws IllegalArgumentException if the ''low surrogate'' is preceded, 
    *                                        or if the ''high surrogate'' isn't followed by the ''low''
    * @return the Unicode code point for a given `offset`, or the negated value of high surrogate 
    *         character if a next index is reached at a given `end`
    */
  protected[encode] def codePointAt(cs: CharSequence, offset: Int, end: Int): Int = {
    if (offset >= end) throw new IndexOutOfBoundsException("Index exceeds specified range")

    val c1: Char = cs.charAt(offset)

    if (!c1.isSurrogate) c1
    else if (c1.isLowSurrogate)
      throw new IllegalArgumentException(s"Unexpected low surrogate character '$c1' at index $offset in $cs")
    else {
      val nextIndex = offset + 1

      if (nextIndex == end) -c1
      else {
        val c2: Char = cs.charAt(nextIndex)

        if (c2.isLowSurrogate) Character.toCodePoint(c1, c2)
        else throw new IllegalArgumentException(s"Expected low surrogate but got char '$c2' at index $nextIndex in $cs")
      }
    }
  }
}
