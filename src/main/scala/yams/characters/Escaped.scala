package yams
package characters

/** All non-printable characters must be escaped. YAML escape sequences use the “\” notation common 
  * to most modern computer languages. Each escape sequence must be parsed into the appropriate Unicode 
  * character.
  * 
  * Note that escape sequences are only interpreted in double-quoted scalars. In all other scalar styles, 
  * the “\” character has no special meaning and non-printable characters are not available.
  * 
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2776092]]
  */
trait Escaped extends scala.util.parsing.combinator.RegexParsers {
  override def skipWhitespace: Boolean = false

  /** YAML escape sequences are a superset of C’s escape sequences:
    * 
    * {{{
    *   [42]                ns-esc-null ::= “0”
    *   [43]                ns-esc-bell ::= “a”
    *   [44]           ns-esc-backspace ::= “b”
    *   [45]      ns-esc-horizontal-tab ::= “t” | #x9
    *   [46]           ns-esc-line-feed ::= “n”
    *   [47]        ns-esc-vertical-tab ::= “v”
    *   [48]           ns-esc-form-feed ::= “f”
    *   [49]     ns-esc-carriage-return ::= “r”
    *   [50]              ns-esc-escape ::= “e”
    *   [51]               ns-esc-space ::= #x20
    *   [52]        ns-esc-double-quote ::= “"”
    *   [53]               ns-esc-slash ::= “/”
    *   [54]           ns-esc-backslash ::= “\”
    *   [55]           ns-esc-next-line ::= “N”
    *   [56]  ns-esc-non-breaking-space ::= “_”
    *   [57]      ns-esc-line-separator ::= “L”
    *   [58] ns-esc-paragraph-separator ::= “P”
    * }}}
    * 
    * @see [[http://yaml.org/spec/1.2/spec.html#id2776092]]
    */
  private val UnescapingReplacements = Map(
    '0' -> "\u0000",    // ASCII null (#x0)
    'a' -> "\u0007",    // ASCII bell (#x7)
    'b' -> "\u0008",    // ASCII backspace (#x8)
    't' -> "\u0009",    // ASCII horizontal tab (#x9)
    'n' -> "\n",        // ASCII line feed (#xA)
    'v' -> "\u000B",    // ASCII vertical tab (#xB)
    'f' -> "\u000C",    // ASCII form feed (#xC)
    'r' -> "\r",        // ASCII carriage return (#xD)
    'e' -> "\u001B",    // ASCII escape (#x1B)
    ' ' -> "\u0020",    // ASCII space (#x20)
    '\"' -> "\"",       // ASCII double quote (#x22)
    '\\' -> "\\",       // ASCII back slash (#x2F)
    'N' -> "\u0085",    // Unicode next line (#x85)
    '_' -> "\u00A0",    // Unicode non-breaking space (#xA0)
    'L' -> "\u2028",    // Unicode line separator (#x2028)
    'P' -> "\u2029"     // Unicode paragraph separator (#x2029)
  )
  
  /** The map which map escape sequences to length of Unicode code points.
    * 
    * {{{
    *   [59]  ns-esc-8-bit ::= “x”
    *                          ( ns-hex-digit × 2 )
    *   [60] ns-esc-16-bit ::= “u”
    *                          ( ns-hex-digit × 4 )
    *   [61] ns-esc-32-bit ::= “U”
    *                          ( ns-hex-digit × 8 )
    * }}}
    * 
    * This lengths is used to read Unicode code points for escape sequences. For `\\x`, indicates that the
    * escape sequence is 8-bit Unicode followed by a 2-digit hexadecimal number. For `\\u`, indicates that the
    * escape sequence is 16-bit Unicode followed by a 4-digit hexadecimal number. Finally, For `\\U`, indicates that the
    * escape sequence is 32-bit Unicode followed by a 8-digit hexadecimal number. 
    * 
    * For example, for `\\x41`, it is 8-bit Unicode and Unicode code points is 41.
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-esc-8-bit]] 
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-esc-16-bit]]
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-esc-32-bit]]
    */
  private val UnescapingUnicodeLength = Map(
    'x' -> 2,           // 8-bit Unicode
    'u' -> 4,           // 16-bit Unicode
    'U' -> 8            // 32-bit Unicode
  )

  /** Any escaped character
    * 
    * {{{
    *   [62] c-ns-esc-char ::= “\”
    *                          ( ns-esc-null | ns-esc-bell | ns-esc-backspace
    *                          | ns-esc-horizontal-tab | ns-esc-line-feed
    *                          | ns-esc-vertical-tab | ns-esc-form-feed
    *                          | ns-esc-carriage-return | ns-esc-escape | ns-esc-space
    *                          | ns-esc-double-quote | ns-esc-slash | ns-esc-backslash
    *                          | ns-esc-next-line | ns-esc-non-breaking-space
    *                          | ns-esc-line-separator | ns-esc-paragraph-separator
    *                          | ns-esc-8-bit | ns-esc-16-bit | ns-esc-32-bit )
    * }}}
    * 
    * @return [[Parser]] for lexing '''c-ns-esc-char'''
    * @see [[http://yaml.org/spec/1.2/spec.html#c-ns-esc-char]]
    */
  private[yams] def escapedCharacter: Parser[String] = Parser { input =>
    val source = input.source
    val subSequence = source.subSequence(input.offset, source.length).toString
    
    def isEscapedCharacter(c: Char): Boolean =
      !Character.isSupplementaryCodePoint(c) && UnescapingReplacements.contains(c)
    
    def isEscapedUnicode(c: Char): Boolean =
      !Character.isSupplementaryCodePoint(c) && UnescapingUnicodeLength.contains(c)

    "\\\\(?<escaped>0|a|b|t|n|v|f|r|e| |\"|\\/|\\\\|N|_|L|P|x|u|U)".r findPrefixMatchOf subSequence match {
      case Some(m) if isEscapedCharacter(m.group("escaped").head) =>
        Success(UnescapingReplacements(m.group("escaped").head), input.drop(m.end))
      case Some(m) if isEscapedUnicode(m.group("escaped").head) =>
        val in = input.drop(m.end)
        val length = UnescapingUnicodeLength(m.group("escaped").head)
        val hex = in.source.subSequence(in.offset, in.offset + length)
        if ("[^0-9a-zA-Z]".r.find(hex))
          Error(s"expected a $length-length hex code for \\${m.group("escaped").head}, but found $hex.", input)
        else 
          Success(new String(Character.toChars(hex.toInt(16))), in.drop(length))
      case _ => Failure("", input)
    }
  }
}
