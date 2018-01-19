package yams.characters

/** The YAML syntax productions make use of the following additional character classes.
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2775468]]
  */
trait MiscChar {
  /** A decimal digit for numbers
    *
    * {{{
    *   [35] ns-dec-digit ::= [#x30-#x39] /* 0-9 */
    * }}}
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-dec-digit]]
    */
  final val DecimalDigit = "\\x30-\\x39"

  /** A hexadecimal digit for escape sequences
    *
    * {{{
    *   [36] ns-hex-digit ::=  ns-dec-digit
    *                        | [#x41-#x46] /* A-F */ | [#x61-#x66] /* a-f */
    * }}}
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-hex-digit]]
    */
  final val HexDigit = s"$DecimalDigit\\x41-\\x46\\x61-\\x66"

  /** ASCII letter (alphabetic) characters
    *
    * {{{
    *   [37] ns-ascii-letter ::= [#x41-#x5A] /* A-Z */ | [#x61-#x7A] /* a-z */
    * }}}
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-ascii-letter]]
    */
  final val AsciiLetter = "\\x41-\\x5A\\x61-\\x7A"

  /** Word (alphanumeric) characters for identifiers
    *
    * {{{
    *   [38] ns-word-char ::= ns-dec-digit | ns-ascii-letter | “-”
    * }}}
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-word-char]]
    */
  final val WordChar = s"$DecimalDigit$AsciiLetter\\x2D"

  /** URI characters for tags, as specified in [[http://www.ietf.org/rfc/rfc2396.txt RFC2396]], with
    * the addition of the “```[```” and “```]```” for presenting IPv6 addresses as proposed in
    * [[http://www.ietf.org/rfc/rfc2732.txt RFC2732]].
    *
    * By convention, any URI characters other than the allowed printable ASCII characters are first
    * encoded in UTF-8, and then each byte is escaped using the “```%```” character. The YAML processor
    * must not expand such escaped characters. Tag characters must be preserved and compared exactly as
    * presented in the YAML stream, without any processing.
    *
    * {{{
    *   [39] ns-uri-char ::=  “%” ns-hex-digit ns-hex-digit | ns-word-char | “#”
    *                       | “;” | “/” | “?” | “:” | “@” | “&” | “=” | “+” | “$” | “,”
    *                       | “_” | “.” | “!” | “~” | “*” | “'” | “(” | “)” | “[” | “]”
    * }}}
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-uri-char]]
    */
  final val UriChar = s"%$HexDigit$WordChar\\x21" +
    "\\x23\\x24\\x26-\\x2C\\x2E\\x2F" +
    "\\x3A\\x3B\\x3D\\x3F\\x40\\x5B\\x5D\\x5F\\x7E"

  /** The “```!```” character is used to indicate the end of a named tag handle; hence its use in tag
    * shorthands is restricted. In addition, such shorthands must not contain the “```[```”, “```]```”,
    * “```{```”, “```}```” and “```,```” characters. These characters would cause ambiguity with flow
    * collection structures.
    *
    * {{{
    *   [40] ns-tag-char ::= ns-uri-char - “!” - c-flow-indicator
    * }}}
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-tag-char]]
    */
  final val TagChar = s"%$HexDigit$WordChar" +
    "\\x23\\x24\\x26-\\x2B\\x2E\\x2F" +
    "\\x3A\\x3B\\x3D\\x3F\\x40\\x5F\\x7E"
}
