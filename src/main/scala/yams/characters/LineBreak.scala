package yams.characters

import util.parsing.combinator.RegexParsers

/** YAML recognizes the following ASCII line break characters.
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2774608]]
  */
trait LineBreak extends RegexParsers {
  /** YAML recognizes the line feed character.
    *
    * {{{
    *   [24] b-line-feed ::= #xA
    * }}}
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#b-line-feed]]
    */
  final val LineFeed = "\\x0A"

  /** YAML recognizes the carriage return character.
    *
    * {{{
    *   [25] b-carriage-return ::= #xD
    * }}}
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#b-carriage-return]]
    */
  final val CarriageReturn = "\\x0D"

  /** YAML recognizes the line break characters.
    *
    * {{{
    *   [26] b-char ::= b-line-feed | b-carriage-return
    * }}}
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#]]
    */
  final val LineBreakChar = s"$LineFeed$CarriageReturn"

  /** All other characters, including the form feed ('''#x0C'''), are considered to be non-break
    * characters. Note that these include the non-ASCII line breaks:
    *
    * - next line ('''#x85''')
    * - line separator ('''#x2028''')
    * - paragraph separator ('''#x2029''')
    *
    * YAML version 1.1 did support the above non-ASCII line break characters; however, JSON does not.
    * Hence, to ensure JSON compatibility, YAML treats them as non-break characters as of version 1.2.
    * In theory this would cause incompatibility with version 1.1; in practice these characters were
    * rarely (if ever) used. YAML 1.2 processors parsing a version 1.1 document should therefore treat
    * these line breaks as non-break characters, with an appropriate warning.
    *
    * {{{
    *   [27] nb-char ::= c-printable - b-char - c-byte-order-mark
    * }}}
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#nb-char]]
    */
  final val NoBreakChar = "[\\x09\\x20-\\x7E\\x85\\xA0-\\x{D7FF}\\x{E000}-\\x{FEFE}\\x{FF00}-\\x{FFFD}\\x{10000}-\\x{10FFFF}]"

  /** Line breaks are interpreted differently by different systems, and have several widely used formats.
    *
    * {{{
    *   [28] b-break ::=  (b-carriage-return b-line-feed) /* DOS, Windows */
    *                    | b-carriage-return              /* MacOS upto 9.x */
    *                    | b-line-feed                    /* UNIX, MacOS X */
    * }}}
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#b-break]]
    */
  final val Break = s"($CarriageReturn$LineFeed)$CarriageReturn$LineFeed"

  /** Line breaks inside scalar content must be normalized by the YAML processor. Each such line break
    * must be parsed into a single line feed character.
    *
    * {{{
    *   [29] b-as-line-feed ::= b-break
    * }}}
    *
    * @return [[Parser]] for lexing '''b-as-line-feed'''
    * @see [[http://yaml.org/spec/1.2/spec.html#b-as-line-feed]]
    */
  final def breakAsLineFeed: Parser[String] = s"[$Break]".r ^^ { _ => "\\n" }

  /** Outside scalar content, YAML allow any line break to be used to terminate lines.
    *
    * {{{
    *   [30] b-non-content ::= b-break
    * }}}
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#b-non-content]]
    */
  final val NonContent = Break
}
