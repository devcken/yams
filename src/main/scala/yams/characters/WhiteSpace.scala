package yams.characters

/** YAML recognizes two white space characters: space and tab.
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2775170]]
  */
trait WhiteSpace {
  /** YAML recognizes the space character('''#x20''').
    *
    * {{{
    *   [31] s-space ::= #x20
    * }}}
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#s-space]]
    */
  final val Space = "\\x20"

  /** YAML recognizes the tab character('''#x09''').
    *
    * {{{
    *   [32] s-tab ::= #x9
    * }}}
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#s-tab]]
    */
  final val Tab = "\\x09"

  /** YAML recognizes the whitespace characters('''#x20''' and '''#x09''').
    *
    * {{{
    *   [33] s-white ::= s-space | s-tab
    * }}}
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#s-white]]
    */
  final val White = s"$Space$Tab"

  /** The rest of the (printable) non-break characters are considered to be non-space characters.
    *
    * {{{
    *   [34] ns-char ::= nb-char - s-white
    * }}}
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-char]]
    */
  final val NoSpaceChar = "\\x09\\x21-\\x7E\\x85\\xA0-\\x{D7FF}\\x{E000}-\\x{FEFE}\\x{FF00}-\\x{FFFD}\\x{10000}-\\x{10FFFF}"
}
