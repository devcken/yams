package yams
package lexers

/** An empty line line consists of the non-content prefix followed by a line break.
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2778853]]
  */
trait EmptyLinesLexer extends scala.util.parsing.combinator.RegexParsers
                         with characters.LineBreak
                         with IndentationSpacesLexer
                         with LinePrefixesLexer {
  /** The semantics of empty lines depend on the scalar style they appear in. This is handled on
    * a case-by-case basis by the relevant productions.
    *
    * {{{
    *   [70]   l-empty(n,c) ::= ( s-line-prefix(n,c) | s-indent(<n) )
    *                           b-as-line-feed
    * }}}
    *
    * @param n a number of indentation spaces
    * @param c [[BlockOut]], [[BlockIn]], [[FlowOut]] or [[FlowIn]]
    * @return [[Parser]] for lexing '''l-empty(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#l-empty(n,c)]]
    */
  def emptyLine(n: Int, c: Context): Parser[String] = (linePrefix(n, c) | indentLt(n)) ~> breakAsLineFeed
}
