package yams
package lexers

/** Line folding allows long lines to be broken for readability, while retaining the semantics of
  * the original long line.
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2779048]]
  */
trait LineFoldingLexer extends scala.util.parsing.combinator.RegexParsers
                          with characters.LineBreak
                          with EmptyLinesLexer
                          with SeparationSpacesLexer {
  /** If a line break is followed by an empty line, it is __trimmed__; the first line break is discarded
    * and the rest are retained as content('''#x20''').
    * 
    * {{{
    *   [71] b-l-trimmed(n,c) ::= b-non-content l-empty(n,c)+
    * }}}
    *
    * @param n a number of indentation spaces
    * @param c [[BlockOut]], [[BlockIn]], [[FlowOut]] or [[FlowIn]]
    * @return [[Parser]] for lexing '''b-l-trimmed(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#b-l-trimmed(n,c)]]
    */
  private[lexers] def trimmed(n: Int, c: Context): Parser[String] = 
    s"[$NonContent]".r ~> emptyLine(n, c).+ ^^ { _.mkString }

  /** If the following line is not empty, the line break is converted to a single space('''#x20''').
    *
    * {{{
    *   [72] b-as-space ::= b-break
    * }}}
    *
    * @return [[Parser]] for lexing '''b-as-space'''
    * @see [[http://yaml.org/spec/1.2/spec.html#b-as-space]]
    */
  private[lexers] def asSpace: Parser[String] = s"[$Break]".r ^^ { _ => " " }

  /** A folded non-empty line may end with either of the above line breaks.
    *
    * {{{
    *   [73] b-l-folded(n,c) ::= b-l-trimmed(n,c) | b-as-space
    * }}}
    * 
    * The above rules are common to both the folded block style and the scalar flow styles.
    *
    * @param n a number of indentation spaces
    * @param c [[BlockOut]], [[BlockIn]], [[FlowOut]] or [[FlowIn]]
    * @return [[Parser]] for lexing '''b-l-folded(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#b-l-folded(n,c)]]
    */
  private[lexers] def folded(n: Int, c: Context): Parser[String] = trimmed(n, c) | asSpace

  /** Folding in flow styles provides more relaxed semantics. Flow styles typically depends on explicit 
    * indicators rather than indentation to convey structure. Hence spaces preceding or following the 
    * text in a line are a presentation detail and must not be used to convey information. Once all such 
    * spaces have been discarded, all line breaks are folded, without exception.
    * 
    * The combined effect of the ''flow line folding'' rules is that:
    * 
    * - Each "paragraph" is interpreted as a line,
    * - empty lines are interpreted as line feeds,
    * - text can be freely more-indented without affecting the content information.
    * 
    * {{{
    *   [74] s-flow-folded(n) ::= s-separate-in-line? b-l-folded(n,flow-in)
    *                             s-flow-line-prefix(n)
    * }}}
    *
    * @param n a number of indentation spaces
    * @return [[Parser]] for lexing '''s-flow-folded(n)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#s-flow-folded(n)]]
    */
  def flowFolded(n: Int): Parser[String] = 
    separateInLine.? ~> folded(n, FlowIn) <~ flowLinePrefix(n)
}
