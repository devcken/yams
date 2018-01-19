package yams
package lexers

/** Inside scalar content, each line begins with a non-content line prefix. This prefix always includes
  * the indentation.
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2778481]]
  */
trait LinePrefixesLexer extends scala.util.parsing.combinator.RegexParsers
                           with IndentationSpacesLexer
                           with SeparationSpacesLexer {
  /** Line prefixes is treated differently according to the context.
    *
    * {{{
    *   [67]     s-line-prefix(n,c) ::= c = block-out => s-block-line-prefix(n)
    *                                   c = block-in => s-block-line-prefix(n)
    *                                   c = flow-out => s-flow-line-prefix(n)
    *                                   c = flow-int => s-flow-line-prefix(n)
    * }}}
    *
    * @param n a number of indentation spaces
    * @param c [[BlockIn]], [[BlockOut]], [[FlowIn]] or [[FlowOut]]
    * @return [[Parser]] for lexing '''s-line-prefix(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#s-line-prefix(n,c)]]
    */
  def linePrefix(n: Int, c: Context): Parser[Int] = c match {
    case BlockIn | BlockOut => blockLinePrefix(n)
    case FlowIn | FlowOut => flowLinePrefix(n)
    case _ => throw new Exception() // TODO definition of the exception
  }

  /** In YAML block styles, prefixed spaces is content except for indentation spaces.
    *
    * {{{
    *   [68] s-block-line-prefix(n) ::= s-indent(n)
    * }}}
    *
    * @param n a number of indentation spaces
    * @return [[Parser]] for lexing '''s-block-line-prefix(n)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#s-line-prefix(n,c)]]
    */
  def blockLinePrefix(n: Int): Parser[Int] = indent(n)

  /** For flow scalar styles it additionally includes all leading white space, which may
    * contain tab characters.
    *
    * {{{
    *   [69] s-flow-line-prefix(n) ::= s-indent(n) s-separate-in-line?
    * }}}
    *
    * @param n a number of indentation spaces
    * @return [[Parser]] for lexing '''s-flow-line-prefix(n)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#s-flow-line-prefix(n)]]
    */
  def flowLinePrefix(n: Int): Parser[Int] = indent(n) <~ separateInLine.?
}
