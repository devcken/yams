package yams
package lexers

/** In YAML block styles, structure is determined by indentation.
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2777534]]
  */
trait IndentationSpacesLexer extends scala.util.parsing.combinator.RegexParsers
                                with characters.WhiteSpace {
  override def skipWhitespace: Boolean = false

  /** In general, indentation is defined as a zero or more space characters at the start of a line.
    *
    * To maintain portability, tab characters must not be used in indentation, since different systems
    * treat tabs differently. Note that most modern editors may be configured so that pressing the tab
    * key results in the insertion of an appropriate number of spaces.
    *
    * {{{
    *   [63] s-indent(n) ::= s-space × n
    * }}}
    *
    * @param n a number of indentation spaces
    * @return [[Parser]] for lexing '''s-indent(n)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#s-indent(n)]]
    */
  def indent(n: Int): Parser[Int] = s"$Space{$n}".r ^^ { _ => n }

  /** In the block collection styles, you should be able to automatically detect additional indentation 
    * levels in addition to the given indentation level.
    * 
    * {{{
    *   [63] s-indent(n) ::= s-space × n
    * }}}
    * 
    * In the specification, if auto-detection of indentation level is required, it is indicated as follows:
    * 
    * {{{
    *   ...
    *   /* For some fixed auto-detected m > 0 */
    *   ...
    * }}}
    *
    * @return [[Parser]] for lexing '''s-indent(n)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#s-indent(n)]]
    */
  def indentAutoDetection: Parser[Int] = Parser { input =>
    parse(s"$Space*".r, input) match {
      case NoSuccess(_, _) => Failure("s-indent(n) expected, but not found", input)
      case Success(y, next) => Success(y.length, next)
    }
  }

  /** A block style construct is terminated when encountering a line which is less indented than the construct.
    *
    * {{{
    *   [64] s-indent(<n) ::= s-space × m /* Where m < n */
    * }}}
    *
    * @param n a number of indentation spaces
    * @return [[Parser]] for lexing '''s-indent(&lt;n)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#s-indent(&lt;n)]]
    */
  def indentLt(n: Int): Parser[Int] = n match {
    case x if x < 0 => throw new Exception() // TODO definition of exception
    case x if x == 0 => Parser { input => Success(0, input) }
    case _ => s"[$Space]*".r ^^ { case space if space.length < n => space.length }
  }

  /** A block style construct is terminated when encountering a line which is less indented than the construct.
    *
    * {{{
    *   [65] s-indent(≤n) ::= s-space × m /* Where m ≤ n */
    * }}}
    *
    * @param n a number of indentation spaces
    * @return [[Parser]] for lexing '''s-indent(≤n)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#s-indent(≤n)]]
    */
  def indentLte(n: Int): Parser[Int] = n match {
    case x if x < 0 => throw new Exception() // TODO definition of exception
    case x if x == 0 => Parser { input => Success(0, input) }
    case _ => s"[$Space]*".r ^^ { case space if space.length <= n => space.length }
  }
}
