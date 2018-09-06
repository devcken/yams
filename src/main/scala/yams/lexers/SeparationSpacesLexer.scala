package yams
package lexers

/** YAML uses white space characters for separation between tokens within a line.
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2778241]]
  */
trait SeparationSpacesLexer extends scala.util.parsing.combinator.RegexParsers
                               with yams.characters.WhiteSpace
                               with yams.characters.LineBreak
                               with StartOfLineLexer {
  override def skipWhitespace: Boolean = false

  /** Outside indentation and scalar content, YAML uses white space characters for separation between
    * tokens within a line. Note that such white space may safely include tab characters.
    * 
    * {{{
    *   [66] s-separate-in-line ::= s-white+ | /* Start of line */
    * }}}
    *
    * @return [[Parser]] for lexing '''s-separate-in-line'''   
    * @see [[http://yaml.org/spec/1.2/spec.html#s-separate-in-line]]
    */
  private[lexers] def separateInLine: Parser[Option[Nothing]] = Parser { input =>
    parse(s"[$White]+".r | startOfLine, input) match {
      case NoSuccess(_, _) => Failure("s-separate-in-line expected, but not found", input)
      case Success(_, next) => Success(None, next)
    }
  }
}
