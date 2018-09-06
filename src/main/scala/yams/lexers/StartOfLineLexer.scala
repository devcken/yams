package yams
package lexers

/** Provide the lexer for '''Start of line'''
  *
  * @author Leejun Choi
  * @since 0.1
  */
trait StartOfLineLexer extends scala.util.parsing.combinator.RegexParsers
                          with characters.LineBreak {
  /** \/\* Start of line \*\/ could be found thrice in YAML specification:
    *
    * 1. s-separate-in-line([[SeparationSpacesLexer.separateInLine]]):
    *
    * {{{
    *   [66] s-separate-in-line ::= s-white+ | /* Start of line */
    * }}}
    *
    * 2. s-l-comments([[CommentLexer.comments]]):
    *
    * {{{
    *   [79] s-l-comments ::= ( s-b-comment | /* Start of line */ )
    *                         l-comment*
    * }}}
    *
    * 3. c-forbidden(not complemented yet):
    *
    * {{{
    *   [206] c-forbidden ::= /* Start of line */
    *                         ( c-directives-end | c-document-end )
    *                         ( b-char | s-white | /* End of file */ )
    * }}}
    *
    * @return [[Parser]] for '''Start of line'''
    * @see [[http://yaml.org/spec/1.2/spec.html#s-separate-in-line]]
    * @see [[http://yaml.org/spec/1.2/spec.html#s-l-comments]]
    */
  private[lexers] def startOfLine: Parser[Option[Nothing]] = Parser { input =>
    if (input.offset == 0) Success(None, input)
    else {
      val precedingSubSequence = input.source.subSequence(0, input.offset)

      parse(s"[$Break]$$".r, precedingSubSequence) match {
        case NoSuccess(_, _) => Failure("/* Start of line */ expected, but not found", input)
        case Success(_, _) => Success(None, input)
      }
    }
  }
}
