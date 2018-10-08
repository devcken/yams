package yams
package lexers

/** The parser which parses the separation within a key and a corresponding structure.
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2780810]]
  */
trait SeparationLinesLexer extends scala.util.parsing.combinator.RegexParsers
                              with CommentLexer
                              with LinePrefixesLexer {
  /** Implicit keys are restricted to a single line. In all other cases, YAML allows tokens to be
    * separated by multi-line (possibly empty) comments.
    *
    * {{{
    *   [80] s-separate(n,c) ::= c = block-out ⇒ s-separate-lines(n)
    *                            c = block-in  ⇒ s-separate-lines(n)
    *                            c = flow-out  ⇒ s-separate-lines(n)
    *                            c = flow-in   ⇒ s-separate-lines(n)
    *                            c = block-key ⇒ s-separate-in-line
    *                            c = flow-key  ⇒ s-separate-in-line
    * }}}
    *
    * @param n a number of indentation spaces
    * @param c [[BlockIn]], [[BlockOut]], [[FlowIn]] or [[FlowOut]]
    * @return [[Parser]] for lexing '''s-separate(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#s-separate(n,c)]]
    */
  private[yams] def separate(n: Int, c: Context): Parser[Option[Nothing]] = c match {
    case BlockOut | BlockIn | FlowOut | FlowIn => separateLines(n)
    case BlockKey | FlowKey => separateInLine
    case _ => throw new Exception
  }

  /** Note that structures following multi-line comment separation must be properly indented, even though
    * there is no such restriction on the separation comment lines themselves.
    *
    * {{{
    *   [81] s-separate-lines(n) ::=   ( s-l-comments s-flow-line-prefix(n) )
    *                                | s-separate-in-line
    * }}}
    *
    * @param n a number of indentation spaces
    * @return [[Parser]] for lexing s-separate-lines(n)
    * @see [[http://yaml.org/spec/1.2/spec.html#s-separate-lines(n)]]
    * @see [[separateInLine]]
    */
  private[lexers] def separateLines(n: Int): Parser[Option[Nothing]] =
    ((comments ~ flowLinePrefix(n)) | separateInLine) ^^ { _ => None }
}
