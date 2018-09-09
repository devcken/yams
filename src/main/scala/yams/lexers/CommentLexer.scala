package yams.lexers

/** An explicit comment is marked by a “#” indicator.
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2780069]]
  */
trait CommentLexer extends util.parsing.combinator.RegexParsers
                      with SeparationSpacesLexer {
  override def skipWhitespace: Boolean = false

  /** An explicit comment must be marked by a “#” indicator.
    * 
    * {{{
    *   [75] c-nb-comment-text ::= "#" nb-char*
    * }}}
    * 
    * @return [[Parser]] for lexing '''c-nb-comment-text'''
    * @see [[http://yaml.org/spec/1.2/spec.html#c-nb-comment-text]]
    */
  private[lexers] def commentText: Parser[String] = Parser { input =>
    parse(s"#$NoBreakChar*".r, input) match {
      case NoSuccess(_, _) => Failure("Not found any c-nb-comment-text", input)
      case Success(y, next) => Success(y, next)
    }
  }
  
  /** Comments must be separated from other tokens by white space characters.
    * 
    * {{{
    *   [76] b-comment ::= b-non-content | /* End of file */
    * }}}
    * 
    * @return [[Parser]] for lexing '''b-comment'''
    * @see [[http://yaml.org/spec/1.2/spec.html#b-comment]]
    */
  private[lexers] def commentBreak: Parser[Option[Nothing]] = Parser { input: Input =>
    val source = input.source
    val subSequence = source.subSequence(input.offset, source.length)

    s"""(?:[$NonContent]|$$)""".r findPrefixMatchOf subSequence match {
//      case Some(m) if input.drop(m.end).atEnd =>
//        Failure("It's not a real failure, means that reached the end of sequence.", input.drop(m.end))
      case Some(m) => Success(None, input.drop(m.end))
      // Should not be reached here. If reached here, it means that something is wrong,
      // and should check all of CommentLexer again.
      case _       => Failure("Not found any b-comment.", input)
    }
  }
  
  /** To ensure JSON compatibility, YAML processors must allow for the omission of the final comment 
    * line break of the input stream. However, as this confuses many tools, YAML processors should 
    * terminate the stream with an explicit line break on output.
    * 
    * {{{
    *   [77] s-b-comment ::= (s-separate-in-line c-nb-comment-text?)?
    *                        b-comment
    * }}}
    *
    * As you can see, optional spaces is followed by optional comment text, then is followed by a break.
    * It means that comment text is optional, some whitespace is followed by it, and the whitespace is
    * optional as well. Therefore, this parser must parse the empty string which consists of only
    * optional spaces and tabs and a break as well as some common case such as ` # comment `.
    *
    * @return [[Parser]] for lexing '''s-b-comment'''
    * @see [[http://yaml.org/spec/1.2/spec.html#s-b-comment]]
    * @note [[commentBreak]] means just a break or end-of-file.
    */
  private[lexers] def optComment: Parser[Option[Nothing]] =
    (separateInLine ~ commentText.?).? <~ commentBreak ^^ { _ => None }

  /** Outside scalar content, comments may appear on a line of their own, independent of the indentation
    * level. Note that outside scalar content, a line containing only white space characters is taken to
    * be a comment line.
    *
    * {{{
    *   [78] l-comment ::= s-separate-in-line c-nb-comment-text? b-comment
    * }}}
    *
    * ''l-comment'' is very similar to ''s-b-comment'' except that ''s-separate-in-line'' is required.
    * So, this parser cannot parse if there is only ''break''.
    *
    * @return [[Parser]] for lexing l-comment
    * @see [[http://yaml.org/spec/1.2/spec.html#l-comment]]
    */
  protected[lexers] def comment: Parser[Option[String]] = separateInLine ~> commentText.? <~ commentBreak

  /** In most cases, when a line may end with a comment, YAML allows it to be followed by additional
    * comment lines. The only exception is a comment ending a block scalar header.
    *
    * {{{
    *   [79] s-l-comments ::= (s-b-comment | /* Start of line */ )
    *                         l-comment*
    * }}}
    *
    * @return [[Parser]] for lexing s-l-comments
    * @see [[http://yaml.org/spec/1.2/spec.html#s-l-comments]]
    */
  protected[lexers] def comments: Parser[Option[Nothing]] = Parser { input =>
    parse((optComment | startOfLine) ~ comment.*, input) match {
      case NoSuccess(_, _) => Failure("s-l-comments expected, but not found", input)
      case Success(_, next) => Success(None, next)
    }
  }
}
