package yams.lexers

trait DocumentStartLexer extends util.parsing.combinator.RegexParsers {
  import yams.tokens.DocumentStartToken

  /*
     [203]   c-directive-end ::=  "-" "-" "-"
   */

  /** The regular expression for `document start`.
    *
    * Although it is termed `c-directive-end` in the specification, Here define as `document start`.
    * The line of `c-directive-end` must be ended by a new line.
    * There may be a space between the three consecutive dashes and a new line.
    */
  private val DocumentStartRegex = "-{3}\\s*\\n".r
  
  def documentStart: Parser[DocumentStartToken.type] = DocumentStartRegex ^^ { _ => yams.tokens.DocumentStartToken }
}
