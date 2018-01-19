package yams.lexers

trait DocumentEndLexer extends util.parsing.combinator.RegexParsers {
  import yams.tokens.DocumentEndToken

  /*
     [204]   c-document-end ::=  "." "." "."
   */

  /** The regular expression for `document end`.
    *
    * The line of `c-document-end` must be ended by a new line.
    * There may be a space between the three consecutive dashes and a new line.
    */
  private val DocumentEndRegex = "\\.{3}\\s*\\n".r

  def documentEnd: Parser[DocumentEndToken.type] = DocumentEndRegex ^^ { _ => yams.tokens.DocumentEndToken }
}
