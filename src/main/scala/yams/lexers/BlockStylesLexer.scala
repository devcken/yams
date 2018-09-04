package yams
package lexers

/** YAML’s block styles employ indentation rather than indicators to denote structure. This results in 
  * a more human readable (though less compact) notation.
  * 
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#Block]]
  */
trait BlockStylesLexer extends scala.util.parsing.combinator.RegexParsers
                          with characters.LineBreak
                          with characters.MiscChar {
  /** Typically, the indentation level of a block scalar is detected from its first non-empty line. It is 
    * an error for any of the leading empty lines to contain more spaces than the first non-empty line.
    * 
    * Detection fails when the first non-empty line contains leading content space characters. Content 
    * may safely start with a tab or a “#” character.
    * 
    * When detection would fail, YAML requires that the indentation level for the content be given using 
    * an explicit indentation indicator. This level is specified as the integer number of the additional 
    * indentation spaces used for the content, relative to its parent node.
    * 
    * It is always valid to specify an indentation indicator for a block scalar node, though a YAML 
    * processor should only emit an explicit indentation indicator for cases where detection will fail.
    * 
    * {{{
    *   [163] c-indentation-indicator(m) ::= ns-dec-digit ⇒ m = ns-dec-digit - #x30
    *                                        /* Empty */  ⇒ m = auto-detect()
    * }}}
    * 
    * @return [[Parser]] for lexing c-indentation-indicator(m)
    * @see [[http://yaml.org/spec/1.2/spec.html#c-indentation-indicator(m)]]
    */
  private[lexers] def indentationIndicator: Parser[Option[Int]] = Parser { input =>
    parse("[\\x31-\\x39]".r, input) match {
      case NoSuccess(_, next) =>
        parse(s"[$Break]|$$".r, next) match {
          case NoSuccess(m, _) => Failure(m, input)
          case Success(y, _) => Success(None, next)
        }
      case Success(y, next) => Success(Some(y.toInt), next)
    } 
  }
}
