package yams
package lexers

trait ChompingMethod

object Strip extends ChompingMethod
object Clip extends ChompingMethod
object Keep extends ChompingMethod

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
  override def skipWhitespace: Boolean = false

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
          case Success(_, _) => Success(None, next)
        }
      case Success(y, next) => Success(Some(y.toInt), next)
    } 
  }

  /** Chomping controls how final line breaks and trailing empty lines are interpreted. YAML provides
    * three chomping methods:
    *
    * =Strip=
    * Stripping is specified by the “-” chomping indicator. In this case, the final line break and any
    * trailing empty lines are excluded from the scalar’s content.
    *
    * =Clip=
    * Clipping is the default behavior used if no explicit chomping indicator is specified. In this case,
    * the final line break character is preserved in the scalar’s content. However, any trailing empty
    * lines are excluded from the scalar’s content.
    *
    * =Keep=
    * Keeping is specified by the “+” chomping indicator. In this case, the final line break and any
    * trailing empty lines are considered to be part of the scalar’s content. These additional lines are
    * not subject to folding.
    *
    * {{{
    *   [164] c-chomping-indicator(t) ::= “-”         ⇒ t = strip
    *                                     “+”         ⇒ t = keep
    *                                     /* Empty */ ⇒ t = clip
    * }}}
    *
    * @return [[Parser]] for lexing '''c-chomping-indicator(t)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#c-chomping-indicator(t)]]
    */
  private[lexers] def chompingIndicator: Parser[ChompingMethod] = Parser { input =>
    parse(s"\\x2D|\\x2B|[$Break]|($$)".r, input) match {
      case NoSuccess(m, _) => Failure(m, input)
      case Success(y, next) => y match {
        case "-" => Success(Strip, next)
        case "+" => Success(Keep, next)
        case _ => Success(Clip, next)
      }
    }
  }

  /** The interpretation of the final line break of a block scalar is controlled by the chomping indicator
    * specified in the block scalar header.
    *
    * {{{
    *   [165] b-chomped-last(t) ::= t = strip ⇒ b-non-content | /* End of file */
    *                               t = clip  ⇒ b-as-line-feed | /* End of file */
    *                               t = keep  ⇒ b-as-line-feed | /* End of file */
    * }}}
    *
    * @param t [[Strip]], [[Keep]] or [[Clip]]
    * @return [[Parser]] for lexing '''b-chomped-last(t)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#b-chomped-last(t)]]
    */
  private[lexers] def chompedLast(t: ChompingMethod): Parser[String] = t match {
    case Strip => (s"[$NonContent]".r | "$$".r) ^^ { _ => "" }
    case Clip | Keep => breakAsLineFeed | "$$".r
  }
}
