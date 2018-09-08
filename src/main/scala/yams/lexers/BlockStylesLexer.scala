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
                          with characters.MiscChar
                          with IndentationSpacesLexer
                          with CommentLexer
                          with EmptyLinesLexer
                          with LineFoldingLexer {
  /** Block scalars are controlled by a few indicators given in a header preceding the content itself. 
    * This header is followed by a non-content line break with an optional comment. This is the only case 
    * where a comment must not be followed by additional comment lines.
    * 
    * {{{
    *   [162] c-b-block-header(m,t) ::= ( ( c-indentation-indicator(m)
    *                                       c-chomping-indicator(t) )
    *                                   | ( c-chomping-indicator(t)
    *                                       c-indentation-indicator(m) ) )
    *                                   s-b-comment
    * }}}
    * 
    * @return [[Parser]] for lexing '''c-b-block-header(m,t)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#c-b-block-header(m,t)]]
    */
  private[lexers] def blockHeader: Parser[(Int, ChompingMethod)] =
    ((indentationIndicator ~ chompingIndicator) ^^ { case a ~ b => (a, b) } |
      (chompingIndicator ~ indentationIndicator) ^^ { case b ~ a => (a, b) }) <~ optComment ^^ {
      case (Some(m), t) => (m, t)
      case (None, t) => (0, t)
    }
  
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
    parse("[\\x31-\\x39]{1}".r, input) match {
      case NoSuccess(_, _) => Success(None, input)
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
    parse(s"[\\x2D\\x2B]{1}".r, input) match {
      case NoSuccess(_, _) => Success(Clip, input)
      case Success(y, next) => y match {
        case "-" => Success(Strip, next)
        case "+" => Success(Keep, next)
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

  /** The interpretation of the trailing empty lines following a block scalar is also controlled by the
    * chomping indicator specified in the block scalar header.
    *
    * {{{
    *   [166] l-chomped-empty(n,t) ::= t = strip ⇒ l-strip-empty(n)
    *                                  t = clip  ⇒ l-strip-empty(n)
    *                                  t = keep  ⇒ l-keep-empty(n)
    *   [167]     l-strip-empty(n) ::= ( s-indent(≤n) b-non-content )*
    *                                  l-trail-comments(n)?
    *   [168]      l-keep-empty(n) ::= l-empty(n,block-in)*
    *                                  l-trail-comments(n)?
    * }}}
    *
    * Explicit comment lines may follow the trailing empty lines. To prevent ambiguity, the first such
    * comment line must be less indented than the block scalar content. Additional comment lines, if any,
    * are not so restricted. This is the only case where the indentation of comment lines is constrained.
    *
    * {{{
    *   [169] l-trail-comments(n) ::= s-indent(<n) c-nb-comment-text b-comment
    *                                 l-comment*
    * }}}
    *
    * If a block scalar consists only of empty lines, then these lines are considered as trailing lines
    * and hence are affected by chomping.
    *
    * @param n a number of indentation spaces
    * @param t [[Strip]], [[Keep]] or [[Clip]]
    * @return [[Parser]] for lexing l-chomped-empty(n,t)
    * @see [[http://yaml.org/spec/1.2/spec.html#l-trail-comments(n)]]
    */
  private[lexers] def chompedEmpty(n: Int, t: ChompingMethod): Parser[Option[String]] = {
    def stripEmpty(n: Int): Parser[Option[String]] =
      (indentLte(n) ~ s"[$NonContent]".r).* ~> trailComments(n).? ^^ {
        case Some(x) if x.nonEmpty => None
        case _ => Some("")
      }

    def keepEmpty(n: Int): Parser[Option[String]] =
      emptyLine(n, BlockIn).* ~> trailComments(n).? ^^ {
        case Some(x) if x.nonEmpty => None
        case _ => Some("\n")
      }

    def trailComments(n: Int): Parser[String] =
      indentLt(n) ~> (commentText <~ commentBreak) ~ comment.* ^^ {
        case a ~ b => a + b.filter(p => p.nonEmpty).map(x => x.get).mkString
      }

    t match {
      case Strip | Clip => stripEmpty(n)
      case Keep => keepEmpty(n)
    }
  }

  /** The literal style is denoted by the “|” indicator. It is the simplest, most restricted, and most 
    * readable scalar style.
    * 
    * {{{
    *   [170] c-l+literal(n) ::= “|” c-b-block-header(m,t)
    *                            l-literal-content(n+m,t)
    * }}}
    * 
    * @param n a number of indentation spaces
    * @return [[Parser]] for '''c-l+literal(n)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#c-l+literal(n)]]
    */
  private[lexers] def literal(n: Int): Parser[String] = Parser { input =>
    parse("|" ~> blockHeader, input) match {
      case NoSuccess(_, _) => Failure("c-l+literal(n) expected, but not found", input)
      case Success((m, t), next1) => parse(literalContent(n + m, t), next1) match {
        case NoSuccess(msg, _) => Error(msg, next1)
        case Success(y, next2) => Success(y, next2)
      }
    }
  }

  /** Inside literal scalars, all (indented) characters are considered to be content, including white 
    * space characters. Note that all line break characters are normalized. In addition, empty lines are 
    * not folded, though final line breaks and trailing empty lines are chomped.
    * 
    * There is no way to escape characters inside literal scalars. This restricts them to printable 
    * characters. In addition, there is no way to break a long literal line.
    * 
    * {{{
    *   [171]   l-nb-literal-text(n) ::= l-empty(n,block-in)*
    *                                    s-indent(n) nb-char+
    *   [172]   b-nb-literal-next(n) ::= b-as-line-feed
    *                                    l-nb-literal-text(n)
    *   [173] l-literal-content(n,t) ::= ( l-nb-literal-text(n) b-nb-literal-next(n)*
    *                                      b-chomped-last(t) )?
    *                                    l-chomped-empty(n,t)
    * }}}
    * 
    * @param n a number of indentation spaces
    * @param t [[Strip]], [[Keep]] or [[Clip]]
    * @return [[Parser]] for lexing '''l-literal-content(n,t)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#l-literal-content(n,t)]]
    */
  private[lexers] def literalContent(n: Int, t: ChompingMethod): Parser[String] = {
    def literalTextLines(n: Int): Parser[String] =
      emptyLine(n, BlockIn).* ~ (indent(n) ~> s"$NoBreakChar+".r) ^^ { case a ~ b => a.mkString + b }

    def literalTextLinesPrecedingBreak(n: Int): Parser[String] =
      breakAsLineFeed ~ literalTextLines(n) ^^ { case a ~ b => a + b }
    
    (literalTextLines(n) ~ literalTextLinesPrecedingBreak(n).* ~ chompedLast(t)).? ~ chompedEmpty(n, t) ^^ {
      case Some(a ~ b ~ c) ~ Some(d) => a + b.mkString + c + d
      case Some(a ~ b ~ c) ~ None => a + b.mkString + c
      case None ~ Some(d) => d
      case None ~ None => ""
    }
  }

  /** Folding allows long lines to be broken anywhere a single space character separates two non-space
    * characters.
    *
    * {{{
    *   [175]  s-nb-folded-text(n) ::= s-indent(n) ns-char nb-char*
    *   [176] l-nb-folded-lines(n) ::= s-nb-folded-text(n)
    *                                  ( b-l-folded(n,block-in) s-nb-folded-text(n) )*
    * }}}
    *
    * @param n a number of indentation spaces
    * @return [[Parser]] for lexing '''l-nb-folded-lines(n)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#l-nb-folded-lines(n)]]
    */
  private[lexers] def foldedLines(n: Int): Parser[String] = {
    def foldedText(n: Int): Parser[String] =
      indent(n) ~> s"[$NoSpaceChar]".r ~ s"$NoBreakChar*".r ^^ { case a ~ b => a + b }

    foldedText(n) ~ (folded(n, BlockIn) ~ foldedText(n)).* ^^ {
      case a ~ b => a + b.map(c => c._1 + c._2).mkString
    }
  }

  /** Lines starting with white space characters (more-indented lines) are not folded.
    *
    * {{{
    *   [177]  s-nb-spaced-text(n) ::= s-indent(n) s-white nb-char*
    *   [178]        b-l-spaced(n) ::= b-as-line-feed
    *                                  l-empty(n,block-in)*
    *   [179] l-nb-spaced-lines(n) ::= s-nb-spaced-text(n)
    *                                  ( b-l-spaced(n) s-nb-spaced-text(n) )*
    * }}}
    *
    * @param n a number of indentation spaces
    * @return [[Parser]] for lexing '''l-nb-spaced-lines(n)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#l-nb-spaced-lines(n)]]
    */
  private[lexers] def spacedLines(n: Int): Parser[String] = {
    def spacedText(n: Int): Parser[String] =
      indent(n) ~> s"[$White]".r ~ s"$NoBreakChar*".r ^^ { case a ~ b => a + b }

    def spaced(n: Int): Parser[String] =
      breakAsLineFeed ~ emptyLine(n, BlockIn).* ^^ { case a ~ b => a + b.mkString }

    spacedText(n) ~ (spaced(n) ~ spacedText(n)).* ^^ { case a ~ b => a + b.map(c => c._1 + c._2).mkString }
  }

  /** Line breaks and empty lines separating folded and more-indented lines are also not folded.
    *
    * {{{
    *   [180] l-nb-same-lines(n) ::= l-empty(n,block-in)*
    *                                ( l-nb-folded-lines(n) | l-nb-spaced-lines(n) )
    *   [181] l-nb-diff-lines(n) ::= l-nb-same-lines(n)
    *                                ( b-as-line-feed l-nb-same-lines(n) )*
    * }}}
    *
    * @param n a number of indentation spaces
    * @return [[Parser]] for lexing '''l-nb-diff-lines(n)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#l-nb-diff-lines(n)]]
    */
  private[lexers] def diffLines(n: Int): Parser[String] = {
    def sameLines(n: Int): Parser[String] =
      emptyLine(n, BlockIn).* ~ (foldedLines(n) | spacedLines(n)) ^^ { case a ~ b => a.mkString + b }

    sameLines(n) ~ (breakAsLineFeed ~ sameLines(n)).* ^^ { case a ~ b => a + b.map(c => c._1 + c._2).mkString }
  }
}
