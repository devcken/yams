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
                          with LineFoldingLexer
                          with FlowStylesLexer {
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
  private[lexers] def blockHeader: Parser[(Int, ChompingMethod)] = Parser { input =>
    parse(((indentationIndicator ~ chompingIndicator) ^^ { case a ~ b => (a, b) } |
      (chompingIndicator ~ indentationIndicator) ^^ { case b ~ a => (a, b) }) <~ optComment, input) match {
      case Success((Some(m), t), next) => Success((m, t), next)
      case Success((None, t), next) => parse(indentAutoDetection, next) match {
        case Success(m, _) => Success((m, t), next) // backtracking auto-detected indentations
        case NoSuccess(_, _) => Failure("c-b-block-header(m,t) expected, but not found", input)
      }
    }
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
    parse("[\\x31-\\x39]*".r, input) match {
      case NoSuccess(_, _) => Success(None, input)
      case Success(y, _) if y.isEmpty => Success(None, input) 
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
  
  import tokens.ScalarToken
  import tokens.Literal
  import tokens.Folded

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
  private[lexers] def literal(n: Int): Parser[ScalarToken] = Parser { input =>
    parse("|" ~> blockHeader, input) match {
      case NoSuccess(_, _) => Failure("c-l+literal(n) expected, but not found", input)
      case Success((m, t), next1) => parse(literalContent(n + m, t), next1) match {
        case NoSuccess(_, _) => Failure("c-l+literal(n) expected, but not found", input)
        case Success(y, next2) => Success(ScalarToken(y, Literal), next2)
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

  /** The folded style is denoted by the “>” indicator. It is similar to the literal style; however,
    * folded scalars are subject to line folding.
    *
    * {{{
    *   [174] c-l+folded(n) ::= “>” c-b-block-header(m,t)
    *                           l-folded-content(n+m,t)
    * }}}
    *
    * @param n a number of indentation spaces
    * @return [[Parser]] for lexing '''c-l+folded(n)'''
    */
  private[lexers] def folded(n: Int): Parser[ScalarToken] = Parser { input =>
    parse(">" ~> blockHeader, input) match {
      case NoSuccess(_, _) => Failure("c-l+folded(n) expected, but not found", input)
      case Success((m, t), next1) => parse(foldedContent(n + m, t), next1) match {
        case NoSuccess(_, _) => Failure("", input)
        case Success(y, next2) => Success(ScalarToken(y, Folded), next2)
      }
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

  /** The final line break, and trailing empty lines if any, are subject to chomping and are never folded.
    *
    * {{{
    *   [182] l-folded-content(n,t) ::= ( l-nb-diff-lines(n) b-chomped-last(t) )?
    *                                   l-chomped-empty(n,t)
    * }}}
    *
    * @param n a number of indentation spaces
    * @param t [[Strip]], [[Keep]] or [[Clip]]
    * @return [[Parser]] for lexing '''l-folded-content(n,t)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#l-folded-content(n,t)]]
    */
  private[lexers] def foldedContent(n: Int, t: ChompingMethod): Parser[String] =
    (diffLines(n) ~ chompedLast(t)).? ~ chompedEmpty(n, t) ^^ {
      case Some(a ~ b) ~ Some(c) => a + b + c
      case Some(a ~ b) ~ None => a + b
      case None ~ Some(c) => c
      case None ~ None => ""
    }
  
  import tokens.{FlowEntryToken, FlowNodeToken, FlowSequenceToken, FlowMappingToken}

  /** A block sequence is simply a series of nodes, each denoted by a leading “-” indicator. The “-”
    * indicator must be separated from the node by white space. This allows “-” to be used as the first
    * character in a plain scalar if followed by a non-space character (e.g. “-1”).
    *
    * {{{
    *   [183]    l+block-sequence(n) ::= ( s-indent(n+m) c-l-block-seq-entry(n+m) )+
    *                                    /* For some fixed auto-detected m > 0 */
    *   [184] c-l-block-seq-entry(n) ::= “-” /* Not followed by an ns-char */
    *                                    s-l+block-indented(n,block-in)
    * }}}
    *
    * @param n a number of indentation spaces
    * @return [[Parser]] for lexing '''l+block-sequence(n)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#l+block-sequence(n)]]
    */
  def blockSequence(n: Int): Parser[FlowSequenceToken] = {
    def blockSequenceEntries: Parser[FlowNodeToken] = Parser { input =>
      parse(indentAutoDetection, input) match {
        case NoSuccess(_, _) => Failure("l+block-sequence(n) expected, but not found", input)
        case Success(m, next) =>
          parse(blockSeqEntry(n + m), next) match {
            case NoSuccess(msg, _) => Failure("l+block-sequence(n) expected, but not found", input)
            case Success(y, rest) => Success(y, rest)
          }
      }
    }
    
    blockSequenceEntries.+ ^^ FlowSequenceToken
  }

  def blockSeqEntry(n: Int): Parser[FlowNodeToken] = Parser { input =>
    parse("-" ~> blockIndented(n, BlockIn), input)
  }

  /** The entry node may be either completely empty, be a nested block node, or use a compact in-line
    * notation. The compact notation may be used when the entry is itself a nested block collection.
    * In this case, both the “-” indicator and the following spaces are considered to be part of the
    * indentation of the nested collection. Note that it is not possible to specify node properties
    * for such a collection.
    *
    * {{{
    *   [185]  s-l+block-indented(n,c) ::=   ( s-indent(m)
    *                                          ( ns-l-compact-sequence(n+1+m)
    *                                          | ns-l-compact-mapping(n+1+m) ) )
    *                                      | s-l+block-node(n,c)
    *                                      | ( e-node s-l-comments )
    *   [186] ns-l-compact-sequence(n) ::= c-l-block-seq-entry(n)
    *                                      ( s-indent(n) c-l-block-seq-entry(n) )*
    * }}}
    *
    * @param n a number of indentation spaces
    * @param c [[BlockOut]], [[BlockIn]], [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''s-l+block-indented(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#s-l+block-indented(n,c)]]
    */
  def blockIndented(n: Int, c: Context): Parser[FlowNodeToken] = {
    def compactSequence(n: Int): Parser[FlowSequenceToken] = 
      blockSeqEntry(n) ~ (indent(n) ~> blockSeqEntry(n)).* ^^ { case a ~ b => FlowSequenceToken(a +: b) }
    
    def compactSeqOrMap: Parser[FlowNodeToken] = Parser { input => 
      parse(indentAutoDetection, input) match {
        case Success(m, next1) =>
          parse(compactSequence(n + m + 1) | compactMapping(n + m + 1), next1) match {
            case NoSuccess(_, _) => Failure("", input)
            case Success(y, next2) => Success(y, next2)
          }
      }
    }

    compactSeqOrMap | blockNode(n, c) | (emptyNode <~ comments)
  }

  /** A Block mapping is a series of entries, each presenting a key: value pair.
    *
    * {{{
    *   [187] l+block-mapping(n) ::= ( s-indent(n+m) ns-l-block-map-entry(n+m) )+
    *                                /* For some fixed auto-detected m > 0 */
    * }}}
    *
    * @param n a number of indentation spaces
    * @return [[Parser]] for lexing '''l+block-mapping(n)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#l+block-mapping(n)]]
    */
  def blockMapping(n: Int): Parser[FlowMappingToken] = {
    def blockMappingEntries: Parser[FlowEntryToken] = Parser { input =>
      parse(indentAutoDetection, input) match {
        case NoSuccess(_, _) => Failure("l+block-mapping(n) expected, but not found", input)
        case Success(m, next1) => parse(blockMapEntry(n + m), next1) match {
          case NoSuccess(msg, _) => Failure("l+block-mapping(n) expected, but not found", input)
          case Success(y, next2) => Success(y, next2)
        }
      }
    }
    
    blockMappingEntries.+ ^^ FlowMappingToken
  }

  /** If the “?” indicator is specified, the optional value node must be specified on a separate line,
    * denoted by the “:” indicator. Note that YAML allows here the same compact in-line notation
    * described above for block sequence entries.
    *
    * {{{
    *   [188]         ns-l-block-map-entry(n) ::=   c-l-block-map-explicit-entry(n)
    *                                             | ns-l-block-map-implicit-entry(n)
    *   [189] c-l-block-map-explicit-entry(n) ::= c-l-block-map-explicit-key(n)
    *                                             ( l-block-map-explicit-value(n)
    *                                             | e-node )
    *   [190]   c-l-block-map-explicit-key(n) ::= “?” s-l+block-indented(n,block-out)
    *   [191]   l-block-map-explicit-value(n) ::= s-indent(n)
    *                                             “:” s-l+block-indented(n,block-out)
    * }}}
    *
    * @param n a number of indentation spaces
    * @return [[Parser]] for lexing '''ns-l-block-map-entry(n)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-l-block-map-entry(n)]]
    */
  def blockMapEntry(n: Int): Parser[FlowEntryToken] = {
    def blockMapExplicitEntry(n: Int): Parser[FlowEntryToken] = 
      blockMapExplicitKey(n) ~ (blockMapExplicitValue(n) | emptyNode) ^^ {
        case k ~ v => FlowEntryToken(k, v)
      }

    def blockMapExplicitKey(n: Int): Parser[FlowNodeToken] =
      "?" ~> blockIndented(n, BlockOut)

    def blockMapExplicitValue(n: Int): Parser[FlowNodeToken] = Parser { input =>
      parse(indent(n) ~> ":" ~> blockIndented(n, BlockOut), input)
    }

    blockMapExplicitEntry(n) | blockMapImplicitEntry(n)
  }

  /** If the “?” indicator is omitted, parsing needs to see past the implicit key, in the same way as in
    * the single key: value pair flow mapping. Hence, such keys are subject to the same restrictions;
    * they are limited to a single line and must not span more than 1024 Unicode characters.
    *
    * {{{
    *   [192] ns-l-block-map-implicit-entry(n) ::= ( ns-s-block-map-implicit-key
    *                                              | e-node )
    *                                              c-l-block-map-implicit-value(n)
    *   [193]      ns-s-block-map-implicit-key ::=  c-s-implicit-json-key(block-key)
    *                                              | ns-s-implicit-yaml-key(block-key)
    * }}}
    *
    * In this case, the value may be specified on the same line as the implicit key. Note however that
    * in block mappings the value must never be adjacent to the “:”, as this greatly reduces readability
    * and is not required for JSON compatibility (unlike the case in flow mappings).
    *
    * There is no compact notation for in-line values. Also, while both the implicit key and the value
    * following it may be empty, the “:” indicator is mandatory. This prevents a potential ambiguity with
    * multi-line plain scalars.
    *
    * {{{
    *   [194] c-l-block-map-implicit-value(n) ::= “:” ( s-l+block-node(n,block-out)
    *                                                 | ( e-node s-l-comments ) )
    * }}}
    *
    * @param n a number of indentation spaces
    * @return [[Parser]] for lexing '''c-l-block-map-implicit-value(n)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#c-l-block-map-implicit-value(n)]]
    */
  def blockMapImplicitEntry(n: Int): Parser[FlowEntryToken] = {
    def blockMapImplicitKey: Parser[FlowNodeToken] = Parser { input =>
      parse(implicitKey(BlockKey, FlowJson) | implicitKey(BlockKey, FlowYaml), input)
    }

    def blockMapImplicitValue(n: Int): Parser[FlowNodeToken] = Parser { input =>
      parse(":" ~> (blockNode(n, BlockOut) | (emptyNode <~ comments)), input)
    }

    (blockMapImplicitKey | emptyNode) ~ blockMapImplicitValue(n) ^^ { case k ~ v => FlowEntryToken(k, v) }
  }

  /** A compact in-line notation is also available. This compact notation may be nested inside block
    * sequences and explicit block mapping entries. Note that it is not possible to specify node
    * properties for such a nested mapping.
    *
    * {{{
    *   [195] ns-l-compact-mapping(n) ::= ns-l-block-map-entry(n)
    *                                     ( s-indent(n) ns-l-block-map-entry(n) )*
    * }}}
    *
    * @param n a number of indentation spaces
    * @return [[Parser]] for lexing '''ns-l-compact-mapping(n)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-l-compact-mapping(n)]]
    */
  def compactMapping(n: Int): Parser[FlowMappingToken] = 
    blockMapEntry(n) ~ (indent(n) ~> blockMapEntry(n)).* ^^ { case a ~ b => FlowMappingToken(a +: b) }

  /** YAML allows flow nodes to be embedded inside block collections (but not vice-versa). Flow nodes
    * must be indented by at least one more space than the parent block collection. Note that flow nodes
    * may begin on a following line.
    *
    * It is at this point that parsing needs to distinguish between a plain scalar and an implicit key
    * starting a nested block mapping.
    *
    * {{{
    *   [196]  s-l+block-node(n,c) ::= s-l+block-in-block(n,c) | s-l+flow-in-block(n)
    *   [197] s-l+flow-in-block(n) ::= s-separate(n+1,flow-out)
    *                                  ns-flow-node(n+1,flow-out) s-l-comments
    * }}}
    *
    * The block node’s properties may span across several lines. In this case, they must be indented by
    * at least one more space than the block collection, regardless of the indentation of the block
    * collection entries.
    *
    * {{{
    *   [198] s-l+block-in-block(n,c) ::= s-l+block-scalar(n,c) | s-l+block-collection(n,c)
    *   [199]   s-l+block-scalar(n,c) ::= s-separate(n+1,c)
    *                                     ( c-ns-properties(n+1,c) s-separate(n+1,c) )?
    *                                     ( c-l+literal(n) | c-l+folded(n) )
    * }}}
    *
    * Since people perceive the “-” indicator as indentation, nested block sequences may be indented by
    * one less space to compensate, except, of course, if nested inside another block sequence
    * (block-out context vs. block-in context).
    *
    * {{{
    *   [200] s-l+block-collection(n,c) ::= ( s-separate(n+1,c) c-ns-properties(n+1,c) )?
    *                                       s-l-comments
    *                                       ( l+block-sequence(seq-spaces(n,c))
    *                                       | l+block-mapping(n) )
    *   [201]           seq-spaces(n,c) ::= c = block-out ⇒ n-1
    *                                       c = block-in  ⇒ n
    * }}}
    *
    * @param n a number of indentation spaces
    * @param c [[BlockOut]], [[BlockIn]], [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''s-l+block-node(n,c)'''
    */
  private[lexers] def blockNode(n: Int, c: Context): Parser[FlowNodeToken] = {
    def flowInBlock(n: Int): Parser[FlowNodeToken] =
      separate(n + 1, FlowOut) ~> flowNode(n + 1, FlowOut) <~ comments

    def blockInBlock(n: Int, c: Context): Parser[FlowNodeToken] = blockScalar(n, c) | blockCollection(n, c)

    def blockScalar(n: Int, c: yams.Context): Parser[ScalarToken] =
      separate(n + 1, c) ~> 
        (nodeProperties(n + 1, c) <~ separate(n + 1, c)).? ~
        (literal(n) | folded(n)) ^^ { case property ~ scalar => scalar + property }

    def blockCollection(n: Int, c: Context): Parser[FlowNodeToken] =
      (separate(n + 1, c) ~> nodeProperties(n + 1, c)).? ~
        (comments ~> (blockSequence(seqSpaces(n, c)) | blockMapping(n))) ^^ { 
        case property ~ node => node + property
      }

    def seqSpaces(n: Int, c: Context): Int = c match {
      case BlockOut => n - 1
      case BlockIn => n
      case _ => n
    }

    blockInBlock(n, c) | flowInBlock(n)
  }
}
