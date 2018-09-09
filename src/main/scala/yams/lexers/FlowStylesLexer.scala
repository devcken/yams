package yams
package lexers

/** YAML’s flow styles can be thought of as the natural extension of JSON to cover:
  * 
  * - folding long content lines for readability,
  * - tagging nodes to control construction of native data structures,
  * - and using anchors and aliases to reuse constructed object instances
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#Flow]]
  */
trait FlowStylesLexer extends scala.util.parsing.combinator.RegexParsers
                         with characters.CharacterSet
                         with characters.LineBreak
                         with characters.Escaped
                         with EmptyLinesLexer
                         with SeparationLinesLexer
                         with LinePrefixesLexer
                         with LineFoldingLexer
                         with NodePropertyLexer {
  import tokens.AliasToken
  
  /** Subsequent occurrences of a previously serialized node are presented as alias nodes.
    * The first occurrence of the node must be marked by an ''anchor'' to allow subsequent occurrences
    * to be presented as alias nodes.
    * 
    * An alias node is denoted by the '''“*”''' indicator.
    * The alias refers to the most recent preceding node having the same anchor.
    * 
    * {{{
    *   [104] c-ns-alias-node ::= “*” ns-anchor-name
    * }}}
    * 
    * @return [[Parser]] for '''c-ns-alias-nodes'''
    * @see [[http://yaml.org/spec/1.2/spec.html#c-ns-alias-node]]
    */
  def aliasNode: Parser[AliasToken] = Parser { input =>
    parse("*", input) match {
      case NoSuccess(_, _) => Failure("Expected [104]c-ns-alias-node, but not found", input)
      case Success(_, next) => parse(s"$AnchorName".r, next) match {
        case NoSuccess(_, rest) => Error("Failed to parse a anchor name.", rest)
        case Success(y, rest) => Success(AliasToken(y), rest)
      }
    }
  }

  import tokens.EmptyNodeToken
  
  /** YAML allows the node content to be omitted in many cases. Nodes with empty content are interpreted
    * as if they were plain scalars with an empty value. Such nodes are commonly resolved to a '''“null”''' value.
    * 
    * {{{
    *   [105] e-scalar ::= /* Empty */
    * }}}
    * 
    * @return [[Parser]] for '''e-scalar'''
    * @see [[http://yaml.org/spec/1.2/spec.html#e-scalar]]
    */
  private[lexers] def emptyScalar: Parser[EmptyNodeToken] = Parser { input =>
    parse(s"[$Space]*([$Break]||[,:]||$$)".r, input) match {
      case NoSuccess(_, _) => Failure("No empty scalar", input)
      case Success(_, _) => Success(EmptyNodeToken(), input)
    }
  }

  /** Both the node’s properties and node content are optional. This allows for a completely empty node.
    * Completely empty nodes are only valid when following some explicit indication for their existence.
    * 
    * {{{
    *   [106] e-node ::= e-scalar
    * }}}
    * 
    * @return [[Parser]] for '''e-node'''
    * @see [[http://yaml.org/spec/1.2/spec.html#e-node e-node]]
    */
  
  private[lexers] def emptyNode: Parser[EmptyNodeToken] = emptyScalar
  
  /** The double-quoted style is specified by surrounding “"” indicators. This is the only style capable 
    * of expressing arbitrary strings, by using “\” escape sequences. This comes at the cost of having to 
    * escape the “\” and “"” characters.
    * 
    * {{{
    *   [107] nb-double-char ::= c-ns-esc-char | ( nb-json - “\” - “"” )
    * }}}
    * 
    * @return [[Parser]] for '''nb-double-char'''
    * @see [[http://yaml.org/spec/1.2/spec.html#nb-double-char]]
    */
  private def doubleChar: Parser[String] = escapedCharacter | "[\\x09\\x20-\\x21\\x23-\\x5B\\x5D-\\x{10FFFF}]".r

  /** `ns-double-char` is a character set which whitespace characters is removed from `nb-double-char`.
    * 
    * {{{
    *   [108] ns-double-char ::= nb-double-char - s-white
    * }}}
    * 
    * @return [[Parser]] for '''ns-double-char'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-double-char]]
    */
  private def doubleCharNoSpaces: Parser[String] = escapedCharacter | "[\\x21\\x23-\\x5B\\x5D-\\x{10FFFF}]".r
  
  /** The double-quoted style is specified by surrounding “"” indicators.
    * 
    * {{{
    *   [109] c-double-quoted(n,c) ::= “"” nb-double-text(n,c) “"”
    *   [110]  nb-double-text(n,c) ::= c = flow-out  ⇒ nb-double-multi-line(n)
    *                                  c = flow-in   ⇒ nb-double-multi-line(n)
    *                                  c = block-key ⇒ nb-double-one-line
    *                                  c = flow-key  ⇒ nb-double-one-line
    * }}}
    * 
    * @param n a number of indentation spaces
    * @param c [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @return [[Parser]] for '''c-double-quoted'''
    * @see [[http://yaml.org/spec/1.2/spec.html#c-double-quoted(n,c)]]
    */
  private[lexers] def doubleQuoted(n: Int, c: Context): Parser[String] = "\"" ~> {
    c match {
      case _ @ (FlowOut | FlowIn) => doubleMultiLine(n)
      case _ @ (BlockKey | FlowKey) => doubleOneLine
      case _ => throw new Exception("context not supported") // TODO definition of exception
    }
  } <~ "\""

  /** ''Double-quoted'' scalars are restricted to a single line when contained inside an implicit key.
    * 
    * {{{
    *   [111] nb-double-one-line ::= nb-double-char*
    * }}}
    * 
    * @return [[Parser]] for '''nb-double-one-line'''
    * @see [[http://yaml.org/spec/1.2/spec.html#nb-double-one-line]]
    */
  private[lexers] def doubleOneLine: Parser[String] = doubleChar.* ^^ { _.mkString }
  
  /** In a multi-line double-quoted scalar, line breaks are are subject to flow line folding, which 
    * discards any trailing white space characters. It is also possible to escape the line break 
    * character. In this case, the line break is excluded from the content, and the trailing white space 
    * characters are preserved. Combined with the ability to escape white space characters, this allows 
    * double-quoted lines to be broken at arbitrary positions.
    * 
    * {{{
    *   [112] s-double-escaped(n) ::= s-white* “\” b-non-content
    *                                 l-empty(n,flow-in)* s-flow-line-prefix(n)
    *   [113]   s-double-break(n) ::= s-double-escaped(n) | s-flow-folded(n)
    * }}}
    * 
    * @param n a number of indentation spaces
    * @return [[Parser]] for lexing s-double-break(n)
    * @see [[http://yaml.org/spec/1.2/spec.html#s-double-escaped(n)]]
    * @see [[http://yaml.org/spec/1.2/spec.html#s-double-break(n)]]
    */
  private[lexers] def doubleBreak(n: Int): Parser[String] = {
    def doubleEscaped(n: Int): Parser[String] =
      (s"[$White]*".r <~ s"\\x5C[$Break]".r) ~ emptyLine(n, FlowIn).* <~ flowLinePrefix(n) ^^ 
        { case w ~ e => w.replace("\t", "\\t") + e.mkString }
    doubleEscaped(n) | flowFolded(n)
  }
  
  /** All leading and trailing white space characters are excluded from the content. Each continuation 
    * line must therefore contain at least one non-space character. Empty lines, if any, are consumed 
    * as part of the line folding.
    * 
    * {{{
    *   [114]    nb-ns-double-in-line ::= ( s-white* ns-double-char )*
    *   [115]   s-double-next-line(n) ::= s-double-break(n)
    *                                     ( ns-double-char nb-ns-double-in-line
    *                                     ( s-double-next-line(n) | s-white* ) )?
    *   [116] nb-double-multi-line(n) ::= nb-ns-double-in-line
    *                                     ( s-double-next-line(n) | s-white* )
    * }}}
    *
    * @param n a number of indentation spaces
    * @return [[Parser]] for lexing '''nb-double-multi-line(n)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#nb-double-multi-line(n)]]
    * @see [[http://yaml.org/spec/1.2/spec.html#s-double-next-line(n)]]
    * @see [[http://yaml.org/spec/1.2/spec.html#nb-ns-double-in-line]]
    */
  private[lexers] def doubleMultiLine(n: Int): Parser[String] = {
    def doubleInLine: Parser[String] = (s"[$White]*".r ~ doubleCharNoSpaces).* ^^
      { _.map{ case w ~ c => w.mkString + c }.mkString }
    
    def doubleNextLine(n: Int): Parser[String] =
      doubleBreak(n) ~ (doubleCharNoSpaces ~ doubleInLine ~ (doubleNextLine(n) | s"[$White]*".r)).? ^^ {
        case a ~ Some(b ~ c ~ d) => a + b + c + d
        case _ ~ None => ""
      }
    doubleInLine ~ (doubleNextLine(n) | s"[$White]*".r) ^^ { case a ~ b => a + b }
  }
  
  /** The single-quoted style is specified by surrounding “'” indicators. Therefore, within 
    * a single-quoted scalar, such characters need to be repeated. This is the only form of escaping 
    * performed in single-quoted scalars.
    * 
    * {{{
    *   [117] c-quoted-quote ::= “'” “'”
    * }}}
    * 
    * @return escaped single-quote
    */
  private val QuotedQuote = "\'\'"
  
  /** In particular, within a single-quoted scalar, the “\” and “"” characters may be freely used. This 
    * restricts single-quoted scalars to printable characters. In addition, it is only possible to break 
    * a long single-quoted line where a space character is surrounded by non-spaces.
    * 
    * {{{
    *   [119] ns-single-char ::= nb-single-char - s-white
    * }}}
    * 
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-single-char]]
    */
  private def singleCharNoSpace: Parser[String] = QuotedQuote | "[\\x09\\x21-\\x26\\x28-\\x{10FFFF}]".r

  /** Single-quoted scalars are restricted to a single line when contained inside a implicit key.
    * 
    * {{{
    *   [120] c-single-quoted(n,c) ::= “'” nb-single-text(n,c) “'”
    *   [121]  nb-single-text(n,c) ::= c = flow-out  ⇒ nb-single-multi-line(n)
    *                                  c = flow-in   ⇒ nb-single-multi-line(n)
    *                                  c = block-key ⇒ nb-single-one-line
    *                                  c = flow-key  ⇒ nb-single-one-line
    *   [122]   nb-single-one-line ::= nb-single-char*
    *   [118]       nb-single-char ::= c-quoted-quote | ( nb-json - “'” )
    * }}}
    *
    * @param n a number of indentation spaces
    * @param c [[FlowIn]], [[FlowOut]], [[BlockKey]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''c-single-quoted(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#c-single-quoted(n,c)]]
    * @see [[http://yaml.org/spec/1.2/spec.html#nb-single-one-line]]
    * @see [[http://yaml.org/spec/1.2/spec.html#nb-single-char]]
    */
  private[lexers] def singleQuoted(n: Int, c: Context): Parser[String] = {
    def singleChar: Parser[String] = QuotedQuote | "[\\x09\\x20-\\x26\\x28-\\x{10FFFF}]".r
    def singleOneLine: Parser[String] = singleChar.* ^^ { _.mkString }
    
    "'" ~> {
      c match {
        case _@(FlowOut | FlowIn) => singleMultiLine(n)
        case _@(BlockKey | FlowKey) => singleOneLine
        case _ => throw new Exception("context not supported")
      }
    } <~ "'" 
  }
  
  /** All leading and trailing white space characters are excluded from the content. Each continuation 
    * line must therefore contain at least one non-space character. Empty lines, if any, are consumed 
    * as part of the line folding.
    * 
    * {{{
    *   [123]    nb-ns-single-in-line ::= ( s-white* ns-single-char )*
    *   [124]   s-single-next-line(n) ::= s-flow-folded(n)
    *                                     ( ns-single-char nb-ns-single-in-line
    *                                       ( s-single-next-line(n) | s-white* ) )?
    *   [125] nb-single-multi-line(n) ::= nb-ns-single-in-line
    *                                     ( s-single-next-line(n) | s-white* )
    * }}}
    *
    * @param n a number of indentation spaces
    * @return [[Parser]] for lexing '''nb-single-multi-line(n)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#nb-single-multi-line(n)]]
    * @see [[http://yaml.org/spec/1.2/spec.html#s-single-next-line(n)]]
    * @see [[http://yaml.org/spec/1.2/spec.html#nb-ns-single-in-line]]
    */
  private[lexers] def singleMultiLine(n: Int): Parser[String] = {
    def singleInLine: Parser[String] = (s"[$White]*".r ~ singleCharNoSpace).* ^^
      { _.map{ case w ~ c => w.mkString + c }.mkString }
    
    def singleNextLine(n: Int): Parser[String] =
    flowFolded(n) ~ (singleCharNoSpace ~ singleInLine ~ (singleNextLine(n) | s"[$White]*".r)).? ^^ {
      case a ~ Some(b ~ c ~ d) => a + b + c + d
      case _ ~ None => ""
    }
    
    singleInLine ~ (singleNextLine(n) | s"[$White]*".r) ^^ { case a ~ b => a + b }
  }
  
  /** Plain scalars must not begin with most indicators, as this would cause ambiguity with other YAML 
    * constructs. However, the “:”, “?” and “-” indicators may be used as the first character if followed 
    * by a non-space “safe” character, as this causes no ambiguity.
    * 
    * {{{
    *   [126] ns-plain-first(c) ::=  ( ns-char - c-indicator )
    *                              | ( ( “?” | “:” | “-” )
    *                                  /* Followed by an ns-plain-safe(c)) */ )
    * }}}
    * 
    * @param c [[FlowOut]], [[BlockKey]], [[FlowIn]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''ns-plain-first(c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-plain-first(c)]]
    */
  private[lexers] def plainFirst(c: Context): Parser[String] = Parser { input =>
    parse("[\\x24\\x28\\x29\\x2B\\x2E-\\x39\\x3B-\\x3D\\x41-\\x5A\\x5C\\x5E-\\x5F\\x61-\\x7A\\x7E\\x85\\xA0-\\x{D7FF}\\x{E000}-\\x{FEFE}\\x{FF00}-\\x{FFFD}\\x{10000}-\\x{10FFFF}]".r, input) match {
      case NoSuccess(_, _) => parse("[\\x3F\\x3A\\x2D]".r <~ plainSafe(c, backtracking = true), input) match {
        case NoSuccess(m, next) => Failure(m, next)
        case Success(y, next) => Success(y, next)
      }
      case Success(y, next) => Success(y, next)
    }
  }
  
  /** Plain scalars must never contain the “: ” and “ #” character combinations. Such combinations would 
    * cause ambiguity with mapping key: value pairs and comments. In addition, inside flow collections, 
    * or when used as implicit keys, plain scalars must not contain the “[”, “]”, “{”, “}” and “,” 
    * characters. These characters would cause ambiguity with flow collection structures.
    * 
    * {{{
    *   [127]  ns-plain-safe(c) ::= c = flow-out  ⇒ ns-plain-safe-out
    *                               c = flow-in   ⇒ ns-plain-safe-in
    *                               c = block-key ⇒ ns-plain-safe-out
    *                               c = flow-key  ⇒ ns-plain-safe-in
    *   [128] ns-plain-safe-out ::= ns-char
    *   [129]  ns-plain-safe-in ::= ns-char - c-flow-indicator
    * }}}
    * 
    * @param c [[FlowOut]], [[BlockKey]], [[FlowIn]] or [[FlowKey]]
    * @param backtracking whether to backtrack or not, false is the default
    * @return [[Parser]] for lexing '''ns-plain-safe(c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-plain-safe(c)]]
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-plain-safe-out]]
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-plain-safe-in]]
    */
  private[lexers] def plainSafe(c: Context, backtracking: Boolean = false): Parser[String] = Parser { input =>
    val PlainSafeOutRegex = s"[$NoSpaceChar]".r
    val PlainSafeInRegex = "[\\x09\\x21-\\x2B\\x2D-\\x5A\\x5C\\x5E-\\x7A\\x7C\\x7E\\x85\\xA0-\\x{D7FF}\\x{E000}-\\x{FEFE}\\x{FF00}-\\x{FFFD}\\x{10000}-\\x{10FFFF}]".r
    
    val source = input.source
    val subSequence = source.subSequence(input.offset, source.length)
    
    (c match {
      case _@(FlowOut | BlockKey) => PlainSafeOutRegex
      case _@(FlowIn | FlowKey) => PlainSafeInRegex
      case _ => throw new Exception("context not supported")
    }) findPrefixMatchOf subSequence match {
      case Some(m) => Success(m.source.subSequence(m.start, m.end).toString, if (backtracking) input else input.drop(m.end))
      case _ => Failure("Not found ns-plain-safe(c)", input)
    }
  }
  
  /** 
    *
    * {{{
    *   [130]  ns-plain-char(c) ::=  ( ns-plain-safe(c) - “:” - “#” )
    *                              | ( /* An ns-char preceding */ “#” )
    *                              | ( “:” /* Followed by an ns-plain-safe(c) */ )
    * }}}
    * 
    * @param c [[FlowOut]], [[BlockKey]], [[FlowIn]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''ns-plain-char(c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-plain-char(c)]]
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-plain-safe(c)]]
    */
  private[lexers] def plainChar(c: Context): Parser[String] = Parser { input =>
    val PlainSafeOutRegex = "[\\x09\\x21\\x22\\x24-\\x39\\x3B-\\x7E\\x85\\xA0-\\x{D7FF}\\x{E000}-\\x{FEFE}\\x{FF00}-\\x{FFFD}\\x{10000}-\\x{10FFFF}]".r
    val PlainSafeInRegex = "[\\x09\\x21\\x22\\x24-\\x2B\\x2D-\\x39\\x3B-\\x5A\\x5C\\x5E-\\x7A\\x7C\\x7E\\x85\\xA0-\\x{D7FF}\\x{E000}-\\x{FEFE}\\x{FF00}-\\x{FFFD}\\x{10000}-\\x{10FFFF}]".r

    def plainSafeForChar: Parser[String] = c match {
      case _@(FlowOut | BlockKey) => PlainSafeOutRegex
      case _@(FlowIn | FlowKey) => PlainSafeInRegex
      case _ => throw new Exception("context not supported")
    }
    
    def noSpaceCharPrecedingHash: Parser[String] = Parser { input =>
      val source = input.source
      val subSequence = source.subSequence(input.offset - 1, if (source.length > input.offset + 1) input.offset + 1 else input.offset)
      
      s"[$NoSpaceChar]\\x23".r findPrefixMatchOf subSequence match {
        case Some(_) => Success("#", input.drop(1))
        case _ => Failure("Not found ns-char preceding", input)
      }
    }
    
    def colonFollowedByPlainSafe: Parser[String] = "\\x3A".r <~ plainSafe(c, backtracking = true)
    
    parse(plainSafeForChar | noSpaceCharPrecedingHash | colonFollowedByPlainSafe, input)
  }

  /** Plain scalars are further restricted to a single line when contained inside an implicit key.
    * 
    * {{{
    *   [131]          ns-plain(n,c) ::= c = flow-out  ⇒ ns-plain-multi-line(n,c)
    *                                    c = flow-in   ⇒ ns-plain-multi-line(n,c)
    *                                    c = block-key ⇒ ns-plain-one-line(c)
    *                                    c = flow-key  ⇒ ns-plain-one-line(c)
    *   [132] nb-ns-plain-in-line(c) ::= ( s-white* ns-plain-char(c) )*
    *   [133]   ns-plain-one-line(c) ::= ns-plain-first(c) nb-ns-plain-in-line(c)
    * }}}
    * 
    * All leading and trailing white space characters are excluded from the content. Each continuation 
    * line must therefore contain at least one non-space character. Empty lines, if any, are consumed 
    * as part of the line folding.
    * 
    * {{{
    *   [134] s-ns-plain-next-line(n,c) ::= s-flow-folded(n)
    *                                       ns-plain-char(c) nb-ns-plain-in-line(c)
    *   [135]  ns-plain-multi-line(n,c) ::= ns-plain-one-line(c)
    *                                       s-ns-plain-next-line(n,c)*
    * }}}
    * 
    * @param n a number of indentation spaces
    * @param c [[FlowIn]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''ns-plain'''
    * @see [[http://yaml.org/spec/1.2/spec.html#s-ns-plain-next-line(n,c)]]
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-plain-multi-line(n,c)]]
    */
  private[lexers] def plain(n: Int, c: Context): Parser[String] = {
    def plainInLine(c: Context): Parser[String] = 
      (s"[$White]*".r ~ plainChar(c)).* ^^ { _.map{ case a ~ b => a + b }.mkString }
    
    def plainOneLine(c: Context): Parser[String] = 
      plainFirst(c) ~ plainInLine(c) ^^ { case a ~ b => a + b }
    
    def plainNextLine(n: Int, c: Context): Parser[String] = 
      flowFolded(n) ~ plainChar(c) ~ plainInLine(c) ^^ { case x ~ y ~ z => x + y + z }
    
    def plainMultiLine(n: Int, c: Context): Parser[String] = 
      plainOneLine(c) ~ plainNextLine(n, c).* ^^ { case x ~ y => x + y.mkString }
    
    c match {
      case FlowOut | FlowIn => plainMultiLine(n, c)
      case BlockKey | FlowKey => plainOneLine(c)
      case _ => throw new Exception("context not supported") // TODO definition of exception
    }
  }

  import tokens.FlowNodeToken
  import tokens.FlowEntryToken
  import tokens.FlowSequenceToken
  import tokens.FlowMappingToken
  import tokens.ScalarToken
  import tokens.SingleQuoted
  import tokens.DoubleQuoted
  import tokens.Plain

  /** A flow collection may be nested within a block collection (flow-out context), nested within another 
    * flow collection (flow-in context), or be a part of an implicit key (flow-key context or block-key 
    * context). Flow collection entries are terminated by the “,” indicator. The final “,” may be omitted. 
    * This does not cause ambiguity because flow collection entries can never be completely empty.
    * 
    * {{{
    *   [136] in-flow(c) ::= c = flow-out  ⇒ flow-in
    *                        c = flow-in   ⇒ flow-in
    *                        c = block-key ⇒ flow-key
    *                        c = flow-key  ⇒ flow-key
    * }}}
    * 
    * @param c [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''in-flow(c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#in-flow(c)]]
    */
  private[lexers] def flow(c: Context): Context = c match {
    case FlowOut | FlowIn => FlowIn
    case BlockKey | FlowKey => FlowKey
    case _ => throw new Exception("Not allowed context")
  }

  /** Flow sequence content is denoted by surrounding “[” and “]” characters.
    * 
    * {{{
    *   [137] c-flow-sequence(n,c) ::= “[” s-separate(n,c)?
    *                                  ns-s-flow-seq-entries(n,in-flow(c))? “]”
    * }}}
    * 
    * @param n a number of indentation spaces
    * @param c [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''c-flow-sequence(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#c-flow-sequence(n,c)]]
    */
  private[lexers] def flowSequence(n: Int, c: Context): Parser[FlowSequenceToken] =
    "[" ~> separate(n, c).? ~> flowSeqEntries(n, flow(c)) <~ "]" ^^ { FlowSequenceToken }
  
  /** Sequence entries are separated by a “,” character.
    * 
    * {{{
    *   [138] ns-s-flow-seq-entries(n,c) ::= ns-flow-seq-entry(n,c) s-separate(n,c)?
    *                                        ( “,” s-separate(n,c)?
    *                                          ns-s-flow-seq-entries(n,c)? )?
    * }}}
    * 
    * @param n a number of indentation spaces
    * @param c [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''ns-s-flow-seq-entries(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#c-flow-seq-entries(n,c)]]
    */
  private[lexers] def flowSeqEntries(n: Int, c: Context): Parser[List[FlowNodeToken]] =
    (flowSeqEntry(n, c) <~ separate(n, c).?) ~ ("," ~> separate(n, c).? ~> flowSeqEntries(n, c).?).? ^^ {
      case entry ~ None => List(entry)
      case entry ~ Some(None) => List(entry)
      case entry ~ Some(Some(entries)) => entry +: entries
    }

  /** Any flow node may be used as a flow sequence entry. In addition, YAML provides a compact notation 
    * for the case where a flow sequence entry is a mapping with a single key: value pair.
    * 
    * {{{
    *   [139] ns-flow-seq-entry(n,c) ::= ns-flow-pair(n,c) | ns-flow-node(n,c)
    * }}}
    * 
    * @param n a number of indentation spaces
    * @param c [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''ns-flow-seq-entry(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-flow-seq-entry(n,c)]]
    */
  private[lexers] def flowSeqEntry(n: Int, c: Context): Parser[FlowNodeToken] = flowPair(n, c) | flowNode(n, c)
  
  /** Flow mappings are denoted by surrounding “{” and “}” characters.
    * 
    * {{{
    *   [140] c-flow-mapping(n,c) ::= “{” s-separate(n,c)?
    *                                 ns-s-flow-map-entries(n,in-flow(c))? “}”
    * }}}
    * 
    * @param n a number of indentation spaces
    * @param c [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''c-flow-mapping(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#c-flow-mapping(n,c)]]
    */
  private[lexers] def flowMapping(n: Int, c: Context): Parser[FlowMappingToken] =
    "{" ~> separate(n, c).? ~> flowMapEntries(n, flow(c)) <~ "}" ^^ { FlowMappingToken }
  
  /** Mapping entries are separated by a “,” character.
    * 
    * {{{
    *   [141] ns-s-flow-map-entries(n,c) ::= ns-flow-map-entry(n,c) s-separate(n,c)?
    *                                        ( “,” s-separate(n,c)?
    *                                          ns-s-flow-map-entries(n,c)? )?
    * }}}
    * 
    * @param n a number of indentation spaces
    * @param c [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''ns-s-flow-map-entries(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-s-flow-map-entries(n,c)]]
    */
  private[lexers] def flowMapEntries(n: Int, c: Context): Parser[List[FlowEntryToken]] =
    (flowMapEntry(n, c) <~ separate(n, c).?) ~ ("," ~> separate(n, c).? ~> flowMapEntries(n, c).?).? ^^ {
      case entry ~ None => List(entry)
      case entry ~ Some(None) => List(entry)
      case entry ~ Some(Some(entries)) => entry +: entries
    }

  /** The entry inside flow mapping is either explicit or implicit.
    * 
    * {{{
    *   [142] ns-flow-map-entry(n,c) ::=  ( “?” s-separate(n,c)
    *                                       ns-flow-map-explicit-entry(n,c) )
    *                                   | ns-flow-map-implicit-entry(n,c)
    * }}}
    * 
    * @param n a number of indentation spaces
    * @param c [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''ns-flow-map-entry(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-flow-map-entry(n,c)]]
    */
  private[lexers] def flowMapEntry(n: Int, c: Context): Parser[FlowEntryToken] =
    flowMapExplicitEntry(n, c) | flowMapImplicitEntry(n, c)

  /** If the optional “?” mapping key indicator is specified, the rest of the entry may be completely empty.
    * 
    * {{{
    *   [143] ns-flow-map-explicit-entry(n,c) ::=  ns-flow-map-implicit-entry(n,c)
    *                                            | ( e-node /* Key */
    *                                                e-node /* Value */ )
    * }}}
    * 
    * @param n a number of indentation spaces
    * @param c [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''ns-flow-map-explicit-entry(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-flow-map-explicit-entry(n,c)]]
    */
  private[lexers] def flowMapExplicitEntry(n: Int, c: Context): Parser[FlowEntryToken] =
    "?" ~> separate(n, c) ~>
      (flowMapImplicitEntry(n, c) |
        (emptyNode ~ emptyNode) ^^ { case key ~ value => FlowEntryToken(key, value) })

  /** In the implicit entry of the flow mapping, the key and value must be separated by “:”. Both of the
    * key and value can be empty, however they must be separated by “:”.
    *
    * {{{
    *   [144]   ns-flow-map-implicit-entry(n,c) ::=   ns-flow-map-yaml-key-entry(n,c)
    *                                               | c-ns-flow-map-empty-key-entry(n,c)
    *                                               | c-ns-flow-map-json-key-entry(n,c)
    *   [145]   ns-flow-map-yaml-key-entry(n,c) ::= ns-flow-yaml-node(n,c)
    *                                               ( ( s-separate(n,c)?
    *                                                   c-ns-flow-map-separate-value(n,c) )
    *                                               | e-node )
    *   [148] c-ns-flow-map-json-key-entry(n,c) ::= c-flow-json-node(n,c)
    *                                               ( ( s-separate(n,c)?
    *                                                   c-ns-flow-map-adjacent-value(n,c) )
    *                                               | e-node )
    * }}}
    *
    * @param n a number of indentation spaces
    * @param c [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''ns-flow-map-implicit-entry(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-flow-map-implicit-entry(n,c)]]
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-flow-map-yaml-key-entry(n,c)]]
    * @see [[http://yaml.org/spec/1.2/spec.html#c-ns-flow-map-json-key-entry(n,c)]]
    */
  private[lexers] def flowMapImplicitEntry(n: Int, c: Context): Parser[FlowEntryToken] = {
    def flowMapYamlKeyEntry(n: Int, c: Context): Parser[FlowEntryToken] =
      (flowNode(n, c, FlowYaml) ~ ((separate(n, c).? ~> flowMapSeparateValue(n, c)) | emptyNode)) ^^ {
        case key ~ value => FlowEntryToken(key, value)
      }

    def flowMapJsonKeyEntry(n: Int, c: Context): Parser[FlowEntryToken] =
      flowNode(n, c, FlowJson) ~ ((separate(n, c).? ~> flowMapAdjacentValue(n, c)) | emptyNode) ^^ {
        case key ~ value => FlowEntryToken(key, value)
      }

    flowMapYamlKeyEntry(n, c) | flowMapEmptyKeyEntry(n, c) | flowMapJsonKeyEntry(n, c)
  }

  /** YAML allows that a key of the entry inside flow mapping is the empty node.
    *
    * {{{
    *   [146] c-ns-flow-map-empty-key-entry(n,c) ::= e-node /* Key */
    *                                                c-ns-flow-map-separate-value(n,c)
    * }}}
    *
    * @param n a number of indentation spaces
    * @param c [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''c-ns-flow-map-empty-key-entry(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#c-ns-flow-map-empty-key-entry(n,c)]]
    */
  private[lexers] def flowMapEmptyKeyEntry(n: Int, c: Context): Parser[FlowEntryToken] =
    emptyNode ~ flowMapSeparateValue(n, c) ^^ { case key ~ value => FlowEntryToken(key, value) }

  /** Normally, YAML insists the “:” mapping value indicator be separated from the value by white space.
    * A benefit of this restriction is that the “:” character can be used inside plain scalars,
    * as long as it is not followed by white space. This allows for unquoted URLs and timestamps.
    * It is also a potential source for confusion as “a:1” is a plain scalar and not a key: value pair.
    *
    * Note that the value may be completely empty since its existence is indicated by the “:”.
    *
    * {{{
    *   [147] c-ns-flow-map-separate-value(n,c) ::= “:” /* Not followed by an
    *                                                      ns-plain-safe(c) */
    *                                               ( ( s-separate(n,c) ns-flow-node(n,c) )
    *                                               | e-node /* Value */ )
    * }}}
    *
    * @param n a number of indentation spaces
    * @param c [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''c-ns-flow-map-separate-value(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#c-ns-flow-map-separate-value(n,c)]]
    */
  private[lexers] def flowMapSeparateValue(n: Int, c: Context): Parser[FlowNodeToken] =
    ":" ~> ((separate(n, c) ~> flowNode(n, c)) | emptyNode)

  /** To ensure JSON compatibility, if a key inside a flow mapping is JSON-like, YAML allows
    * the following value to be specified adjacent to the “:”. This causes no ambiguity, as all JSON-like
    * keys are surrounded by indicators. However, as this greatly reduces readability, YAML processors
    * should separate the value from the “:” on output, even in this case.
    *
    * {{{
    *   [149] c-ns-flow-map-adjacent-value(n,c) ::= “:” ( ( s-separate(n,c)?
    *                                                       ns-flow-node(n,c) )
    *                                                   | e-node ) /* Value */
    * }}}
    *
    * @param n a number of indentation spaces
    * @param c [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''c-ns-flow-map-adjacent-value(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#c-ns-flow-map-adjacent-value(n,c)]]
    */
  private[lexers] def flowMapAdjacentValue(n: Int, c: Context): Parser[FlowNodeToken] =
    ":" ~> ((separate(n, c).? ~> flowNode(n, c)) | emptyNode)

  /** If the “?” indicator is explicitly specified, parsing is unambiguous, and the syntax is identical
    * to the general case.
    *
    * {{{
    *   [150] ns-flow-pair(n,c) ::=  ( “?” s-separate(n,c)
    *                                  ns-flow-map-explicit-entry(n,c) )
    *                              | ns-flow-pair-entry(n,c)
    * }}}
    *
    * @param n a number of indentation spaces
    * @param c [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''ns-flow-pair(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-flow-pair(n,c)]]
    */
  private[lexers] def flowPair(n: Int, c: Context): Parser[FlowEntryToken] =
    flowMapExplicitEntry(n, c) | flowPairEntry(n, c)

  /** If the “?” indicator is omitted, parsing needs to see past the implicit key to recognize it as
    * such. To limit the amount of lookahead required, the “:” indicator must appear at most 1024 Unicode
    * characters beyond the start of the key. In addition, the key is restricted to a single line.
    *
    * {{{
    *   [151]            ns-flow-pair-entry(n,c) ::=  ns-flow-pair-yaml-key-entry(n,c)
    *                                               | c-ns-flow-map-empty-key-entry(n,c)
    *                                               | c-ns-flow-pair-json-key-entry(n,c)
    *   [152]   ns-flow-pair-yaml-key-entry(n,c) ::= ns-s-implicit-yaml-key(flow-key)
    *                                                c-ns-flow-map-separate-value(n,c)
    *   [153] c-ns-flow-pair-json-key-entry(n,c) ::= c-s-implicit-json-key(flow-key)
    *                                                c-ns-flow-map-adjacent-value(n,c)
    * }}}
    *
    * @param n a number of indentation spaces
    * @param c [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @return [[Parser]] for lexing '''ns-flow-pair-entry(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-flow-pair-entry(n,c)]]
    */
  private[lexers] def flowPairEntry(n: Int, c: Context): Parser[FlowEntryToken] = {
    def flowPairYamlKeyEntry(n: Int, c: Context): Parser[FlowEntryToken] =
      implicitKey(FlowKey, FlowYaml) ~ flowMapSeparateValue(n, c) ^^ {
        case key ~ value => FlowEntryToken(key, value)
      }

    def flowPairJsonKeyEntry(n: Int, c: Context): Parser[FlowEntryToken] =
      implicitKey(FlowKey, FlowJson) ~ flowMapAdjacentValue(n, c) ^^ {
        case key ~ value => FlowEntryToken(key, value)
      }

    flowPairYamlKeyEntry(n, c) | flowMapEmptyKeyEntry(n, c) | flowPairJsonKeyEntry(n, c)
  }

  sealed trait FlowNodesType

  private[lexers] case object FlowYaml extends FlowNodesType
  private[lexers] case object FlowJson extends FlowNodesType
  private[lexers] case object FlowEither extends FlowNodesType

  /** Note that YAML allows arbitrary nodes to be used as keys. In particular, a key may be a sequence
    * or a mapping. Thus, without the above restrictions, practical one-pass parsing would have been
    * impossible to implement.
    *
    * {{{
    *   [154] ns-s-implicit-yaml-key(c) ::= ns-flow-yaml-node(n/a,c) s-separate-in-line?
    *                                       /* At most 1024 characters altogether */
    *   [155]  c-s-implicit-json-key(c) ::= c-flow-json-node(n/a,c) s-separate-in-line?
    *                                       /* At most 1024 characters altogether */
    * }}}
    *
    * @param c [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @param t [[FlowYaml]], [[FlowJson]] or [[FlowEither]]
    * @return [[Parser]] for lexing '''ns-s-implicit-yaml-key(c)''' or '''c-s-implicit-json-key(c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-s-implicit-yaml-key(c)]]
    * @see [[http://yaml.org/spec/1.2/spec.html#c-s-implicit-json-key(c)]]
    */
  private[lexers] def implicitKey(c: Context, t: FlowNodesType): Parser[FlowNodeToken] = Parser { input =>
    t match {
      case FlowYaml | FlowJson => parse(flowNode(0, c, t) <~ separateInLine.?, input) match {
        case NoSuccess(m, next) => Failure(m, next)
        case Success(_, next) if next.offset - input.offset >= 1024 => Error("Some key is too long. The length of the key is limited to 1024 at most.", input)
        case Success(y, next) => Success(y, next)
      }
      case _ => throw new Exception()
    }
  }

  /** JSON-like flow styles all have explicit start and end indicators. The only flow style that does not
    * have this property is the plain scalar. Note that none of the “JSON-like” styles is actually
    * acceptable by JSON. Even the double-quoted style is a superset of the JSON string format.
    *
    * {{{
    *   [156] ns-flow-yaml-content(n,c) ::= ns-plain(n,c)
    *   [157]  c-flow-json-content(n,c) ::=   c-flow-sequence(n,c) | c-flow-mapping(n,c)
    *                                       | c-single-quoted(n,c) | c-double-quoted(n,c)
    *   [158]      ns-flow-content(n,c) ::= ns-flow-yaml-content(n,c) | c-flow-json-content(n,c)
    * }}}
    *
    * @param n a number of indentation spaces
    * @param c [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @param t [[FlowYaml]], [[FlowJson]] or [[FlowEither]]
    * @return [[Parser]] for lexing '''ns-flow-yaml-content(n,c)''', '''c-flow-json-content(n,c)''' or
    *        '''s-flow-content(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-flow-content(n,c)]]
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-flow-yaml-content(n,c)]]
    * @see [[http://yaml.org/spec/1.2/spec.html#c-flow-json-content(n,c)]]
    */
  private[lexers] def flowContent(n: Int, c: Context, t: FlowNodesType = FlowEither): Parser[FlowNodeToken] = {
    def flowYamlContent(n: Int, c: Context): Parser[FlowNodeToken] = plain(n, c) ^^ { x => ScalarToken(x, Plain) }

    def flowJsonContent(n: Int, c: Context): Parser[FlowNodeToken] =
      flowSequence(n, c) |
        flowMapping(n, c) |
        singleQuoted(n, c) ^^ { x => ScalarToken(x, SingleQuoted) } |
        doubleQuoted(n, c) ^^ { x => ScalarToken(x, DoubleQuoted) }

    t match {
      case FlowYaml => flowYamlContent(n, c)
      case FlowJson => flowJsonContent(n, c)
      case FlowEither => flowYamlContent(n, c) | flowJsonContent(n, c)
    }
  }

  /** A complete flow node also has optional node properties, except for alias nodes which refer to the
    * anchored node properties.
    *
    * {{{
    *   [159] ns-flow-yaml-node(n,c) ::=   c-ns-alias-node
    *                                    | ns-flow-yaml-content(n,c)
    *                                    | ( c-ns-properties(n,c)
    *                                        ( ( s-separate(n,c)
    *                                            ns-flow-yaml-content(n,c) )
    *                                          | e-scalar ) )
    *   [160]  c-flow-json-node(n,c) ::= ( c-ns-properties(n,c) s-separate(n,c) )?
    *                                    c-flow-json-content(n,c)
    *   [161]      ns-flow-node(n,c) ::=   c-ns-alias-node
    *                                    | ns-flow-content(n,c)
    *                                    | ( c-ns-properties(n,c)
    *                                        ( ( s-separate(n,c)
    *                                            ns-flow-content(n,c) )
    *                                          | e-scalar ) )
    * }}}
    *
    * @param n a number of indentation spaces
    * @param c [[FlowOut]], [[FlowIn]], [[BlockKey]] or [[FlowKey]]
    * @param t [[FlowYaml]], [[FlowJson]] or [[FlowEither]]
    * @return [[Parser]] for lexing '''ns-flow-yaml-node(n,c)''', '''c-flow-json-node(n,c)''' or
    *        '''ns-flow-node(n,c)'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-flow-node(n,c)]]
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-flow-yaml-node(n,c)]]
    * @see [[http://yaml.org/spec/1.2/spec.html#c-flow-json-node(n,c)]]
    */
  private[lexers] def flowNode(n: Int, c: Context, t: FlowNodesType = FlowEither): Parser[FlowNodeToken] = {
    def flowYamlOrGeneralNode(n: Int, c: Context, t: FlowNodesType): Parser[FlowNodeToken] =
      aliasNode |
        flowContent(n, c, t) |
        (nodeProperties(n, c) ~ ((separate(n, c) ~> flowContent(n, c, t)) | emptyScalar)) ^^ {
          case property ~ node => node + Some(property)
        }

    def flowJsonNode(n: Int, c: Context): Parser[FlowNodeToken] =
      ((nodeProperties(n, c) <~ separate(n, c)).? ~ flowContent(n, c, FlowJson)) ^^ {
        case property ~ node => node + property
      }

    t match {
      case FlowYaml => flowYamlOrGeneralNode(n, c, FlowYaml)
      case FlowJson => flowJsonNode(n, c)
      case FlowEither => flowYamlOrGeneralNode(n, c, FlowEither)
    }
  }
}
