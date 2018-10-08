package yams
package lexers

/** Each node may have two optional properties, anchor and tag, in addition to its content.
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://www.yaml.org/spec/1.2/spec.html#id2783797]]
  */
trait NodePropertyLexer extends scala.util.parsing.combinator.RegexParsers
                           with DirectiveLexer
                           with SeparationLinesLexer
                           with com.typesafe.scalalogging.LazyLogging {
  import tokens.NodePropertyToken

  /** Anchor and tag may be specified in any order before the node's content. Either or both may be omitted.
    *
    * {{{
    *   [96] c-ns-properties(n,c) ::=  ( c-ns-tag-property ( s-separate(n,c) c-ns-anchor-property )? )
    *                                | ( c-ns-anchor-property ( s-separate(n,c) c-ns-tag-property )? )
    * }}}
    *
    * @return [[Parser]] for lexing '''c-ns-properties(n,c)'''
    * @see [[http://www.yaml.org/spec/1.2/spec.html#c-ns-properties(n,c)]]
    */
  private[yams] def nodeProperties(n: Int, c: Context): Parser[NodePropertyToken] =
    tagProperty ~ (separate(n, c) ~> anchorProperty).? ^^ { case tag ~ anchor => NodePropertyToken(Some(tag), anchor) } |
    anchorProperty ~ (separate(n, c) ~> tagProperty).? ^^ { case anchor ~ tag => NodePropertyToken(tag, Some(anchor)) }

  import tokens.TagToken

  /** The tag property identifies the type of the native data structure presented by the node. A tag is
    * denoted by the “!” indicator.
    *
    * {{{
    *   [97] c-ns-tag-property ::=  c-verbatim-tag
    *                             | c-ns-shorthand-tag
    *                             | c-non-specific-tag
    * }}}
    *
    * @return [[Parser]] for lexing '''c-ns-tag-property'''
    * @see [[http://www.yaml.org/spec/1.2/spec.html#c-ns-tag-property]]
    * @see [[http://yaml.org/spec/1.2/spec.html#id2782090]]
    */
  private[lexers] def tagProperty: Parser[TagToken] = verbatimTag | shorthandTag | nonSpecificTag
  
  implicit class TagString(val s: String) {
    private val LocalTagTester = "^(?:!|[^,\\[\\]\\{\\}])(?:%[0-9a-f]{2}|[0-9a-z\\-#;\\/\\?:@&=\\+\\$,_\\.!~\\*'\\(\\)\\[\\]])*$".r

    /** This regular expression has been borrowed from 
      * [[http://jmrware.com/articles/2009/uri_regexp/URI_regex.html]] written by Jeff Roberson.
      * 
      * Thanks to Jeff, we could test global tags easily. 
      */
    private val GlobalTagTester = "[A-Za-z][A-Za-z0-9+\\-.]*:(?:\\/\\/(?:(?:[A-Za-z0-9\\-._~!$&'()*+,;=:]|%[0-9A-Fa-f]{2})*@)?(?:\\[(?:(?:(?:(?:[0-9A-Fa-f]{1,4}:){6}|::(?:[0-9A-Fa-f]{1,4}:){5}|(?:[0-9A-Fa-f]{1,4})?::(?:[0-9A-Fa-f]{1,4}:){4}|(?:(?:[0-9A-Fa-f]{1,4}:){0,1}[0-9A-Fa-f]{1,4})?::(?:[0-9A-Fa-f]{1,4}:){3}|(?:(?:[0-9A-Fa-f]{1,4}:){0,2}[0-9A-Fa-f]{1,4})?::(?:[0-9A-Fa-f]{1,4}:){2}|(?:(?:[0-9A-Fa-f]{1,4}:){0,3}[0-9A-Fa-f]{1,4})?::[0-9A-Fa-f]{1,4}:|(?:(?:[0-9A-Fa-f]{1,4}:){0,4}[0-9A-Fa-f]{1,4})?::)(?:[0-9A-Fa-f]{1,4}:[0-9A-Fa-f]{1,4}|(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?))|(?:(?:[0-9A-Fa-f]{1,4}:){0,5}[0-9A-Fa-f]{1,4})?::[0-9A-Fa-f]{1,4}|(?:(?:[0-9A-Fa-f]{1,4}:){0,6}[0-9A-Fa-f]{1,4})?::)|[Vv][0-9A-Fa-f]+\\.[A-Za-z0-9\\-._~!$&'()*+,;=:]+)\\]|(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)|(?:[A-Za-z0-9\\-._~!$&'()*+,;=]|%[0-9A-Fa-f]{2})*)(?::[0-9]*)?(?:\\/(?:[A-Za-z0-9\\-._~!$&'()*+,;=:@]|%[0-9A-Fa-f]{2})*)*|\\/(?:(?:[A-Za-z0-9\\-._~!$&'()*+,;=:@]|%[0-9A-Fa-f]{2})+(?:\\/(?:[A-Za-z0-9\\-._~!$&'()*+,;=:@]|%[0-9A-Fa-f]{2})*)*)?|(?:[A-Za-z0-9\\-._~!$&'()*+,;=:@]|%[0-9A-Fa-f]{2})+(?:\\/(?:[A-Za-z0-9\\-._~!$&'()*+,;=:@]|%[0-9A-Fa-f]{2})*)*|)(?:\\?(?:[A-Za-z0-9\\-._~!$&'()*+,;=:@\\/?]|%[0-9A-Fa-f]{2})*)?(?:\\#(?:[A-Za-z0-9\\-._~!$&'()*+,;=:@\\/?]|%[0-9A-Fa-f]{2})*)?".r
    
    def isLocalTag: Boolean = s.length > 1 && s.startsWith("!") && LocalTagTester.matches(s)
    def isGlobalTag: Boolean = !s.startsWith("!") && GlobalTagTester.matches(s)
    def isValidTag: Boolean = s.isGlobalTag || s.isLocalTag
  }

  /** A tag may be written verbatim by surrounding it with the “<” and “>” characters. In this case,
    * the YAML processor must deliver the verbatim tag as-is to the application. In particular, verbatim
    * tags are not subject to tag resolution. A verbatim tag must either begin with a “!” (a local tag)
    * or be a valid URI (a global tag).
    *
    * {{{
    *   [98] c-verbatim-tag ::= “!” “<” ns-uri-char+ “>”
    * }}}
    *
    * @return [[Parser]] for lexing '''c-verbatim-tag'''
    * @see [[http://www.yaml.org/spec/1.2/spec.html#c-verbatim-tag]]
    */
  private def verbatimTag: Parser[TagToken] = Parser { input => 
    parse("!<" ~> s"[$UriChar]+".r <~ ">", input) match {
      case Success(y, next) if y.startsWith("!") && y.isLocalTag => Success(TagToken(None, Some(y)), next)
      case Success(y, next) if !y.startsWith("!") && y.isGlobalTag => Success(TagToken(None, Some(y)), next)
      case _ if input.source.length - input.offset >= 2 && input.source.subSequence(input.offset, input.offset + 2) == "!<" => 
        Error("Expected a c-verbatim-tag, but failed to resolve a verbatim tag.", input)
      case _ => Failure("Expected a c-verbatim-tag, but the other found.", input)
    }
  }
  
  /** A tag shorthand consists of a valid tag handle followed by a non-empty suffix. The tag handle must
    * be associated with a prefix, either by default or by using a “TAG” directive. The resulting parsed
    * tag is the concatenation of the prefix and the suffix, and must either begin with “!” (a local tag)
    * or be a valid URI (a global tag).
    *
    * The suffix must not contain any “!” character. This would cause the tag shorthand to be interpreted
    * as having a named tag handle. In addition, the suffix must not contain the “[”, “]”, “{”, “}”
    * and “,” characters. These characters would cause ambiguity with flow collection structures.
    * If the suffix needs to specify any of the above restricted characters, they must be escaped using
    * the “%” character. This behavior is consistent with the URI character escaping rules (specifically,
    * section 2.3 of RFC2396).
    *
    * {{{
    *   [99] c-ns-shorthand-tag ::= c-tag-handle ns-tag-char+
    * }}}
    *
    * @return [[Parser]] for lexing '''c-ns-shorthand-tag'''
    * @see [[http://www.yaml.org/spec/1.2/spec.html#c-ns-shorthand-tag]]
    * @see [[http://www.ietf.org/rfc/rfc2396.txt]]
    */
  private def shorthandTag: Parser[TagToken] = Parser { input =>
    parse(tagHandle, input) match {
      case Success(handle, next1) => parse(s"[$TagChar]+".r, next1) match {
        case Success(suffix, next2) if (handle + suffix).isValidTag => Success(TagToken(Some(handle), Some(suffix)), next2)
        case Success(_, _) => Error("A c-ns-shorthand-tag is founded, but it's a invalid tag.", input)
        case _ if handle == "!" => Failure("Failed to resolve shorthand tag(maybe it's a non-specific tag)", input)
        case _ => Error("Expected a c-ns-shorthand-tag, but failed to parse it.", input)
      }
      case _ => Failure("Failed to resolve shorthand tag(maybe it's not a tag property)", input)
    }
  }

  /** If a node has no tag property, it is assigned a non-specific tag that needs to be resolved to
    * a specific one. This non-specific tag is “!” for non-plain scalars and “?” for all other nodes.
    * This is the only case where the node style has any effect on the content information.
    *
    * It is possible for the tag property to be explicitly set to the “!” non-specific tag. By convention,
    * this “disables” tag resolution, forcing the node to be interpreted as “tag:yaml.org,2002:seq”,
    * “tag:yaml.org,2002:map”, or “tag:yaml.org,2002:str”, according to its kind.
    *
    * There is no way to explicitly specify the “?” non-specific tag. This is intentional.
    *
    * {{{
    *   [100] c-non-specific-tag ::= “!”
    * }}}
    *
    * @return [[Parser]] for lexing '''c-non-specific-tag'''
    * @see [[http://www.yaml.org/spec/1.2/spec.html#c-non-specific-tag]]
    */
  private def nonSpecificTag: Parser[TagToken] = "!" ^^ { x => TagToken(Some(x), None) }

  import tokens.AnchorToken

  /** An anchor is denoted by the “&” indicator. It marks a node for future reference. An alias node can
    * then be used to indicate additional inclusions of the anchored node. An anchored node need not be
    * referenced by any alias nodes; in particular, it is valid for all nodes to be anchored.
    *
    * {{{
    *   [101] c-ns-anchor-property ::= “&” ns-anchor-name
    * }}}
    *
    * @return [[Parser]] for lexing '''c-ns-anchor-property'''
    * @see [[http://www.yaml.org/spec/1.2/spec.html#c-ns-anchor-property]]
    */
  private[lexers] def anchorProperty: Parser[AnchorToken] = "&" ~> AnchorName.r ^^ { x => AnchorToken(tokens.Anchor(x)) }

  /** Anchor names must not contain the “[”, “]”, “{”, “}” and “,” characters. These characters would
    * cause ambiguity with flow collection structures.
    *
    * {{{
    *   [102] ns-anchor-char ::= ns-char - c-flow-indicator
    *   [103] ns-anchor-name ::= ns-anchor-char+
    * }}}
    *
    * @return Regular expression string for ns-anchor-name
    * @see [[http://www.yaml.org/spec/1.2/spec.html#ns-anchor-char]]
    * @see [[http://www.yaml.org/spec/1.2/spec.html#ns-anchor-name]]
    */
  private[lexers] val AnchorName = {
    val AnchorChar = "\\x21-\\x7E\\x85\\xA0-\\x{D7FF}\\x{E000}-\\x{FEFE}\\x{FF00}-\\x{FFFD}\\x{10000}-\\x{10FFFF}"
    s"[$AnchorChar]+"
  }
}
