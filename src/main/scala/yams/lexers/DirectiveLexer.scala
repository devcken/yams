package yams
package lexers

/** Directives are instructions to the YAML processor.
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2781147]]
  */
trait DirectiveLexer extends scala.util.parsing.combinator.RegexParsers
                        with CommentLexer
                        with characters.MiscChar {
  /** This specification defines two directives, “YAML” and “TAG”, and reserves all other directives for
    * future use. There is no way to define private directives. This is intentional.
    *
    * {{{
    *   [82]       l-directive ::= “%”
    *                              ( ns-yaml-directive
    *                              | ns-tag-directive
    *                              | ns-reserved-directive )
    *                              s-l-comments
    * }}}
    *
    * @return [[Parser]] for lexing '''l-directive'''
    * @see [[http://yaml.org/spec/1.2/spec.html#l-directive]]
    */
  private[lexers] def directive: Parser[tokens.DirectiveToken] =
    "%" ~> (yamlDirective | tagDirective | reservedDirective) <~ comments
  
  import tokens.ReservedDirectiveToken

  /** Each directive is specified on a separate non-indented line starting with the “%” indicator,
    * followed by the directive name and a list of parameters. The semantics of these parameters
    * depends on the specific directive. A YAML processor should ignore unknown directives with an
    * appropriate warning.
    *
    * {{{
    *   [83]  ns-reserved-directive ::= ns-directive-name
    *                                   ( s-separate-in-line ns-directive-parameter )*
    *   [84]      ns-directive-name ::= ns-char+
    *   [85] ns-directive-parameter ::= ns-char+
    * }}}
    * 
    * @return [[Parser]] for lexing '''ns-reserved-directive'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-reserved-directive]]
    */
  private def reservedDirective: Parser[ReservedDirectiveToken] = {
    val DirectiveName = s"[$NoSpaceChar]+"

    /** `ns-directive-name` is `ns-char+`, but in case of succeeding comments could NOT be found any
      * difference between `( s-separate-in-line ns-directive-parameter )` and
      * `(s-separate-in-line c-nb-comment-text?)?`.
      *
      * That is, `#` cannot be followed by `ns-directive-parameter`. So, `\^(?!#)` should be added to
      * this regular expression.
      */
    val DirectiveParameter = s"^(?!#)[$NoSpaceChar]+"

    DirectiveName.r ~ (separateInLine ~> DirectiveParameter.r).* ^^ { case name ~ param => ReservedDirectiveToken(name, param) }
  }

  /** The “YAML” directive specifies the version of YAML the document conforms to.
    * 
    * {{{
    *   [86] ns-yaml-directive ::= “Y” “A” “M” “L”
    *                              s-separate-in-line ns-yaml-version
    * }}}
    *
    * @return [[Parser]] for lexing '''ns-yaml-directive'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-yaml-directive]]
    */
  private def yamlDirective: Parser[tokens.YamlDirectiveToken] = Parser { input =>
    parse(("YAML" ~ separateInLine) ~> yamlVersion, input) match {
      case Success(v, next) => Success(tokens.YamlDirectiveToken(v), next)
      case Error(m, next) => Error(m, next)
      case _ => Failure("Not a YAML directive.", input)
    }
  }

  /** This specification defines version “1.2”, including recommendations for YAML 1.1 processing.
    *
    * {{{
    *   [87] ns-yaml-version ::= ns-dec-digit+ “.” ns-dec-digit+
    * }}}
    *
    * @return [[Parser]] for lexing '''ns-yaml-version'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-yaml-version]]
    */
  private def yamlVersion: Parser[Version] = Parser { input =>
    parse(s"[$DecimalDigit]+".r ~ ("." ~> s"[$DecimalDigit]+".r), input) match {
      case Success(major ~ minor, next) => Success(Version(major.toInt, minor.toInt), next)
      case NoSuccess(m, next) => Error(s"Failed to lex the YAML version(reason: $m).", next)
    }
  }

  /** The “TAG” directive establishes a tag shorthand notation for specifying node tags. Each “TAG”
    * directive associates a handle with a prefix. This allows for compact and readable tag notation.
    *
    * {{{
    *   [88] ns-tag-directive ::=  “T” “A” “G”
    *                              s-separate-in-line c-tag-handle
    *                              s-separate-in-line ns-tag-prefix
    * }}}
    *
    * @return [[Parser]] for lexing '''ns-tag-directive'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-tag-directive]]
    */
  private def tagDirective: Parser[tokens.TagDirectiveToken] =
    ("TAG" ~ separateInLine) ~> tagHandle ~ (separateInLine ~> tagPrefix) ^^ { x => tokens.TagDirectiveToken((x._1, x._2)) }

  /** The tag handle exactly matches the prefix of the affected tag shorthand. There are three tag
    * handle variants:
    *
    * {{{
    *   [89] c-tag-handle ::=  c-named-tag-handle
    *                        | c-secondary-tag-handle
    *                        | c-primary-tag-handle
    * }}}
    *
    * =Primary Handle=
    *
    * The primary tag handle is a single “!” character. This allows using the most compact possible
    * notation for a single “primary” name space. By default, the prefix associated with this handle is
    * “!”. Thus, by default, shorthands using this handle are interpreted as local tags.
    *
    * It is possible to override the default behavior by providing an explicit “TAG” directive,
    * associating a different prefix for this handle. This provides smooth migration from using local
    * tags to using global tags, by the simple addition of a single “TAG” directive.
    *
    * {{{
    *   [90] c-primary-tag-handle ::=  “!”
    * }}}
    *
    * =Secondary Handle=
    *
    * The secondary tag handle is written as “!!”. This allows using a compact notation for a single
    * “secondary” name space. By default, the prefix associated with this handle is “tag:yaml.org,2002:”.
    * This prefix is used by the YAML tag repository.
    *
    * It is possible to override this default behavior by providing an explicit “TAG” directive
    * associating a different prefix for this handle.
    *
    * {{{
    *   [91] c-secondary-tag-handle ::=  “!” “!”
    * }}}
    *
    * =Named Handles=
    *
    * A named tag handle surrounds a non-empty name with “!” characters. A handle name must not be used
    * in a tag shorthand unless an explicit “TAG” directive has associated some prefix with it.
    *
    * {{{
    *   [92] c-named-tag-handle ::=  “!” ns-word-char+ “!”
    * }}}
    *
    * @return [[Parser]] for lexing '''c-tag-handle'''
    * @see [[http://yaml.org/spec/1.2/spec.html#c-tag-handle]]
    */
  private[lexers] def tagHandle: Parser[String] = {
    val PrimaryTagHandle = "!"
    val SecondaryTagHandle = "!!"
    val NamedTagHandle = s"![$WordChar]+!"
    NamedTagHandle.r | SecondaryTagHandle | PrimaryTagHandle
  }

  /** There are two tag prefix variants:
    *
    * {{{
    *   [93] ns-tag-prefix ::= ;c-ns-local-tag-prefix | ns-global-tag-prefix
    * }}}
    *
    * =Local Tag Prefix=
    *
    * If the prefix begins with a “!” character, shorthands using the handle are expanded to a local tag.
    * Note that such a tag is intentionally not a valid URI, and its semantics are specific to the
    * application. In particular, two documents in the same stream may assign different semantics to the
    * same local tag.
    *
    * {{{
    *   [94] c-ns-local-tag-prefix ::= “!” ns-uri-char*
    * }}}
    *
    * =Global Tag Prefix=
    *
    * If the prefix begins with a character other than “!”, it must to be a valid URI prefix, and should
    * contain at least the scheme and the authority. Shorthands using the associated handle are expanded
    * to globally unique URI tags, and their semantics is consistent across applications. In particular,
    * every documents in every stream must assign the same semantics to the same global tag.
    *
    * {{{
    *   [95] ns-global-tag-prefix ::= ns-tag-char ns-uri-char*
    * }}}
    *
    * @return [[Parser]] for lexing '''ns-tag-prefix'''
    * @see [[http://yaml.org/spec/1.2/spec.html#ns-tag-prefix]]
    */
  private def tagPrefix: Parser[String] = {
    val LocalTagPrefix = s"![$UriChar]*"
    val GlobalTagPrefix = s"[$TagChar][$UriChar]*"
    // val LocalTagTester = "^!(?:%[\\da-fA-F]{2}|[a-zA-Z\\d-#;\\/?:@&=+$,_.!~*`\\(\\)\\[\\]])*$".r
    // val GlobalTagTester = "^(?:%[\\da-fA-F]{2}|[a-zA-Z\\d-#;\\/?:@&=+$_.~*`\\(\\)])(?:%[\\da-fA-F]{2}|[a-zA-Z\\d-#;\\/?:@&=+$,_.!~*`\\(\\)\\[\\]])*$".r
    LocalTagPrefix.r | GlobalTagPrefix.r
  }
}
