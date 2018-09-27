package yams
package lexers

/** A YAML character stream may contain several documents. Each document is completely independent from 
  * the rest.
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2800132]]
  */
trait DocumentLexer extends scala.util.parsing.combinator.RegexParsers
                       with CommentLexer
                       with BlockStylesLexer {
  /** A document may be preceded by a prefix specifying the character encoding, and optional comment 
    * lines. Note that all documents in a stream must use the same character encoding. However it is 
    * valid to re-specify the encoding using a byte order mark for each document in the stream. This 
    * makes it easier to concatenate streams.
    * 
    * The existence of the optional prefix does not necessarily indicate the existence of an actual 
    * document.
    * 
    * {{{
    *   [202] l-document-prefix ::= c-byte-order-mark? l-comment*
    * }}}
    * 
    * @return [[Parser]] for lexing '''l-document-prefix'''
    */
  private[lexers] def documentPrefix: Parser[List[Option[String]]] = commentLine.*

  /** Using directives creates a potential ambiguity. It is valid to have a “%” character at the start of
    * a line (e.g. as the first character of the second line of a plain scalar). How, then, to distinguish
    * between an actual directive and a content line that happens to start with a “%” character?
    *
    * The solution is the use of two special marker lines to control the processing of directives,
    * one at the start of a document and one at the end.
    *
    * At the start of a document, lines beginning with a “%” character are assumed to be directives.
    * The (possibly empty) list of directives is terminated by a directives end marker line. Lines
    * following this marker can safely use “%” as the first character.
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#c-directives-end]]
    */
  private val DirectivesEndMarker = "---".r

  /** At the end of a document, a document end marker line is used to signal the parser to begin scanning
    * for directives again.
    *
    * {{{
    *   [204] c-document-end ::= “.” “.” “.”
    * }}}
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#c-document-end]]
    */
  private[lexers] val DocumentEndMarker = "...".r

  /** The existence of this optional document suffix does not necessarily indicate the existence of
    * an actual following document.
    *
    * {{{
    *   [205] l-document-suffix ::= c-document-end s-l-comments
    * }}}
    *
    * @return [[Parser]] for lexing '''l-document-suffix'''
    * @see [[http://yaml.org/spec/1.2/spec.html#l-document-suffix]]
    */
  private[lexers] def documentSuffix: Parser[String] = DocumentEndMarker <~ comments
  
  import yams.tokens.DocumentToken

  /** A bare document does not begin with any directives or marker lines. Such documents are very “clean”
    * as they contain nothing other than the content. In this case, the first non-comment line may not
    * start with a “%” first character.
    *
    * Document nodes are indented as if they have a parent indented at -1 spaces. Since a node must be
    * more indented than its parent node, this allows the document’s node to be indented at zero or
    * more spaces.
    *
    * {{{
    *   [207] l-bare-document ::= s-l+block-node(-1,block-in)
    *                             /* Excluding c-forbidden content */
    * }}}
    *
    * @return [[Parser]] for lexing '''l-bare-document'''
    * @see [[http://yaml.org/spec/1.2/spec.html#l-bare-document]]
    */
  private[lexers] def bareDocument: Parser[DocumentToken] = blockNode(-1, BlockIn) ^^ { DocumentToken(_) }

  /** An explicit document begins with an explicit directives end marker line but no directives.
    * Since the existence of the document is indicated by this marker, the document itself may be
    * completely empty.
    *
    * {{{
    *   [208] l-explicit-document ::= c-directives-end
    *                                 ( l-bare-document
    *                                 | ( e-node s-l-comments ) )
    * }}}
    *
    * @return [[Parser]] for lexing '''l-explicit-document'''
    * @see [[http://yaml.org/spec/1.2/spec.html#l-explicit-document]]
    */
  private[lexers] def explicitDocument: Parser[DocumentToken] =
    DirectivesEndMarker ~> (bareDocument | (emptyNode <~ comments) ^^ { DocumentToken(_) })

  /** A directives document begins with some directives followed by an explicit directives end marker line.
    *
    * {{{
    *   [209] l-directive-document ::= l-directive+
    *                                  l-explicit-document
    * }}}
    *
    * @return [[Parser]] for lexing '''l-directive-document'''
    * @see [[http://yaml.org/spec/1.2/spec.html#l-directive-document]]
    */
  private[lexers] def directiveDocument: Parser[DocumentToken] = 
    directive.+ ~ explicitDocument ^^ { case a ~ b => b + a }
}
