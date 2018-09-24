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
                       with CommentLexer {
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
}
