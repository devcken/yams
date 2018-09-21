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
  
  import yams.nodes.Tag

  /** The marker indicating the directives-end.
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#marker/directives%20end/ directives end marker]]
    */
  private val DirectivesEndMark = """-{3,3}""".r

  protected[lexers] final val DefaultTagDirectives = Map("!" -> "!", "!!" -> Tag.DefaultPrefix)
  
  
}
