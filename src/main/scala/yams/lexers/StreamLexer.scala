package yams.lexers

/** A YAML stream consists of zero or more documents. Subsequent documents require some sort of
  * separation marker line. If a document is not terminated by a document end marker line, then
  * the following document must begin with a directives end marker line.
  *
  * The stream format is intentionally “sloppy” to better support common use cases, such as
  * stream concatenation.
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2801681]]
  */
trait StreamLexer extends DocumentLexer {
  import yams.tokens.{StreamToken, DocumentToken}

  def yamlStream: Parser[StreamToken] = {
    def anyDocument: Parser[DocumentToken] = directiveDocument | explicitDocument | bareDocument

    documentPrefix ~> anyDocument ~
      ((documentSuffix.+ ~> documentPrefix ~> anyDocument) |
        (documentPrefix ~> explicitDocument)).* ^^ {
      case d ~ ds => StreamToken(d +: ds)
    }
  }
}
