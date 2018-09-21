package yams.lexers

/** Tests for [[DocumentLexerTest]]
  * 
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2800132]]
  */
class DocumentLexerTest extends yams.helper.YamsSpec {
  import yams.YamlLoadError
  
  "9.1. Documents" - {
    "9.1.1. Document Prefix" - {
      "ex 9.1. Document Prefix" in {
        object DocumentPrefixTestParser extends DocumentLexer {
          def apply(x: String): Either[YamlLoadError, List[Option[String]]] = {
            parse(documentPrefix, x) match {
              case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }
        
        val x = readAll("example/ch9_character-stream/ex9.1_document-prefix.yml")
        
        val expected = Right(List(Some("# Comment"), Some("# lines")))
        val actual = DocumentPrefixTestParser(x)
        
        assertResult(expected)(actual)
      }
    }
  }
}
