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

      "ex 9.2. Document Markers" in {
        object DocumentMarkersTestParser extends DocumentLexer {
          def apply(x: String, s1: String, s2: String): Either[YamlLoadError, (String, String)] = {
            parse(s1 ~> DocumentEndMarker ~ (s2 ~> documentSuffix), x) match {
              case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
              case Success(a ~ b, _) => Right((a, b))
            }
          }
        }

        val x = readAll("example/ch9_character-stream/ex9.2_document-markers.yml")

        val expected = Right(("---", "..."))
        val actual = DocumentMarkersTestParser(x, "%YAML 1.2\n", "\nDocument\n")

        assertResult(expected)(actual)
      }
    }
  }
}
