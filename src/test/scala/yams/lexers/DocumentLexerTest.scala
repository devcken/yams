package yams.lexers

/** Tests for [[DocumentLexer]]
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
        
        /*
           # Comment
           # lines
           Document
         */

        val x = readLines("example/ch9_character-stream/ex9.1_document-prefix.yml", 0 to 1)

        val expected = Right(List(Some("# Comment"), Some("# lines")))
        val actual = DocumentPrefixTestParser(x)

        assertResult(expected)(actual)
      }
    }

    "9.1.2. Document Markers" - {
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
    
    import yams.tokens.{DocumentToken, ScalarToken, FlowMappingToken, FlowEntryToken, EmptyNodeToken, YamlDirectiveToken}

    "9.1.3. Bare Documents" - {
      "ex 9.3. Bare Documents" in {
        object BareDocumentTestParser extends DocumentLexer {
          def apply(x: String): Either[YamlLoadError, DocumentToken] = {
            parse(bareDocument, x) match {
              case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }
        
        List(
          (0 to 1, DocumentToken(ScalarToken("Bare document"))),
          (5 to 6, DocumentToken(ScalarToken("%!PS-Adobe-2.0\\n")))
        ).foreach {
          case (lines, expected) =>
            val x = readLines("example/ch9_character-stream/ex9.3_bare-documents.yml", lines)
            
            assertResult(Right(expected))(BareDocumentTestParser(x))
        }
      }
    }

    "9.1.4. Explicit Documents" - {
      "ex 9.4. Explicit Documents" in {
        object ExplicitDocumentTestParser extends DocumentLexer {
          def apply(x: String): Either[YamlLoadError, DocumentToken] = {
            parse(explicitDocument, x) match {
              case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }
        
        List(
          (0 to 2, DocumentToken(FlowMappingToken(List(FlowEntryToken(ScalarToken("matches %"), ScalarToken("20")))))),
          (4 to 5, DocumentToken(EmptyNodeToken()))
        ).foreach {
          case (lines, expected) =>
            val x = readLines("example/ch9_character-stream/ex9.4_explicit-documents.yml", lines)

            assertResult(Right(expected))(ExplicitDocumentTestParser(x))
        }
      }
    }
    
    import yams.Version
    
    "9.1.5. Directives Documents" - {
      "ex 9.5. Directives Documents" in {
        object DirectivesDocumentTestParser extends DocumentLexer {
          def apply(x: String): Either[YamlLoadError, DocumentToken] = {
            parse(directiveDocument, x) match {
              case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }
        
        List(
          (0 to 2, DocumentToken(ScalarToken("%!PS-Adobe-2.0\\n"),List(YamlDirectiveToken(Version())))),
          (4 to 6, DocumentToken(EmptyNodeToken(), List(YamlDirectiveToken(Version()))))
        ).foreach {
          case (lines, expected) =>
            val x = readLines("example/ch9_character-stream/ex9.5_directives-documents.yml", lines)
            
            assertResult(Right(expected))(DirectivesDocumentTestParser(x))
        }
      }
    }
  }
}
