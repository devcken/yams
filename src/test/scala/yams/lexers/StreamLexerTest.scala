package yams.lexers

/** Tests for [[StreamLexer]]
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#YAML]]
  */
class StreamLexerTest extends yams.helper.YamsSpec {
  import yams.YamlLoadError
  import yams.tokens.{StreamToken, DocumentToken, ScalarToken, EmptyNodeToken, MappingToken, EntryToken}
  
  "9.2 Streams" - {
    "ex 9.6. Stream" in {
      object YamlStreamTextLexer extends StreamLexer {
        def apply(x: String): Either[YamlLoadError, StreamToken] = {
          parseAll(yamlStream, x) match {
            case NoSuccess(msg, next) => Left(YamlLoadError(next.pos, msg))
            case Success(y, _) => Right(y)
          }
        }
      }
      
      val x = readAll("example/ch9_character-stream/ex9.6_stream.yml")
      
      val expected = Right(StreamToken(
        List(
          DocumentToken(ScalarToken("Document")),
          DocumentToken(EmptyNodeToken()), 
          DocumentToken(MappingToken(List(EntryToken(ScalarToken("matches %"), ScalarToken("20")))))
        )
      ))
      val actual = YamlStreamTextLexer(x)
      
      assertResult(expected)(actual)
    }
  }
}
