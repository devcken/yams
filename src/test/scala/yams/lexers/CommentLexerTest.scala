package yams.lexers


/** Tests for [[CommentLexer]].
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2780069]]
  */
class CommentLexerTest extends yams.helper.YamsSpec {
  import yams.YamlLoadError

  object CommentTestLexer extends CommentLexer {
    def apply(x: String): Either[YamlLoadError, Option[String]] =
      parse(commentLine, x) match {
        case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
        case Success(y, _)      => Right(y)
      }
  }

  "Only one single line comment" in assertResult(Right(Some("# Comment ")))(CommentTestLexer(" # Comment "))

  "No comment" in assertResult(Left(YamlLoadError(scala.util.parsing.input.OffsetPosition(" ", 0), "l-comment expected, but not found")))(CommentTestLexer(" "))
  
  "ex 6.9. Separated Comment" in {
    /*
       key:    # Comment
         value
     */
    
    val x = readAll("example/ch6_basic-structures/ex6.9_separated-comment.yml")

    object SeparatedCommentTestParser extends CommentLexer {
      def apply(x: String): Either[YamlLoadError, (String, String)] =
        parse("key:" ~ optComment ~ "  value", x) match {
          case NoSuccess(m, rest)          => Left(YamlLoadError(rest.pos, m))
          case Success(key ~ _ ~ value, _) => Right((key, value))
        }
    }
    
    val expected = Right(("key:", "  value"))
    val actual = SeparatedCommentTestParser(x)

    assertResult(expected)(actual)
  }
  
  "ex 6.10. Comment Lines" in {
    /*
         # Comment
          
       
       
     */
    
    val x = readAll("example/ch6_basic-structures/ex6.10_comment-lines.yml")

    val expected = Right(Some("# Comment"))
    val actual = CommentTestLexer(x)
    
    assertResult(expected)(actual)
  }
  
  "ex 6.11. Multi-Line Comments" in {
    /*
       key:····# Comment↓
       ········# lines↓
         value↓
       ↓
     */
    
    val x = readAll("example/ch6_basic-structures/ex6.11_multi-line-comments.yml")

    object CommentTestLexer extends CommentLexer {
      def apply(x: String): Either[YamlLoadError, (String, String)] =
        parse("key:" ~ comments ~ "  value", x) match {
          case NoSuccess(m, rest)          => Left(YamlLoadError(rest.pos, m))
          case Success(key ~ _ ~ value, _) => Right((key, value))
        }
    }

    val expected = Right(("key:", "  value"))
    val actual = CommentTestLexer(x)

    assertResult(expected)(actual)
  }
}
