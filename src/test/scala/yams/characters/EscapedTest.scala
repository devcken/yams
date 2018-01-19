package yams.characters

/** Tests for [[Escaped]].
  * 
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2776092]]
  */
class EscapedTest extends yams.helper.YamsSpec {
  "ex 5.13. Escaped characters" in {
    object EscapedCharactersTestParser extends util.parsing.combinator.RegexParsers 
                                          with Escaped 
                                          with LineBreak 
                                          with WhiteSpace {
      def apply(x: String): Either[yams.YamlLoadError, List[String]] = {
        parse("\"Fun with " ~> repsep(escapedCharacter, "\\x20\\x5C\\x0D?\\x0A|\\x0D?\\x0A|\\x20".r), x) match {
          case NoSuccess(m, rest) => Left(yams.YamlLoadError(rest.pos, m))
          case Success(y, _) => Right(y)
        }
      }
    }

    /* 
       "Fun with \\
       \" \a \b \e \f \
       \n \r \t \v \0 \
       \  \_ \N \L \P \
       \x41 \u0041 \U00000041"
     */
    
    val x = readAll("example/ch5_characters/ex5.13_escaped-characters.yml")
    
    val expected = Right(List("\\", "\"", "\u0007", "\b", "\u001b", "\f", "\n", "\r", "\t", "\u000b", "\u0000", " ", "\u00a0", "\u0085", "\u2028", "\u2029", "A", "A", "A"))
    val actual = EscapedCharactersTestParser(x)
    
    assertResult(expected)(actual)
  }

  "invalid hex codes" in {
    import util.parsing.combinator.RegexParsers

    object EscapedCharactersTestParser extends RegexParsers 
                                          with Escaped {
      def apply(x: String): Either[yams.YamlLoadError, String] = {
        parse(escapedCharacter, x) match {
          case Success(y, _) => Right(y)
          case NoSuccess(m, rest) => Left(yams.YamlLoadError(rest.pos, m))
        }
      }
    }
    
    val xs = List(("\\x4!", "expected a 2-length hex code for \\x, but found 4!."),
      ("\\u004_", "expected a 4-length hex code for \\u, but found 004_."),
      ("\\U0000004%", "expected a 8-length hex code for \\U, but found 0000004%."))
    
    xs.foreach(x => {
      val actual = EscapedCharactersTestParser(x._1)
      
      assert(actual.isLeft)
      assertResult(x._2)(actual.left.get.message)
    })
  }
}
