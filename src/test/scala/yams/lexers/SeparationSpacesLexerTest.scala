package yams.lexers

/** Tests for [[SeparationSpacesLexer]]
  * 
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2778241]]
  */
class SeparationSpacesLexerTest extends yams.helper.YamsSpec {
  "ex 6.3. Separation Spaces" in {
    object SeparationSpacesTestLexer extends SeparationSpacesLexer {
      import yams.YamlLoadError

      def apply(x: String): Either[YamlLoadError, String] = {
        parse("-" ~ separateInLine ~ "foo:" ~ separateInLine ~ "bar\n- -" ~ separateInLine ~ "baz\n  -" ~ separateInLine ~ "baz", x) match {
          case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
          case Success(_, _) => Right("Success")
        }
      }
    }
    
    /*
       -·foo:→·bar
       - -·baz
         -→baz
     */
    
    val x = readAll("example/ch6_basic-structures/ex6.3_separation-spaces.yml")
    
    val expected = Right("Success")
    val actual = SeparationSpacesTestLexer(x)
    
    assertResult(expected)(actual)
  }
}
