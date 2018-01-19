package yams.lexers

/** Tests for [[IndentationSpacesLexer]]
  * 
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2777534]]
  */
class IndentationSpacesLexerTest extends yams.helper.YamsSpec {
  import yams.YamlLoadError
  
  object IndentationSpacesTestLexer extends IndentationSpacesLexer {
    def apply(x: String, n: Int): Either[YamlLoadError, Int] =
      parse(indent(n), x) match {
        case Success(y, _)      => Right(y)
        case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
      }
  }

  "Indentation Spaces" in {
    val xs = List(
      (" By one space:", 1),
      ("    By four", 4),
      ("      spaces", 4)
    )

    for (x <- xs) {
      val expected = Right(x._2)
      val actual = IndentationSpacesTestLexer(x._1, x._2)

      assert(expected == actual)
    }
  }
}
