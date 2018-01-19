package yams.lexers

import org.scalatest.FreeSpec
import yams.helper.ExampleReader

/** A test for [[EmptyLinesLexer]]
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2778853]]
  */
class EmptyLinesLexerTest extends FreeSpec with ExampleReader {
  import yams.{YamlLoadError, Context, FlowOut}

  "ex 6.5. Empty Lines" - {
    "Folding" in {
      object EmptyLinesTestLexer extends EmptyLinesLexer {
        def apply(s1: String, s2: String, x: String, n: Int, c: Context): Either[YamlLoadError, String] =
          parse(indent(n) ~> s1 ~ emptyLine(n, c) ~ (indent(n) ~> s2), x) match {
            case Success(s ~ e ~ i, _) => Right(s"${s.replace("\n", "")}$e$i")
            case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
          }
      }
      
      /*
         Folding:
           "Empty line
         ···→
           as a line feed"
       */
      
      val x = readLines("example/ch6_basic-structures/ex6.5_empty-lines.yml", 1 to 3)

      val expected = Right("\"Empty line\\nas a line feed\"")
      val actual = EmptyLinesTestLexer("\"Empty line\n", "as a line feed\"", x, 2, FlowOut)

      assertResult(expected)(actual)
    }

    // TODO The semantics of empty lines depend on the scala style they appear in.
    "Chomping" in {
      object EmptyLinesTestParser extends EmptyLinesLexer {
        def apply(s1: String, x: String, n: Int, c: Context): Either[YamlLoadError, String] =
          parse(indent(n) ~> s1 ~ emptyLine(n, c), x) match {
            case Success(s ~ e, _) => Right(s"${s.replace("\n", "")}$e")
            case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
          }
      }
      
      /*
         Chomping: |
           Clipped empty lines
         ·
       */

      val x = readLines("example/ch6_basic-structures/ex6.5_empty-lines.yml", 5 to 6)

      val expected = Right("Clipped empty lines\\n")
      val actual = EmptyLinesTestParser("Clipped empty lines", x, 2, FlowOut)

      assertResult(expected)(actual)
    }
  }
}
