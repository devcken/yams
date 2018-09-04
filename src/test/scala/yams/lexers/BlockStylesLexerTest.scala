package yams
package lexers

/** Tests for [[BlockStylesLexer]]
  * 
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#Block]]
  */
class BlockStylesLexerTest extends yams.helper.YamsSpec {
  "8.1. Block Scalar Styles" - {
    "8.1.1. Block Scalar Headers" - {
      "8.1.1.1. Block Indentation Indicator" - {
        object IndentationIndicatorTestLexer extends BlockStylesLexer {
          def apply(x: String, s: String): Either[YamlLoadError, Option[Int]] = {
            parse(s ~> indentationIndicator, x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }
        
        "ex 8.2. Block Indentation Indicator" in {
          /*
           - |°
           ·detected
           - >°
           ·
           ··
           ··# detected
           - |1
           ··explicit
           - >°
           ·→
           ·detected
         */

          val x1 = readLine("example/ch8_block-styles/ex8.2_block-indentation-indicator.yml", 0)

          val expected1 = Right(None)
          val actual1 = IndentationIndicatorTestLexer(x1, "- |")

          assertResult(expected1)(actual1)

          val x2 = readLine("example/ch8_block-styles/ex8.2_block-indentation-indicator.yml", 2)

          val expected2 = Right(None)
          val actual2 = IndentationIndicatorTestLexer(x2, "- >")

          assertResult(expected2)(actual2)

          val x3 = readLine("example/ch8_block-styles/ex8.2_block-indentation-indicator.yml", 6)

          val expected3 = Right(Some(1))
          val actual3 = IndentationIndicatorTestLexer(x3, "- |")

          assertResult(expected3)(actual3)

          val x4 = readLine("example/ch8_block-styles/ex8.2_block-indentation-indicator.yml", 8)

          val expected4 = Right(None)
          val actual4 = IndentationIndicatorTestLexer(x4, "- >")

          assertResult(expected4)(actual4)
        }
      }
    }
  }
}
