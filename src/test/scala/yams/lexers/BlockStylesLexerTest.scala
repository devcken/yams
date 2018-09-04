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
          val exampleYamlPath = "example/ch8_block-styles/ex8.2_block-indentation-indicator.yml"

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

          List((0, None, "- |"), (2, None, "- >"), (6, Some(1), "- |"), (8, None, "- >"))
            .foreach{
              case (line, expected, prefix) =>
                assertResult(Right(expected))(IndentationIndicatorTestLexer(readLine(exampleYamlPath, line), prefix))
            }
        }
      }

      "8.1.1.2. Block Chomping Indicator" - {
        object ChompingIndicatorTestLexer extends BlockStylesLexer {
          def apply(x: String, s: String): Either[YamlLoadError, ChompingMethod] = {
            parse(s ~> chompingIndicator, x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }

        object ChompedLastTestLexer extends BlockStylesLexer {
          def apply(x: String, t: ChompingMethod): Either[YamlLoadError, String] = {
            parse("  text" ~> chompedLast(t), x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }

        "ex 8.4. Chomping Final Line Break" in {
          val exampleYamlPath = "example/ch8_block-styles/ex8.4_chomping-final-line-break.yml"

          /*
             strip: |-
               text↓
             clip: |
               text↓
             keep: |+
               text↓
           */

          List((0, Strip, "strip: |", ""), (3, Clip, "clip: |", "\\n"), (6, Keep, "keep: |", "\\n"))
            .foreach{
              case (line, expected, prefix, expected2) =>
                val chompingMethod = ChompingIndicatorTestLexer(readLine(exampleYamlPath, line), prefix)
                assertResult(Right(expected))(chompingMethod)
                val chomped = ChompedLastTestLexer(readLines(exampleYamlPath, line + 1 to line + 2), chompingMethod.right.get)
                assertResult(Right(expected2))(chomped)
            }
        }
      }
    }
  }
}
