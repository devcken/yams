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
        "ex 8.1. Block Scalar Header" in {
          object BlockHeaderTestLexer extends BlockStylesLexer {
            def apply(x: String, s: String): Either[YamlLoadError, (Int, ChompingMethod)] = {
              parse(s ~> blockHeader, x) match {
                case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
                case Success(y, _) => Right(y)
              }
            }
          }
          
          val exampleYamlPath = "example/ch8_block-styles/ex8.1_block-scalar-header.yml"
          
          /*
             - | # Empty header↓
              literal
             - >1 # Indentation indicator↓
              ·folded
             - |+ # Chomping indicator↓
              keep
             
             - >1- # Both indicators↓
              ·strip
           */
          
          List((0, (0, Clip), "- |"), (2, (1, Clip), "- >"), (4, (0, Keep), "- |"), (7, (1, Strip), "- >"))
            .foreach{
              case (line, expected, prefix) =>
                val actual = BlockHeaderTestLexer(readLine(exampleYamlPath, line), prefix)
                assertResult(Right(expected))(actual)
            }
        }
        
        "ex 8.2. Block Indentation Indicator" in {
          object IndentationIndicatorTestLexer extends BlockStylesLexer {
            def apply(x: String, s: String): Either[YamlLoadError, Option[Int]] = {
              parse(s ~> indentationIndicator, x) match {
                case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
                case Success(y, _) => Right(y)
              }
            }
          }
          
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

        object ChompedEmptyTestLexer extends BlockStylesLexer {
          def apply(x: String, n: Int, t: ChompingMethod): Either[YamlLoadError, Option[String]] = {
            parse(chompedEmpty(n, t), x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }

        "ex 8.5. Chomping Trailing Lines" in {
          val exampleYamlPath = "example/ch8_block-styles/ex8.5_chomping-trailing-lines.yml"

          /*
               # Strip
                # Comments:
              strip: |-
                # text↓
              ··⇓
              ·# Clip
              ··# comments:
              ↓
              clip: |
                # text↓
              ·↓
              ·# Keep
              ··# comments:
              ↓
              keep: |+
                # text↓
              ↓
              ·# Trail
              ··# comments.
           */

          List((2, Strip, "strip: |"), (9, Clip, "clip: |"), (16, Keep, "keep: |"))
            .foreach{ case (line, expected1, prefix) =>
              val actual1 = ChompingIndicatorTestLexer(readLine(exampleYamlPath, line), prefix)
              assertResult(Right(expected1))(actual1)

              val expected2 = Right(None)
              val actual2 = ChompedEmptyTestLexer(readLines(exampleYamlPath, line + 2 to line + 5), 2, actual1.right.get)
              assertResult(expected2)(actual2)
            }
        }

        "ex 8.6. Empty Scalar Chomping" in {
          val exampleYamlPath = "example/ch8_block-styles/ex8.6_empty-scalar-chomping.yml"

          /*
             strip: >-
             ↓
             clip: >
             ↓
             keep: |+
             ↓
           */

          List((0, Strip, "strip: >", ""), (3, Clip, "clip: >", ""), (6, Keep, "keep: |", "\n"))
            .foreach{ case (line, expected, prefix, expected2) =>
              val chompingMethod = ChompingIndicatorTestLexer(readLine(exampleYamlPath, line), prefix)
              assertResult(Right(expected))(chompingMethod)
              assertResult(Right(Some(expected2)))(ChompedEmptyTestLexer(readLines(exampleYamlPath, line + 1 to line + 2), 2, chompingMethod.right.get))
            }
        }
      }
    }
    
    "8.1.2. Literal Style" - {
      "ex 8.7. Literal Scalar" in {
        object LiteralTestLexer extends BlockStylesLexer {
          def apply(x: String, n: Int): Either[YamlLoadError, String] = {
            parse(literal(n), x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }
        
        val x = readAll("example/ch8_block-styles/ex8.7_literal-scalar.yml")
        
        /*
           |↓
           ·literal↓
           ·→text↓
           ↓
         */
        
        val expected = Right("literal\\n\ttext\\n")
        val actual = LiteralTestLexer(x, 1)
        
        assertResult(expected)(actual)
      }
      
      "ex 8.8. Literal Content" in {
        object LiteralContentTestLexer extends BlockStylesLexer {
          def apply(x: String, n: Int, t: ChompingMethod): Either[YamlLoadError, String] = {
            parse(literalContent(n, t), x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }

        val x = readLines("example/ch8_block-styles/ex8.8_literal-content.yml", 1 to 8)

        /*
           |
           ·
           ··
           ··literal↓
           ···↓
           ··
           ··text↓
           ↓
           ·# Comment
         */

        val expected = Right("\\n\\nliteral\\n \\n\\ntext\\n")
        val actual = LiteralContentTestLexer(x, 2, Clip)

        assertResult(expected)(actual)
      }
    }

    "8.1.3. Folded Style" - {
      "ex 8.10. Folded Lines" in {
        object FoldedLinesTestLexer extends BlockStylesLexer {
          def apply(x: String, n: Int): Either[YamlLoadError, String] = {
            parse(foldedLines(n), x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }

        val exampleYamlPath = "example/ch8_block-styles/ex8.10_folded-lines.yml"

        List(
          (2 to 6, "folded line\\nnext line"),
          (12 to 13, "last line"),
        ).foreach{ case (lines, expected) =>
          val actual = FoldedLinesTestLexer(readLines(exampleYamlPath, lines), 1)
          assertResult(Right(expected))(actual)
        }
      }

      "ex 8.11. More Indented Lines" in {
        object SpacedLinesTestLexer extends BlockStylesLexer {
          def apply(x: String, n: Int): Either[YamlLoadError, String] = {
            parse(spacedLines(n), x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }

        val exampleYamlPath = "example/ch8_block-styles/ex8.10_folded-lines.yml"

        val x = readLines(exampleYamlPath, 7 to 10)

        val expected = Right("  * bullet\\n\\n  * list\\n  * lines")
        val actual = SpacedLinesTestLexer(x, 1)

        assertResult(expected)(actual)
      }

      "ex 8.12. Empty Separation Lines" in {
        object DiffLineTestLexer extends BlockStylesLexer {
          def apply(x: String, n: Int): Either[YamlLoadError, String] = {
            parse(diffLines(n), x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }

        val exampleYamlPath = "example/ch8_block-styles/ex8.10_folded-lines.yml"

        val x = readLines(exampleYamlPath, 1 to 13)

        val expected = Right("\\nfolded line\\nnext line\\n  * bullet\\n\\n  * list\\n  * lines\\n\\nlast line")
        val actual = DiffLineTestLexer(x, 1)

        assertResult(expected)(actual)
      }

      "ex 8.13. Final Empty Lines" in {
        object FoldedContent extends BlockStylesLexer {
          def apply(x: String, n: Int, t: ChompingMethod): Either[YamlLoadError, String] = {
            parse(foldedContent(n, t), x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }

        val exampleYamlPath = "example/ch8_block-styles/ex8.10_folded-lines.yml"

        val x = readLines(exampleYamlPath, 1 to 15)

        val expected = Right("\\nfolded line\\nnext line\\n  * bullet\\n\\n  * list\\n  * lines\\n\\nlast line\\n")
        val actual = FoldedContent(x, 1, Clip)

        assertResult(expected)(actual)
      }
    }
  }
}
