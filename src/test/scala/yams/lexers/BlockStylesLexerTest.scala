package yams
package lexers

/** Tests for [[BlockStylesLexer]]
  * 
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#Block]]
  */
class BlockStylesLexerTest extends yams.helper.YamsSpec {
  import tokens.ScalarToken
  
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
          def apply(x: String, n: Int): Either[YamlLoadError, ScalarToken] = {
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
        
        val expected = Right(ScalarToken("literal\\n\ttext\\n", Literal))
        val actual = LiteralTestLexer(x, 0)
        
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
      "ex 8.9. Folded Scalar" in {
        object FoldedTestLexer extends BlockStylesLexer {
          def apply(x: String, n: Int): Either[YamlLoadError, ScalarToken] = {
            parse(folded(n), x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }

        val x = readAll("example/ch8_block-styles/ex8.9_folded-scalar.yml")

        val expected = Right(ScalarToken("folded text\\n", Folded))
        val actual = FoldedTestLexer(x, 0)

        assertResult(expected)(actual)
      }

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

  "8.2. Block Collection Styles" - {
    import tokens.{NodeToken, SequenceToken, MappingToken, EntryToken, EmptyNodeToken, NodePropertyToken}

    "8.2.3. Block Nodes" - {
      "ex 8.14. Block Sequence" in {
        object BlockSequenceTestLexer extends BlockStylesLexer {
          def apply(x: String, n: Int): Either[YamlLoadError, SequenceToken] = {
            parse(blockSequence(n), x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }

        /*
           block sequence:
           ··- one↓
             - two : three↓
         */

        val x = readLines("example/ch8_block-styles/ex8.14_block-sequence.yml", 1 to 2)

        val expected =
          SequenceToken(
            List(ScalarToken("one", Plain),
              MappingToken(
                List(
                  EntryToken(ScalarToken("two"), ScalarToken("three"))
                )
              )
            )
          )
        val actual = BlockSequenceTestLexer(x, 2)

        assertResult(Right(expected))(actual)
      }

      "ex 8.15. Block Sequence Entry Types" in {
        object BlockIndentedTestLexer extends BlockStylesLexer {
          def apply(x: String, n: Int, c: Context): Either[YamlLoadError, NodeToken] = {
            parse("-" ~> blockIndented(n, c), x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }

        List(
          (0 to 0, EmptyNodeToken()),
          (1 to 2, ScalarToken("block node\\n", Literal)),
          (3 to 4, SequenceToken(List(ScalarToken("one"), ScalarToken("two")))),
          (5 to 5, MappingToken(List(EntryToken(ScalarToken("one"), ScalarToken("two")))))
        ).foreach{
          case (lines, expected) =>
            val actual = BlockIndentedTestLexer(readLines("example/ch8_block-styles/ex8.15_block-sequences-entry-types.yml", lines), 0, BlockIn)
            assertResult(Right(expected))(actual)
        }
      }

      "ex 8.16. Block Mappings" in {
        object BlockMappingTestLexer extends BlockStylesLexer {
          def apply(x: String, n: Int): Either[YamlLoadError, MappingToken] = {
            parse(blockMapping(n), x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }

        val x = readLine("example/ch8_block-styles/ex8.16_block-mappings.yml", 1)

        val expected = Right(MappingToken(List(EntryToken(ScalarToken("key"), ScalarToken("value")))))
        val actual = BlockMappingTestLexer(x, 1)

        assertResult(expected)(actual)
      }

      "ex 8.17. Explicit Block Mapping Entries" in {
        object BlockMapEntryTestLexer extends BlockStylesLexer {
          def apply(x: String, n: Int): Either[YamlLoadError, EntryToken] = {
            parse(blockMapEntry(n), x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }

        List(
          (0 to 0, 0, EntryToken(ScalarToken("explicit key"), EmptyNodeToken())),
          (1 to 4, 0, EntryToken(ScalarToken("block key\\n", Literal), SequenceToken(List(ScalarToken("one"), ScalarToken("two")))))
        ).foreach {
          case (lines, indentation, expected) =>
            val actual = BlockMapEntryTestLexer(readLines("example/ch8_block-styles/ex8.17_explicit-block-mapping-entries.yml", lines), indentation)
            assertResult(Right(expected))(actual)
        }
      }

      "ex 8.18. Implicit Block Mapping Entries" in {
        object BlockMapImplicitEntry extends BlockStylesLexer {
          def apply(x: String, n: Int): Either[YamlLoadError, EntryToken] = {
            parse(blockMapImplicitEntry(n), x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }

        List[(Range, NodeToken)](
          (0 to 0, EntryToken(ScalarToken("plain key"), ScalarToken("in-line value"))),
          (1 to 1, EntryToken(EmptyNodeToken(), EmptyNodeToken())),
          (2 to 4, EntryToken(ScalarToken("quoted key", DoubleQuoted), SequenceToken(List(ScalarToken("entry")))))
        ).foreach {
          case (lines, expected) =>
            val actual = BlockMapImplicitEntry(readLines("example/ch8_block-styles/ex8.18_implicit-block-mapping-entries.yml", lines), 0)
            assertResult(Right(expected))(actual)
        }
      }

      "ex 8.19. Compact Block Mappings" in {
        object CompactMappingTestLexer extends BlockStylesLexer {
          def apply(x: String, n: Int): Either[YamlLoadError, MappingToken] = {
            parse("- " ~> compactMapping(n), x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }

        List(
          (0 to 0, MappingToken(List(EntryToken(ScalarToken("sun"), ScalarToken("yellow"))))),
          (1 to 2, MappingToken(List(EntryToken(MappingToken(List(EntryToken(ScalarToken("earth"), ScalarToken("blue")))), MappingToken(List(EntryToken(ScalarToken("moon"), ScalarToken("white"))))))))
        ).foreach {
          case (lines, expected) =>
            val actual = CompactMappingTestLexer(readLines("example/ch8_block-styles/ex8.19_compact-block-mappings.yml", lines), 2)
            assertResult(Right(expected))(actual)
        }
      }

      object BlockNodeTestLexer extends BlockStylesLexer {
        def apply(x: String, s: String, n: Int, c: Context): Either[YamlLoadError, NodeToken] = {
          parse(s ~> blockNode(n, c), x) match {
            case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
            case Success(y, _) => Right(y)
          }
        }
      }

      "ex 8.20. Block Node Types" in {
        List(
          (0, "-", FlowIn, ScalarToken("flow in block", DoubleQuoted)),
          (2, "-", BlockIn, ScalarToken("Block scalar\\n", Folded)),
          (4, "-", BlockIn, MappingToken(List(EntryToken(ScalarToken("foo"), ScalarToken("bar")))))
        ).foreach{
          case (line, prefix, context, expected) =>
            val actual = BlockNodeTestLexer(readLines("example/ch8_block-styles/ex8.20_block-node-types.yml", line to line + 1), prefix, 0, context)
            assertResult(Right(expected))(actual)
        }
      }

      "ex 8.21. Block Scalar Nodes" in {
        import yams.tokens.TagToken
        
        List(
          (0 to 1, "literal:", 0, FlowIn, ScalarToken("value\\n", Literal)),
          (2 to 5, "folded:", 0, BlockIn, ScalarToken("value", Folded, Some(NodePropertyToken(Some(TagToken(Some("!"), Some("foo")))))))
        ).foreach{
          case (lines, prefix, indent, context, expected) =>
            val actual = BlockNodeTestLexer(readLines("example/ch8_block-styles/ex8.21_block-scalar-nodes.yml", lines), prefix, indent, context)
            assertResult(Right(expected))(actual)
        }
      }

      "ex 8.22. Block Collection Nodes" in {
        List(
          (0 to 3, "sequence:", 0, BlockIn, SequenceToken(List(ScalarToken("entry"), SequenceToken(List(ScalarToken("nested")))))),
          (4 to 5, "mapping:", 1, BlockIn, MappingToken(List(EntryToken(ScalarToken("foo"), ScalarToken("bar")))))
        ).foreach {
          case (lines, prefix, indent, context, expected) =>
            val actual = BlockNodeTestLexer(readLines("example/ch8_block-styles/ex8.22_block-collection-nodes.yml", lines), prefix, indent, context)
            assertResult(Right(expected))(actual)
        }
      }
    }
  }
}
