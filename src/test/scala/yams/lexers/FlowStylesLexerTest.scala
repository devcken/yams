package yams.lexers

/** Tests for [[FlowStylesLexer]]
  * 
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#Flow]]
  */
class FlowStylesLexerTest extends yams.helper.YamsSpec {
  import util.parsing.input.OffsetPosition
  import yams.{YamlLoadError, Context, BlockKey, FlowKey, FlowIn, FlowOut}
  import yams.tokens._

  "7.1 Alias Node" - {
    object AliasNodeTestLexer extends FlowStylesLexer {
      def apply(x: String): Either[YamlLoadError, AliasToken] =
        parse(aliasNode, x) match {
          case Success(y, _) => Right(y)
          case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
        }
    }
    
    "A valid alias node" in {
      val x = "*foo"

      val expected = Right(AliasToken("foo"))
      val actual = AliasNodeTestLexer(x)
      
      assertResult(expected)(actual)
    }

    "The whitespace is not accepted in a alias node" in {
      val x = "* foo"

      val expected = Left(YamlLoadError(OffsetPosition(x, 1), s"Failed to parse a anchor name."))
      val actual = AliasNodeTestLexer(x)

      assertResult(expected)(actual)
    }
  }
  
  "7.2 Empty Nodes" - {
    "ex 7.2. Empty Content" in {
      object EmptyContentTestParser extends FlowStylesLexer {
        def apply(x: String, s1: String, s2: String): Either[YamlLoadError, (String, EmptyNodeToken, String)] = {
          parse(s1 ~ emptyScalar ~ s2, x) match {
            case Success(y, _) => Right((y._1._1, y._1._2, y._2))
            case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
          }
        }
      }
      
      /*
         {
           foo : !!str°,
           !!str° : bar,
         }
       */

      val x1 = readLine("example/ch7_flow-styles/ex7.2_empty-content.yml", 1)
      
      val expected1 = Right(("  foo : !!str", EmptyNodeToken(), ","))
      val actual1 = EmptyContentTestParser(x1, "  foo : !!str", ",")
      
      assertResult(expected1)(actual1)

      val x2 = readLine("example/ch7_flow-styles/ex7.2_empty-content.yml", 2)

      val expected2 = Right(("  !!str", EmptyNodeToken(), " : bar,"))
      val actual2 = EmptyContentTestParser(x2, "  !!str", " : bar,")

      assertResult(expected2)(actual2)
    }
    
    "ex 7.3. Completely Empty Flow Nodes" in {
      object EmptyFlowNodesTestParser extends FlowStylesLexer {
        def apply(x: String, s1: String, s2: String): Either[YamlLoadError, (String, EmptyNodeToken, String)] = {
          parse(s1 ~ emptyNode ~ s2, x) match {
            case Success(y, _) => Right((y._1._1, y._1._2, y._2))
            case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
          }
        }
      }
      
      /*
         {
           ? foo :°,
           °: bar,
         }
       */

      val x1 = readLine("example/ch7_flow-styles/ex7.3_completely-empty-flow-nodes.yml", 1)

      val expected1 = Right(("  ? foo :", EmptyNodeToken(), ","))
      val actual1 = EmptyFlowNodesTestParser(x1, "  ? foo :", ",")

      assertResult(expected1)(actual1)

      val x2 = readLine("example/ch7_flow-styles/ex7.3_completely-empty-flow-nodes.yml", 2)

      val expected2 = Right(("  ", EmptyNodeToken(), ": bar,"))
      val actual2 = EmptyFlowNodesTestParser(x2, "  ", ": bar,")

      assertResult(expected2)(actual2)
    }
  }

  "7.3 Flow Scalar Styles" - {
    "7.3.1 Double-Quoted Style" - {
      "ex 7.4. Double Quoted Implicit Keys" - {
        object DoubleQuotedStyleTestParser extends FlowStylesLexer {
          def apply(x: String, n: Int, c: Context): Either[YamlLoadError, String] =
            parse(doubleQuoted(n, c), x) match {
              case Success(y, _) => Right(y)
              case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
            }
        }

        "Double Quoted Implicit Block Keys" in {
          /*
             "implicit block key" : [
               "implicit flow key" : value,
              ]
           */
          
          val x = readAll("example/ch7_flow-styles/ex7.4_double-quoted-implicit-keys.yml")

          val expected = Right("implicit block key")
          val actual = DoubleQuotedStyleTestParser(x, 0, BlockKey)

          assertResult(expected)(actual)
        }

        "Double Quoted Implicit Flow Keys" in {
          val x = readLine("example/ch7_flow-styles/ex7.4_double-quoted-implicit-keys.yml", 1).trim

          val expected = Right("implicit flow key")
          val actual = DoubleQuotedStyleTestParser(x, 0, FlowKey)

          assertResult(expected)(actual)
        }
      }
      
      "ex 7.5. Double Quoted Line Breaks" in {
        object DoubleQuotedLineBreaksTestLexer extends FlowStylesLexer {
          def apply(s1: String, s2: String, s3: String, s4: String, x: String, n: Int): Either[YamlLoadError, String] =
            parse(s1 ~ doubleBreak(n) ~ s2 ~ doubleBreak(n) ~ s3 ~ doubleBreak(n) ~ s4, x) match {
              case Success(y, _) => Right(s"${y._1._1._1._1._1._1}${y._1._1._1._1._1._2}${y._1._1._1._1._2}${y._1._1._1._2}${y._1._1._2}${y._1._2}${y._2.replace("\\", "").replace("\t", "\\t")}")
              case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
            }
        }

        /*
           "folded·↓
           to a space,→↓
           ·↓
           to a line feed, or·→\↓
           ·\·→non-content"
         */
        val x = readAll("example/ch7_flow-styles/ex7.5_double-quoted-line-breaks.yml")

        val expected = Right("\"folded to a space,\\nto a line feed, or \\t \\tnon-content\"")
        val actual = DoubleQuotedLineBreaksTestLexer("\"folded", "to a space,", "to a line feed, or", "\\ 	non-content\"", x, 0)

        assertResult(expected)(actual)
      }

      "ex 7.6. Double Quoted Lines" in {
        object DoubleQuotedLinesTestLexer extends FlowStylesLexer {
          def apply(x: String, n: Int): Either[YamlLoadError, String] =
            parse("\"" ~> doubleMultiLine(n) <~ "\"", x) match {
              case Success(y, _) => Right(y)
              case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
            }
        }

        /*
           "·1st non-empty↓
           ↓
           ·2nd non-empty·
           →3rd non-empty·"
         */
        val x = readAll("example/ch7_flow-styles/ex7.6_double-quoted-lines.yml")

        val expected = Right(" 1st non-empty\\n2nd non-empty 3rd non-empty ")
        val actual = DoubleQuotedLinesTestLexer(x, 0)

        assertResult(expected)(actual)
      }
    }

    "7.3.2 Single-Quoted Style" - {
      "ex 7.8. Single Quoted Implicit Block Keys" - {
        object SingleQuotedStyleTestParser extends FlowStylesLexer {
          def apply(x: String, n: Int, c: Context): Either[YamlLoadError, String] =
            parse(singleQuoted(n, c), x) match {
              case Success(y, _) => Right(y)
              case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
            }
        }

        "Single Quoted Implicit Block Keys" in {
          val x = readAll("example/ch7_flow-styles/ex7.8_single-quoted-implicit-keys.yml")

          val expected = Right("implicit block key")
          val actual = SingleQuotedStyleTestParser(x, 0, BlockKey)

          assertResult(expected)(actual)
        }

        "Single Quoted Implicit Flow Keys" in {
          val x = readLine("example/ch7_flow-styles/ex7.8_single-quoted-implicit-keys.yml", 1).trim

          val expected = Right("implicit flow key")
          val actual = SingleQuotedStyleTestParser(x, 0, FlowKey)

          assertResult(expected)(actual)
        }
      }
      
      "ex 7.9. Single Quoted Lines" in {
        object SingleQuotedLinesTestLexer extends FlowStylesLexer {
          def apply(x: String, n: Int): Either[YamlLoadError, String] =
            parse("\'" ~> singleMultiLine(n) <~ "\'", x) match {
              case Success(y, _) => Right(y)
              case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
            }
        }

        /*
           '·1st non-empty↓
           ↓
           ·2nd non-empty·
           →3rd non-empty·'
         */
        val x = readAll("example/ch7_flow-styles/ex7.9_single-quoted-lines.yml")

        val expected = Right(" 1st non-empty\\n2nd non-empty 3rd non-empty ")
        val actual = SingleQuotedLinesTestLexer(x, 0)

        assertResult(expected)(actual)
      }
    }

    "7.3.3 Plain Style" - {
      "ex 7.10. plain characters" - {
        val ExampleYamlPath = "example/ch7_flow-styles/ex7.10_plain-characters.yml"
        
        "ns-plain-first(c) followed by ns-plain-char(c)" in {
          object PlainFirstFollowedByPlainCharTestParser extends FlowStylesLexer {
            def apply(x: String, s1: String, s2: String, c: Context): Either[YamlLoadError, String] =
              parse(s1 ~> plainFirst(c) ~ plainChar(c) <~ s2, x) match {
                case Success(y, _) => Right(y._1 + y._2)
                case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
              }
          }

          /*
             - ::vector
           */
          
          val x1 = readLine(ExampleYamlPath, 1)

          val expected1 = Right("::")
          val actual1 = PlainFirstFollowedByPlainCharTestParser(x1, "- ", "vector", FlowOut)

          assertResult(expected1)(actual1)
          
          /*
             - [ ::vector,
           */
          
          val x2 = readLine(ExampleYamlPath, 7)

          val expected2 = Right("::")
          val actual2 = PlainFirstFollowedByPlainCharTestParser(x2, "- [ ", "vector", FlowIn)

          assertResult(expected2)(actual2)
        }
        
        "ns-plain-first(c)" in {
          object PlainFirstTestParser extends FlowStylesLexer {
            def apply(x: String, s1: String, s2: String, c: Context): Either[YamlLoadError, String] =
              parse(s1 ~> plainFirst(c) <~ s2, x) match {
                case Success(y, _) => Right(y)
                case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
              }
          }
          
          /*
             - -123
           */

          val x1 = readLine(ExampleYamlPath, 4)

          val expected1 = Right("-")
          val actual1 = PlainFirstTestParser(x1, "- ", "123", FlowOut)

          assertResult(expected1)(actual1)
          
          /*
               -123,
           */

          val x2 = readLine(ExampleYamlPath, 10)

          val expected2 = Right("-")
          val actual2 = PlainFirstTestParser(x2, "  ", "123", FlowIn)

          assertResult(expected2)(actual2)
        }
        
        "ns-plain-char(c)" in {
          object PlainCharTestParser extends FlowStylesLexer {
            def apply(x: String, s1: String, s2: String, c: Context): Either[YamlLoadError, String] =
              parse(s1 ~> plainChar(c) <~ s2, x) match {
                case Success(y, _) => Right(y)
                case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
              }
          }
          
          /*
             - Up, up, and away!
           */
          
          val x = readLine(ExampleYamlPath, 3)
          
          val expected = Right(",")
          val actual = PlainCharTestParser(x, "- Up", " up, and away!", FlowOut)

          assertResult(expected)(actual)
        }

        "multiple ns-plain-char(c)" in {
          object MultiplePlainCharTestParser extends FlowStylesLexer {
            def apply(x: String, s1: String, s2: String, s3: String, c: Context): Either[YamlLoadError, (String, String)] =
              parse((s1 ~> plainChar(c) <~ s2) ~ plainChar(c) <~ s3, x) match {
                case Success(y, _) => Right((y._1, y._2))
                case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
              }
          }
          
          /*
             - http://example.com/foo#bar
           */
          
          val x1 = readLine(ExampleYamlPath, 5)

          val expected1 = Right((":", "#"))
          val actual1 = MultiplePlainCharTestParser(x1, "- http", "//example.com/foo", "bar", FlowOut)

          assertResult(expected1)(actual1)
          
          /*
               http://example.com/foo#bar ]
           */

          val x2 = readLine(ExampleYamlPath, 11)

          val expected2 = Right((":", "#"))
          val actual2 = MultiplePlainCharTestParser(x2, "  http", "//example.com/foo", "bar ]", FlowIn)

          assertResult(expected2)(actual2)
        }
      }
      
      "ex 7.11. Plain Implicit Keys" - {
        object PlainImplicitKeysTestParser extends FlowStylesLexer {
          def apply(x: String, c: Context): Either[YamlLoadError, String] =
            parse(indent(2).? ~> plain(0, c), x) match {
              case Success(y, _) => Right(y)
              case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
            }
        }

        val ExampleYamlPath = "example/ch7_flow-styles/ex7.11_plain-implicit-keys.yml"

        "Plain Implicit Block Key" in {
          /*
             implicit block key : [
           */

          val x = readLine(ExampleYamlPath, 0)

          val expected = Right("implicit block key")
          val actual = PlainImplicitKeysTestParser(x, BlockKey)

          assertResult(expected)(actual)
        }

        "Plain Implicit Flow Key" in {
          /*
               implicit flow key : value,
           */

          val x = readLine(ExampleYamlPath, 1)

          val expected = Right("implicit flow key")
          val actual = PlainImplicitKeysTestParser(x, FlowKey)

          assertResult(expected)(actual)
        }
      }
      
      "ex 7.12. Plain Lines" in {
        object PlainMultiLinesTestLexer extends FlowStylesLexer {
          def apply(x: String, n: Int, c: Context): Either[YamlLoadError, String] =
            parse(plain(n, c), x) match {
              case Success(y, _) => Right(y)
              case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
            }
        }

        /*
           1st non-empty↓
           ↓
           ·2nd non-empty·
           →3rd non-empty
         */
        val x = readAll("example/ch7_flow-styles/ex7.12_plain-lines.yml")

        val expected = Right("1st non-empty\\n2nd non-empty 3rd non-empty")
        val actual = PlainMultiLinesTestLexer(x, 0, FlowIn)

        assertResult(expected)(actual)
      }
    }
  }
  
  "7.4. Flow Collection Styles" - {
    "7.4.1. Flow Sequences" - {
      "ex 7.13. Flow Sequence" in {
        object FlowSequenceTestLexer extends FlowStylesLexer {
          def apply(x: String, s: String, n: Int, c: Context): Either[YamlLoadError, FlowNodeToken] = {
            parse(s ~> flowSequence(n, c), x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }
        
        val exampleYamlPath = "example/ch7_flow-styles/ex7.13_flow-sequence.yml"
        
        /*
           - [ one, two, ]
           - [three ,four]
         */
        
        val x1 = readLine(exampleYamlPath, 0)
        
        val expected1 = Right(FlowSequenceToken(List(ScalarToken("one"), ScalarToken("two"))))
        val actual1 = FlowSequenceTestLexer(x1, "- ", 0, FlowIn)
        
        assertResult(expected1)(actual1)
        
        val x2 = readLine(exampleYamlPath, 1)
        
        val expected2 = Right(FlowSequenceToken(List(ScalarToken("three"), ScalarToken("four"))))
        val actual2 = FlowSequenceTestLexer(x2, "- ", 0, FlowIn)
        
        assertResult(expected2)(actual2)
      }
      
      "ex 7.14. Flow Sequence Entries" in {
        object FlowSequenceEntryTestLexer extends FlowStylesLexer {
          def apply(x: String, n: Int, c: Context, s: String = ""): Either[YamlLoadError, FlowNodeToken] = {
            parse(s ~> flowSeqEntry(n, c), x) match {
              case Success(y, _) => Right(y)
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
            }
          }
        }
        
        val exampleYamlPath = "example/ch7_flow-styles/ex7.14_flow-sequence-entries.yml"
        
        /*
           [
           "double
            quoted", 'single
                      quoted',
           plain
            text, [ nested ],
           single: pair,
           ]
         */
        
        val x1 = readLines(exampleYamlPath, 1 to 2)

        val expected1 = Right(ScalarToken("double quoted", DoubleQuoted))
        val actual1 = FlowSequenceEntryTestLexer(x1, 0, FlowIn)

        assertResult(expected1)(actual1)

        val x2 = readLines(exampleYamlPath, 2 to 3)

        val expected2 = Right(ScalarToken("single quoted", SingleQuoted))
        val actual2 = FlowSequenceEntryTestLexer(x2, 0, FlowIn, " quoted\", ")

        assertResult(expected2)(actual2)

        val x3 = readLines(exampleYamlPath, 4 to 5)

        val expected3 = Right(ScalarToken("plain text"))
        val actual3 = FlowSequenceEntryTestLexer(x3, 0, FlowIn)

        assertResult(expected3)(actual3)

        val x4 = readLines(exampleYamlPath, 5 to 6)

        val expected4 = Right(FlowSequenceToken(List(ScalarToken("nested"))))
        val actual4 = FlowSequenceEntryTestLexer(x4, 0, FlowIn, " text, ")

        assertResult(expected4)(actual4)

        val x5 = readLine(exampleYamlPath, 6)

        val expected5 = Right(FlowEntryToken(ScalarToken("single"), ScalarToken("pair")))
        val actual5 = FlowSequenceEntryTestLexer(x5, 0, FlowIn)

        assertResult(expected5)(actual5)
      }
    }
    
    "7.4.2. Flow Mappings" - {
      object FlowMappingTestLexer extends FlowStylesLexer {
        def apply(x: String, n: Int, c: Context, s: String = ""): Either[YamlLoadError, FlowNodeToken] = {
          parse(s ~> flowMapping(n, c), x) match {
            case Success(y, _) => Right(y)
            case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
          }
        }
      }
      
      "ex 7.15. Flow Mappings" in {
        val exampleYamlPath = "example/ch7_flow-styles/ex7.15_flow-mappings.yml"
        
        /*
           - { one : two , three: four , }
           - {five: six,seven : eight}
         */
        
        val x1 = readLine(exampleYamlPath, 0)
        
        val expected1 = Right(FlowMappingToken(List(FlowEntryToken(ScalarToken("one"), ScalarToken("two")), FlowEntryToken(ScalarToken("three"), ScalarToken("four")))))
        val actual1 = FlowMappingTestLexer(x1, 0, FlowIn, "- ")
        
        assertResult(expected1)(actual1)

        val x2 = readLine(exampleYamlPath, 1)

        val expected2 = Right(FlowMappingToken(List(FlowEntryToken(ScalarToken("five"), ScalarToken("six")), FlowEntryToken(ScalarToken("seven"), ScalarToken("eight")))))
        val actual2 = FlowMappingTestLexer(x2, 0, FlowIn, "- ")

        assertResult(expected2)(actual2)
      }
      
      "ex 7.16. Flow Mapping Entries" in {
        object FlowMapEntryTestLexer extends FlowStylesLexer {
          def apply(x: String, n: Int, c: Context): Either[YamlLoadError, FlowNodeToken] = {
            parse(flowMapEntry(n, c), x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }
        
        val exampleYamlPath = "example/ch7_flow-styles/ex7.16_flow-mappings-entries.yml"
        
        /*
           {
           ? explicit: entry,
           implicit: entry,
           ?°°
           }
         */

        val x1 = readLine(exampleYamlPath, 1)

        val expected1 = Right(FlowEntryToken(ScalarToken("explicit"), ScalarToken("entry")))
        val actual1 = FlowMapEntryTestLexer(x1, 0, FlowIn)

        assertResult(expected1)(actual1)

        val x2 = readLine(exampleYamlPath, 2)

        val expected2 = Right(FlowEntryToken(ScalarToken("implicit"), ScalarToken("entry")))
        val actual2 = FlowMapEntryTestLexer(x2, 0, FlowIn)

        assertResult(expected2)(actual2)

        val x3 = readLine(exampleYamlPath, 3)

        val expected3 = Right(FlowEntryToken(EmptyNodeToken(), EmptyNodeToken()))
        val actual3 = FlowMapEntryTestLexer(x3, 0, FlowIn)

        assertResult(expected3)(actual3)
      }
      
      "ex 7.17. Flow Mapping Separate Values" in {
        object FlowMapImplicitEntryTextLexer extends FlowStylesLexer {
          def apply(x: String, n: Int, c: Context): Either[YamlLoadError, FlowEntryToken] = {
            parse(flowMapImplicitEntry(n, c), x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }
        
        val exampleYamlPath = "example/ch7_flow-styles/ex7.17_flow-mapping-separate-values.yml"
        
        /*
           {
           unquoted·:·"separate",
           http://foo.com,
           omitted value:°,
           °:·omitted key,
           }
         */
        
        val x1 = readLine(exampleYamlPath, 1)

        val expected1 = Right(FlowEntryToken(ScalarToken("unquoted"), ScalarToken("separate", DoubleQuoted)))
        val actual1 = FlowMapImplicitEntryTextLexer(x1, 0, FlowIn)
        
        assertResult(expected1)(actual1)
        
        val x2 = readLine(exampleYamlPath, 2)
        
        val expected2 = Right(FlowEntryToken(ScalarToken("http://foo.com"), EmptyNodeToken()))
        val actual2 = FlowMapImplicitEntryTextLexer(x2, 0, FlowIn)
        
        assertResult(expected2)(actual2)
        
        val x3 = readLine(exampleYamlPath, 3)
        
        val expected3 = Right(FlowEntryToken(ScalarToken("omitted value"), EmptyNodeToken()))
        val actual3 = FlowMapImplicitEntryTextLexer(x3, 0, FlowIn)
        
        assertResult(expected3)(actual3)
        
        val x4 = readLine(exampleYamlPath, 4)
        
        val expected4 = Right(FlowEntryToken(EmptyNodeToken(), ScalarToken("omitted key")))
        val actual4 = FlowMapImplicitEntryTextLexer(x4, 0, FlowIn)
        
        assertResult(expected4)(actual4)
      }
      
      "ex 7.18. Flow Mapping Adjacent Values" in {
        object FlowMapJsonKeyEntryTextLexer extends FlowStylesLexer {
          def apply(x: String, n: Int, c: Context): Either[YamlLoadError, FlowEntryToken] = {
            parse(flowMapImplicitEntry(n, c), x) match {
              case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
              case Success(y, _) => Right(y)
            }
          }
        }
        
        val exampleYamlPath = "example/ch7_flow-styles/ex7.18_flow-mapping-adjacent-values.yml"
        
        /*
           {
           "adjacent":value,
           "readable":·value,
           "empty":°
           }
         */
        
        val x1 = readLine(exampleYamlPath, 1)
        
        val expected1 = Right(FlowEntryToken(ScalarToken("adjacent", DoubleQuoted), ScalarToken("value")))
        val actual1 = FlowMapJsonKeyEntryTextLexer(x1, 0, FlowIn)
        
        assertResult(expected1)(actual1)
        
        val x2 = readLine(exampleYamlPath, 2)
        
        val expected2 = Right(FlowEntryToken(ScalarToken("readable", DoubleQuoted), ScalarToken("value")))
        val actual2 = FlowMapJsonKeyEntryTextLexer(x2, 0, FlowIn)
        
        assertResult(expected2)(actual2)
        
        val x3 = readLine(exampleYamlPath, 3)
        
        val expected3 = Right(FlowEntryToken(ScalarToken("empty", DoubleQuoted), EmptyNodeToken()))
        val actual3 = FlowMapJsonKeyEntryTextLexer(x3, 0, FlowIn)
        
        assertResult(expected3)(actual3)
      }
      
      object FlowPairTextLexer extends FlowStylesLexer {
        def apply(x: String, n: Int, c: Context): Either[YamlLoadError, FlowEntryToken] = {
          parse(flowPair(n, c), x) match {
            case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
            case Success(y, _) => Right(y)
          }
        }
      }
      
      "ex 7.19. Single Pair Flow Mappings" in {
        val exampleYamlPath = "example/ch7_flow-styles/ex7.19_single-pair-flow-mappings.yml"
        
        /*
           [
           foo: bar
           ]
         */
        
        val x = readLine(exampleYamlPath, 1)
        
        val expected = Right(FlowEntryToken(ScalarToken("foo"), ScalarToken("bar")))
        val actual = FlowPairTextLexer(x, 0, FlowIn)
        
        assertResult(expected)(actual)
      }
      
      "ex 7.20. Single Pair Explicit Entry" in {
        val exampleYamlPath = "example/ch7_flow-styles/ex7.20_single-pair-explicit-entry.yml"
        
        /*
           [
           ? foo
            bar : baz
           ]
         */
        
        val x = readLines(exampleYamlPath, 1 to 2)
        
        val expected = Right(FlowEntryToken(ScalarToken("foo bar"), ScalarToken("baz")))
        val actual = FlowPairTextLexer(x, 0, FlowIn)
        
        assertResult(expected)(actual)
      }
      
      object FlowPairEntryTestLexer extends FlowStylesLexer {
        def apply(x: String, s: String, n: Int, c: Context): Either[YamlLoadError, FlowEntryToken] = {
          parse(s ~> flowPairEntry(n, c), x) match {
            case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
            case Success(y, _) => Right(y)
          }
        }
      }
      
      "ex 7.21. Single Pair Implicit Entry" in {
        val exampleYamlPath = "example/ch7_flow-styles/ex7.21_sing-pair-implicit-entries.yml"
        
        /*           
           - [ YAML·: separate ]
           - [ °: empty key entry ]
           - [ {JSON: like}:adjacent ]
         */
        
        val x1 = readLine(exampleYamlPath, 0)
        
        val expected1 = Right(FlowEntryToken(ScalarToken("YAML"), ScalarToken("separate")))
        val actual1 = FlowPairEntryTestLexer(x1, "- [ ", 0, FlowIn)
        
        assertResult(expected1)(actual1)
        
        val x2 = readLine(exampleYamlPath, 1)
        
        val expected2 = Right(FlowEntryToken(EmptyNodeToken(), ScalarToken("empty key entry")))
        val actual2 = FlowPairEntryTestLexer(x2, "- [ ", 0, FlowIn)
        
        assertResult(expected2)(actual2)
        
        val x3 = readLine(exampleYamlPath, 2)
        
        val expected3 = Right(FlowEntryToken(FlowMappingToken(List(FlowEntryToken(ScalarToken("JSON"), ScalarToken("like")))), ScalarToken("adjacent")))
        val actual3 = FlowPairEntryTestLexer(x3, "- [ ", 0, FlowIn)
        
        assertResult(expected3)(actual3)
      }
      
      "ex 7.22. Invalid Implicit Keys" in {
        val exampleYamlPath = "example/ch7_flow-styles/ex7.22_invalid-implicit-keys.yml"
        
        /*
           [ foo
            bar: invalid,
            "foo...>1K characters...bar": invalid ]
         */
        
        val x1 = readLines(exampleYamlPath, 0 to 1)
        
        val expected1 = Left(YamlLoadError(OffsetPosition(x1, 5), "':' expected but '\n' found"))
        val actual1 = FlowPairEntryTestLexer(x1, "[ ", 0, FlowIn)
        
        assertResult(expected1)(actual1)
        
        val x2 = readLine(exampleYamlPath, 2)
        
        val expected2 = Left(YamlLoadError(OffsetPosition(x2, 1), "Some key is too long. The length of the key is limited to 1024 at most."))
        val actual2 = FlowPairEntryTestLexer(x2, " ", 0, FlowIn)
        
        assertResult(expected2)(actual2)
      }
    }
  }
  
  "7.5. Flow Nodes" - {
    "ex 7.23. Flow Content" in {
      object FlowContentTestParser extends FlowStylesLexer {
        def apply(x: String, n: Int, c: Context): Either[YamlLoadError, FlowNodeToken] = {
          parse("- " ~> flowContent(n, c), x) match {
            case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
            case Success(y, _) => Right(y)
          }
        }
      }
      
      val exampleYamlPath = "example/ch7_flow-styles/ex7.23_flow-content.yml"
      
      /*
         - [ a, b ]
         - { a: b }
         - "a"
         - 'b'
         - c
       */
      
      val x1 = readLine(exampleYamlPath, 0)

      val expected1 = Right(FlowSequenceToken(List(ScalarToken("a"), ScalarToken("b"))))
      val actual1 = FlowContentTestParser(x1, 0, FlowIn)

      assertResult(expected1)(actual1)

      val x2 = readLine(exampleYamlPath, 1)

      val expected2 = Right(FlowMappingToken(List(FlowEntryToken(ScalarToken("a"), ScalarToken("b")))))
      val actual2 = FlowContentTestParser(x2, 0, FlowIn)

      assertResult(expected2)(actual2)

      val x3 = readLine(exampleYamlPath, 2)

      val expected3 = Right(ScalarToken("a", DoubleQuoted))
      val actual3 = FlowContentTestParser(x3, 0, FlowIn)

      assertResult(expected3)(actual3)

      val x4 = readLine(exampleYamlPath, 3)

      val expected4 = Right(ScalarToken("b", SingleQuoted))
      val actual4 = FlowContentTestParser(x4, 0, FlowIn)

      assertResult(expected4)(actual4)

      val x5 = readLine(exampleYamlPath, 4)

      val expected5 = Right(ScalarToken("c"))
      val actual5 = FlowContentTestParser(x5, 0, FlowIn)

      assertResult(expected5)(actual5)
    }
    
    "ex 7.24. Flow Nodes" in {
      object FlowNodeTestParser extends FlowStylesLexer {
        def apply(x: String, n: Int, c: Context): Either[YamlLoadError, FlowNodeToken] = {
          parse("- " ~> flowNode(n, c), x) match {
            case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
            case Success(y, _) => Right(y)
          }
        }
      }
      
      val exampleYamlPath = "example/ch7_flow-styles/ex7.24_flow-nodes.yml"
      
      /*
         - !!str "a"
         - 'b'
         - &anchor "c"
         - *anchor
         - !!str°
       */

      val x1 = readLine(exampleYamlPath, 0)

      val expected1 = Right(ScalarToken("a", DoubleQuoted, Some(NodePropertyToken(Some(TagToken(Some("!!"), Some("str"))), None))))
      val actual1 = FlowNodeTestParser(x1, 0, FlowIn)

      assertResult(expected1)(actual1)

      val x2 = readLine(exampleYamlPath, 1)

      val expected2 = Right(ScalarToken("b", SingleQuoted, None))
      val actual2 = FlowNodeTestParser(x2, 0, FlowIn)

      assertResult(expected2)(actual2)

      val x3 = readLine(exampleYamlPath, 2)

      val expected3 = Right(ScalarToken("c", DoubleQuoted, Some(NodePropertyToken(None, Some(AnchorToken(Anchor("anchor")))))))
      val actual3 = FlowNodeTestParser(x3, 0, FlowIn)

      assertResult(expected3)(actual3)

      val x4 = readLine(exampleYamlPath, 3)

      val expected4 = Right(AliasToken("anchor"))
      val actual4 = FlowNodeTestParser(x4, 0, FlowIn)

      assertResult(expected4)(actual4)

      val x5 = readLine(exampleYamlPath, 4)

      val expected5 = Right(EmptyNodeToken(Some(NodePropertyToken(Some(TagToken(Some("!!"), Some("str"))), None))))
      val actual5 = FlowNodeTestParser(x5, 0, FlowIn)

      assertResult(expected5)(actual5)
    }
  }
}
