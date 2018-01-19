package yams.lexers

/** Tests for [[LinePrefixesLexer]].
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2778481]]
  */
class LinePrefixesLexerTest extends yams.helper.YamsSpec {
  import yams.{BlockIn, FlowIn}

  object LinePrefixesTestLexer extends LinePrefixesLexer {
    import yams.{Context, YamlLoadError}

    def apply(p: String, s: String, x: String, n: Int, c: Context): Either[YamlLoadError, Int] =
      parse(p ~> linePrefix(n, c) <~ s, x) match {
        case Success(y, _)      => Right(y)
        case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
      }
  }

  "ex 6.4. Line Prefixes" - {
    "Flow line" - {
      "Unquoted flow line prefix" in {
        /*
          plain: text
          ··lines
         */
        
        val x = readLines("example/ch6_basic-structures/ex6.4_line-prefixes.yml", 0 to 1)

        val expected = Right(1)
        val actual = LinePrefixesTestLexer("plain: text\n", "lines", x, 1, FlowIn)

        assertResult(expected)(actual)
      }

      "Quoted flow line prefix" in {
        /*
          quoted: "text
          ··→lines"
         */

        val x = readLines("example/ch6_basic-structures/ex6.4_line-prefixes.yml", 2 to 3)

        val expected = Right(1)
        val actual = LinePrefixesTestLexer("quoted: \"text\n", "lines\"", x, 1, FlowIn)

        assertResult(expected)(actual)
      }
    }

    "Block line" - {
      "A block line succeeding to a corresponding key" in {
        /*
           block: |
           ··text
         */

        val x = readLines("example/ch6_basic-structures/ex6.4_line-prefixes.yml", 4 to 5)

        val expected = Right(2)
        val actual = LinePrefixesTestLexer("block: |\n", "text", x, 2, BlockIn)

        assertResult(expected)(actual)
      }

      "A block line succeeding to a preceding block line" in {
        /*
           ··text
           ···→lines
         */
        
        val x = readLines("example/ch6_basic-structures/ex6.4_line-prefixes.yml", 5 to 6)

        val expected = Right(2)
        val actual = LinePrefixesTestLexer("  text\n", " 	lines", x, 2, BlockIn)

        assertResult(expected)(actual)
      }
    }
  }
}
