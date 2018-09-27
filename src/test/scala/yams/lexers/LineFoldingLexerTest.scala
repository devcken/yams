package yams.lexers

/** Tests for [[LineFoldingLexer]]
  * 
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2779048]]
  */
class LineFoldingLexerTest extends yams.helper.YamsSpec {
  import yams.{YamlLoadError, Context, BlockIn}
  
  "ex 6.6. Line folding (trimmed)" in {
    object LineFoldingTestLexer extends LineFoldingLexer {
      def apply(s1: String, s2: String, s3: String, x: String, n: Int, c: Context): Either[YamlLoadError, (String, String)] =
        parse(s1 ~> trimmed(n, c) ~ (s2 ~> asSpace) <~ s3, x) match {
          case Success(t ~ s, _)  => Right((t, s))
          case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
        }
    }
    
    /*
       >-
         trimmed↓
       ··↓
       ·↓
       ↓
         as↓
         space
     */

    val x = readAll("example/ch6_basic-structures/ex6.6_line-folding.yml")
    
    val expected = Right(("\\n\\n\\n", " "))
    val actual = LineFoldingTestLexer(">-\n  trimmed", "  as", "  space", x, 0, BlockIn)

    assertResult(expected)(actual)
  }
  
  "ex 6.7. Block Folding" ignore { // TODO 6.7. Block Folding test ignored now
    object BlockFoldingTestLexer extends LineFoldingLexer {
      def apply(s1: String, s2: String, s3: String, x: String, n: Int, c: Context): Either[YamlLoadError, String] =
        parse(s1 ~ folded(n, c) ~ s2 ~ folded(n, c) ~ s3, x) match {
          case Success(s_1 ~ f_1 ~ s_2 ~ f_2 ~ s_3, input) => Right(s"$s_1$f_1$s_2$f_2$s_3")
          case NoSuccess(m, rest)                      => Left(YamlLoadError(rest.pos, m))
        }
    }
    
    /*
       >
       ··foo·↓
       ·↓
       ··→·bar↓
       ↓
       ··baz↓
     */

    val x1 = readAll("example/ch6_basic-structures/ex6.7_block-folding.yml")

                                        /* Originally, this tab is interpreted as '\t' in example,
                                           but it is just a content detail.
                                           So, here it isn't interpreted as mentioned above.
                                           ↓ */
    val expected1 = Right(">\n  foo \\n\\n  	 bar\\n\\n  baz\n")
                           /*--          --           --
                             ↑           ↑            ↑
                             All certain spaces are indentations in block styles.
                             So, in example, they are interpreted as a indentation of each lines.
                             However, this test don't handle them directly. 
                            */
    val actual1 = BlockFoldingTestLexer(">\n  foo ", "  	 bar", "  baz\n", x1, 2, BlockIn)

    assertResult(expected1)(actual1)
    
    val x2 = readAll("example/ch6_basic-structures/ex6.7_block-folding_no-leading-white.yml")
    
    val expected2 = Right(">\n  foo \\nbar\\n  baz\\n")
    val actual2 = BlockFoldingTestLexer(">\n  foo", "  bar", "  baz\n", x2, 2, BlockIn)
    
    assertResult(expected2)(actual2)
  }
  
  "ex 6.8. Flow folding" in {
    object FlowFoldingTestParser extends LineFoldingLexer {
      def apply(s1: String, s2: String, s3: String, x: String, n: Int): Either[YamlLoadError, String] =
        parse("\"" ~> flowFolded(n) ~ s1 ~ flowFolded(n) ~ s2 ~ flowFolded(n) ~ s3 ~ flowFolded(n) <~ "\"", x) match {
          case Success(f_1 ~ s_1 ~ f_2 ~ s_2 ~ f_3 ~ s_3 ~ f_4, _) => Right(s"$f_1$s_1$f_2$s_2$f_3$s_3$f_4")
          case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
        }
    }
    
    /*
       "↓
       ··foo·↓
       ·↓
       ··→·bar↓
       ↓
       ··baz↓
       "
     */

    val x = readAll("example/ch6_basic-structures/ex6.8_flow-folding.yml")
    
    val expected = Right(" foo\\nbar\\nbaz ")
    val actual = FlowFoldingTestParser("foo", "bar", "baz", x, 0)
    
    assertResult(expected)(actual)
  }
}
