package yams.lexers

/** Tests for [[NodePropertyLexer]]
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://www.yaml.org/spec/1.2/spec.html#id2783797]]
  */
class NodePropertyLexerTest extends yams.helper.YamsSpec {
  import yams.YamlLoadError
  import yams.tokens.{Anchor, AnchorToken, NodePropertyToken, TagToken}

  import util.parsing.input.OffsetPosition
  
  object NodePropertyTestLexer extends NodePropertyLexer {
    def apply(x: String, s1: String, n: Int, c: yams.Context): Either[YamlLoadError, NodePropertyToken] =
      parse(s1 ~> nodeProperties(n, c), x) match {
        case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
        case Success(y, _) => Right(y)
      }
  }
  
  "ex 6.23. Node Properties" in {
    val exampleYamlPath = "example/ch6_basic-structures/ex6.23_node-properties.yml"
    
    /*
       !!str &a1 "foo":
         !!str bar
       &a2 baz : *a1
     */
    
    val x1 = readLine(exampleYamlPath, 0)
    
    val expected1 = Right(NodePropertyToken(Some(TagToken(Some("!!"), Some("str"))), Some(AnchorToken(Anchor("a1")))))
    val actual1 = NodePropertyTestLexer(x1, "", 0, yams.BlockKey)
    
    assertResult(expected1)(actual1)

    val x2 = readLine(exampleYamlPath, 1)

    val expected2 = Right(NodePropertyToken(Some(TagToken(Some("!!"), Some("str"))), None))
    val actual2 = NodePropertyTestLexer(x2, "  ", 0, yams.BlockIn)

    assertResult(expected2)(actual2)

    val x3 = readLine(exampleYamlPath, 2)

    val expected3 = Right(NodePropertyToken(None, Some(AnchorToken(Anchor("a2")))))
    val actual3 = NodePropertyTestLexer(x3, "", 0, yams.BlockKey)

    assertResult(expected3)(actual3)
  }

  "6.9.1. Node Tags" - {
    object TagPropertyTestLexer extends NodePropertyLexer {
      def apply(x: String, s1: String): Either[YamlLoadError, TagToken] = {
        parse(s1 ~> tagProperty, x) match {
          case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
          case Success(y, _) => Right(y)
        }
      }
    }

    "ex 6.24. Verbatim Tag" in {
      val exampleYamlPath = "example/ch6_basic-structures/ex6.24_verbatim-tag.yml"
      
      /*
         !<tag:yaml.org,2002:str> foo :
           !<!bar> baz
       */

      val x1 = readLine(exampleYamlPath, 0)

      val expected1 = Right(TagToken(None, Some("tag:yaml.org,2002:str")))
      val actual1 = TagPropertyTestLexer(x1, "")

      assertResult(expected1)(actual1)

      val x2 = readLine(exampleYamlPath, 1)

      val expected2 = Right(TagToken(None, Some("!bar")))
      val actual2 = TagPropertyTestLexer(x2, "  ")

      assertResult(expected2)(actual2)
    }
    
    "ex 6.25. Invalid Verbatim Tags" in {
      val exampleYamlPath = "example/ch6_basic-structures/ex6.25_invalid-verbatim-tags.yml"
      
      /*
         - !<!> foo
         - !<$:?> bar
       */
      
      val x1 = readLine(exampleYamlPath, 0)
      
      val expected1 = Left(YamlLoadError(OffsetPosition(x1, 2), "Expected a c-verbatim-tag, but failed to resolve a verbatim tag."))
      val actual1 = TagPropertyTestLexer(x1, "- ")
      
      assertResult(expected1)(actual1)
      
      val x2 = readLine(exampleYamlPath, 1)

      val expected2 = Left(YamlLoadError(OffsetPosition(x2, 2), "Expected a c-verbatim-tag, but failed to resolve a verbatim tag."))
      val actual2 = TagPropertyTestLexer(x2, "- ")

      assertResult(expected2)(actual2) 
    }

    "ex 6.26. Tag Shorthands" in {
      val exampleYamlPath = "example/ch6_basic-structures/ex6.26_tag-shortcuts.yml"
      
      /*
         %TAG !e! tag:example.com,2000:app/
         ---
         - !local foo
         - !!str bar
         - !e!tag%21 baz
       */

      val x1 = readLine(exampleYamlPath, 2)

      val expected1 = Right(TagToken(Some("!"), Some("local")))
      val actual1 = TagPropertyTestLexer(x1, "- ")

      assertResult(expected1)(actual1)

      val x2 = readLine(exampleYamlPath, 3)

      val expected2 = Right(TagToken(Some("!!"), Some("str")))
      val actual2 = TagPropertyTestLexer(x2, "- ")

      assertResult(expected2)(actual2)

      val x3 = readLine(exampleYamlPath, 4)

      val expected3 = Right(TagToken(Some("!e!"), Some("tag%21")))
      val actual3 = TagPropertyTestLexer(x3, "- ")

      assertResult(expected3)(actual3)
    }
    
    "ex 6.27. Invalid Tag Shorthands(no suffix)" in {
      val exampleYamlPath = "example/ch6_basic-structures/ex6.27_invalid-tag-shorthands.yml"
      
      /*
         %TAG !e! tag:example,2000:app/
         ---
         - !e! foo
         - !h!bar baz
       */
      
      val x = readLine(exampleYamlPath, 2)
      
      val expected = Left(YamlLoadError(OffsetPosition(x, 2), "Expected a c-ns-shorthand-tag, but failed to parse it."))
      val actual = TagPropertyTestLexer(x, "- ")
      
      assertResult(expected)(actual)
    }

    "ex 6.28. Non-Specific Tags" in {
      /*
         # Assuming conventional resolution:
         - "12"
         - 12
         - ! 12
       */
      
      val x = readLine("example/ch6_basic-structures/ex6.28_non-specific-tags.yml", 3)

      val expected = Right(TagToken(Some("!"), None))
      val actual = TagPropertyTestLexer(x, "- ")

      assertResult(expected)(actual)
    }
  }
  
  "6.9.2. Node Anchors" - {
    object NodeAnchorsTestLexer extends NodePropertyLexer {
      def apply(x: String, s1: String): Either[YamlLoadError, AnchorToken] = {
        parse(s1 ~> anchorProperty, x) match {
          case NoSuccess(m, rest) => Left(YamlLoadError(rest.pos, m))
          case Success(y, _) => Right(y)
        }
      }
    }
    
    "ex 6.29. Node Anchors" in {
      /*
         First occurrence: &anchor Value
         Second occurrence: *anchor
       */
      
      val x = readLine("example/ch6_basic-structures/ex6.29_node-anchors.yml", 0)
      
      val expected = Right(AnchorToken(Anchor("anchor")))
      val actual = NodeAnchorsTestLexer(x, "First occurrence: ")
      
      assertResult(expected)(actual)
    }
  }
}
