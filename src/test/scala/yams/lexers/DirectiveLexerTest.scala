package yams.lexers

/** Tests for [[DirectiveLexer]]
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2781147]]
  */
class DirectiveLexerTest extends yams.helper.YamsSpec {
  import yams.YamlLoadError
  import yams.tokens.{ReservedDirectiveToken, TagDirectiveToken, YamlDirectiveToken}
  
  object DirectiveTestLexer extends DirectiveLexer {


    def apply(x: String): Either[YamlLoadError, Any] =
      parse(directive, x) match {
        case NoSuccess(y, next) => Left(YamlLoadError(next.pos, y))
        case Success(r @ ReservedDirectiveToken(_, _), _) => Right(r)
        case Success(v @ YamlDirectiveToken(_), _) => Right(v)
        case Success(t @ TagDirectiveToken(_), _) => Right(t)
      }
  }

  "ex 6.13. Reserved Directives" in {
    /*
       %FOO  bar baz # Should be ignored
                     # with a warning.
       --- "foo"
     */
    
    val x = readLine("example/ch6_basic-structures/ex6.13_reserved-directives.yml", 0)
    
    val expected = Right(ReservedDirectiveToken("FOO", List("bar", "baz")))
    val actual = DirectiveTestLexer(x)
    
    assertResult(expected)(actual)
  }
  
  import yams.Version

  "A valid YAML directive" in {
    val x = "%YAML 1.2"

    val expected = Right(YamlDirectiveToken(Version()))
    val actual = DirectiveTestLexer(x)

    assertResult(expected)(actual)
  }

  "A valid YAML directive with succeeding link breaks" in {
    val x =
      """%YAML 1.2
        |
        |""".stripMargin

    val expected = Right(YamlDirectiveToken(Version()))
    val actual = DirectiveTestLexer(x)

    assertResult(expected)(actual)
  }

  import util.parsing.input.OffsetPosition

  "Preceding spaces is incorrect form for directives" in {
    val x = " %YAML 1.2"

    val expected = Left(YamlLoadError(OffsetPosition(x, 0), s"'%' expected but ' ' found"))
    val actual = DirectiveTestLexer(x)

    assertResult(expected)(actual)
  }

  "A preceding tabs is incorrect form for directives" in {
    val x = "	%YAML 1.2"

    val expected = Left(YamlLoadError(OffsetPosition(x, 0), s"'%' expected but '	' found"))
    val actual = DirectiveTestLexer(x)

    assertResult(expected)(actual)
  }

  "The correct YAML Directive with some succeeding comment" in {
    val x =
      """%YAML 1.2
        |# Comment
        |""".stripMargin

    val expected = Right(YamlDirectiveToken(Version()))
    val actual = DirectiveTestLexer(x)

    assertResult(expected)(actual)
  }
  
  "ex 6.14. \"YAML\" directive(higher version than latest version)" in {
    /*
       %YAML 1.3 # Attempt parsing
                 # with a warning
       ---
       "foo"
     */
    
    val x = readLine("example/ch6_basic-structures/ex6.14_yaml-directive.yml", 0)
    
    val expected = Right(YamlDirectiveToken(Version()))
    val actual = DirectiveTestLexer(x)
    
    assertResult(expected)(actual)
  }
  
  "ex 6.16. \"TAG\" directive" in {
    /*
       %TAG !yaml! tag:yaml.org,2002:
       ---
       !yaml!str "foo"
     */
    
    val x = readLine("example/ch6_basic-structures/ex6.16_tag-directive.yml", 0)
    
    val expected = Right(TagDirectiveToken("!yaml!" -> "tag:yaml.org,2002:"))
    val actual = DirectiveTestLexer(x)
    
    assertResult(expected)(actual)
  }
  
  "ex 6.18. Primary Tag Handle" in {
    /*
       # Private
       !foo "bar"
       ...
       # Global
       %TAG ! tag:example.com,2000:app/
       ---
       !foo "bar"
     */
    
    val x = readLine("example/ch6_basic-structures/ex6.18_primary-tag-handle.yml", 4)
    
    val expected = Right(TagDirectiveToken("!" -> "tag:example.com,2000:app/"))
    val actual = DirectiveTestLexer(x)
    
    assertResult(expected)(actual)
  }
  
  "ex 6.19. Secondary Tag Handle" in {
    /*
       %TAG !! tag:example.com,2000:app/
       ---
       !!int 1 - 3 # Interval, not integer
     */
    
    val x = readLine("example/ch6_basic-structures/ex6.19_secondary-tag-handle.yml", 0)

    val expected = Right(TagDirectiveToken("!!" -> "tag:example.com,2000:app/"))
    val actual = DirectiveTestLexer(x)

    assertResult(expected)(actual)
  }
  
  "ex 6.20. Tag Handles(named handles)" in {
    /*
       %TAG !e! tag:example.com,2000:app/
       ---
       !e!foo "bar"
     */
    
    val x = readLine("example/ch6_basic-structures/ex6.20_tag-handles.yml", 0)
    
    val expected = Right(TagDirectiveToken("!e!" -> "tag:example.com,2000:app/"))
    val actual = DirectiveTestLexer(x)
    
    assertResult(expected)(actual)
  }
  
  "ex 6.21. Local Tag Prefix" in {
    /*
       %TAG !m! !my-
       --- # Bulb here
       !m!light fluorescent
       ...
       %TAG !m! !my-
       --- # Color here
       !m!light green
     */
    
    val x = readLine("example/ch6_basic-structures/ex6.21_local-tag-prefix.yml", 0)
    
    val expected = Right(TagDirectiveToken("!m!" -> "!my-"))
    val actual = DirectiveTestLexer(x)
    
    assertResult(expected)(actual)
  }

  "ex 6.22. Global Tag Prefix" in {
    /*
       %TAG !e! tag:example.com,2000:app/
       ---
       - !e!foo "bar"
     */

    val x = readLine("example/ch6_basic-structures/ex6.22_global-tag-prefix.yml", 0)

    val expected = Right(TagDirectiveToken("!e!" -> "tag:example.com,2000:app/"))
    val actual = DirectiveTestLexer(x)

    assertResult(expected)(actual)
  }
}
