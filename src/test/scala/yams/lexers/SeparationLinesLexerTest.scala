package yams.lexers

/** Tests for [[SeparationLinesLexer]]
  * 
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2780810]]
  */
class SeparationLinesLexerTest extends yams.helper.YamsSpec {
  import yams.YamlLoadError
  
  "ex 6.12. Separation Lines" - {
    "s-separate-in-line" in {
      object SeparateInLineTestLexer extends SeparationLinesLexer {
        def apply(x: String): Either[YamlLoadError, String] = {
          parse("{" ~ separateInLine ~ "first:" ~ separateInLine ~ "Sammy," ~ separateInLine ~ "last:" ~ separateInLine ~ "Sosa" ~ separateInLine ~ "}:", x) match {
            case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
            case Success(_, _) => Right("Success")
          }
        }
      }
      
      /*
         {·first:·Sammy,·last:·Sosa·}:↓
       */
      
      val x = readLine("example/ch6_basic-structures/ex6.12_separation-lines.yml", 0)
      
      val expected = Right("Success")
      val actual = SeparateInLineTestLexer(x)
      
      assertResult(expected)(actual)
    }

    "s-separate-lines(n) short test" in {
      object SeparateLinesTestLexer extends SeparationLinesLexer {
        def apply(x: String): Either[YamlLoadError, String] = {
          parse("hr:" ~ separateLines(3) ~ "65", x) match {
            case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
            case Success(_, _) => Right("Success")
          }
        }
      }

      val x = "hr:  # Home runs\n     65"

      val expected = Right("Success")
      val actual = SeparateLinesTestLexer(x)

      assertResult(expected)(actual)
    }
    
    "s-separate-lines(n)" in {
      object SeparateLinesTestLexer extends SeparationLinesLexer {
        def apply(x: String): Either[YamlLoadError, String] = {
          parse(separateLines(2) ~ "hr:" ~ separateLines(3) ~ "65\n" ~ indent(2) ~ "avg:" ~ separateLines(3) ~ "0.278", x) match {
            case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
            case Success(_, _) => Right("Success")
          }
        }
      }
      
      /*
         ↓
         # Statistics:
         ··hr:··# Home runs
         ·····65
         ··avg:·# Average
         ···0.278
       */
      
      val x = "\n" + readLines("example/ch6_basic-structures/ex6.12_separation-lines.yml", 1 to 5)
      
      val expected = Right("Success")
      val actual = SeparateLinesTestLexer(x)
      
      assertResult(expected)(actual)
    }
  }
}
