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
    val exampleYamlPath = "example/ch6_basic-structures/ex6.12_separation-lines.yml"
    
    "s-separate-in-line" in {
      object SeparateInLineTestLexer extends SeparationLinesLexer {
        def apply(x: String): Either[YamlLoadError, Unit] = {
          parse("{" ~ separateInLine ~ "first:" ~ separateInLine ~ "Sammy," ~ separateInLine ~ "last:" ~ separateInLine ~ "Sosa" ~ separateInLine ~ "}:", x) match {
            case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
            case Success(_, _) => Right(Unit)
          }
        }
      }
      
      /*
         {·first:·Sammy,·last:·Sosa·}:↓
       */
      
      val x = readLine(exampleYamlPath, 0)
      
      assert(SeparateInLineTestLexer(x).isRight)
    }

    "s-separate-lines(n) short test" in {
      object SeparateLinesTestLexer extends SeparationLinesLexer {
        def apply(x: String): Either[YamlLoadError, Unit] = {
          parse("hr:" ~ separateLines(3) ~ "65", x) match {
            case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
            case Success(_, _) => Right(Unit)
          }
        }
      }

      val x = "hr:  # Home runs\n     65"

      assert(SeparateLinesTestLexer(x).isRight)
    }
    
    "s-separate-lines(n)" in {
      object SeparateLinesTestLexer extends SeparationLinesLexer {
        def apply(x: String): Either[YamlLoadError, Unit] = {
          parse(separateLines(2) ~ "hr:" ~ separateLines(3) ~ "65\n" ~ indent(2) ~ "avg:" ~ separateLines(3) ~ "0.278", x) match {
            case NoSuccess(m, next) => Left(YamlLoadError(next.pos, m))
            case Success(_, _) => Right(Unit)
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
      
      val x = "\n" + readLines(exampleYamlPath, 1 to 5)
      
      assert(SeparateLinesTestLexer(x).isRight)
    }
  }
}
