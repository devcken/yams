package yams
package lexers

/** Tests for [[IndentationSpacesLexer]]
  * 
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2777534]]
  */
class IndentationSpacesLexerTest extends yams.helper.YamsSpec {
  import yams.tokens.{EntryToken, MappingToken, NodeToken, ScalarToken}
  
  object BlockNodeTestLexer extends BlockStylesLexer {
    def apply(x: String): Either[YamlLoadError, NodeToken] =
      parseAll(blockNode(-1, BlockIn), x) match {
        case NoSuccess(msg, rest) => Left(YamlLoadError(rest.pos, msg))
        case Success(y, _) => Right(y)
      }
  }

  "ex 6.1. Indentation Spaces" in {
    import yams.tokens.SequenceToken
    
    /*
       Not indented:
        By one space: |
           By four
             spaces
        Flow style: [    # Leading spaces
          By two,        # in flow style
         Also by two,    # are neither
         	Still by two   # content nor
           ]             # indentation.
     */
    
    val x = readLines("example/ch6_basic-structures/ex6.1_indentation-spaces.yml", 3 to 11)
    
    val expected = Right(
      MappingToken(
        List(
          EntryToken(
            ScalarToken("Not indented"), 
            MappingToken(
              List(
                EntryToken(
                  ScalarToken("By one space"),
                  ScalarToken("By four\\n  spaces\\n", Literal)
                ),
                EntryToken(
                  ScalarToken("Flow style"),
                  SequenceToken(
                    List(
                      ScalarToken("By two"),
                      ScalarToken("Also by two"),
                      ScalarToken("Still by two")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    val actual = BlockNodeTestLexer(x)
    
    assertResult(expected)(actual)
  }
}
