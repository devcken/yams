package yams.lexers

/** Tests for [[IndentationSpacesLexer]]
  * 
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2777534]]
  */
class IndentationSpacesLexerTest extends yams.helper.YamsSpec {
  import yams.{BlockIn, YamlLoadError}
  import yams.tokens.{FlowNodeToken, FlowMappingToken, FlowEntryToken, ScalarToken}
  
  object BlockMappingTestLexer extends BlockStylesLexer {
    def apply(x: String): Either[YamlLoadError, FlowNodeToken] =
      parseAll(blockNode(-1, BlockIn), x) match {
        case NoSuccess(msg, rest) => Left(YamlLoadError(rest.pos, msg))
        case Success(y, _) => Right(y)
      }
  }

  "ex 6.1. Indentation Spaces" in {
    import yams.tokens.{FlowSequenceToken, Literal}
    
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
      FlowMappingToken(
        List(
          FlowEntryToken(
            ScalarToken("Not indented"), 
            FlowMappingToken(
              List(
                FlowEntryToken(
                  ScalarToken("By one space"),
                  ScalarToken("By four\\n  spaces\\n", Literal)
                ),
                FlowEntryToken(
                  ScalarToken("Flow style"),
                  FlowSequenceToken(
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
    val actual = BlockMappingTestLexer(x)
    
    assertResult(expected)(actual)
  }
}
