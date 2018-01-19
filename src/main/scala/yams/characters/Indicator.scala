package yams.characters

/** Indicators are characters that have special semantics.
  * 
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2772075]]
  */
trait Indicator {
  final val SequenceEntry = "\\x2D"       // "-", hyphen
  final val MappingKey = "\\x3F"          // "?", question mark
  final val MappingValue = "\\x3A"        // ":", colon
  
  final val CollectionEntry = "\\x2C"     // ",", comma
  final val SequenceStart = "\\x5B"       // "[", left bracket
  final val SequenceEnd = "\\x5D"         // "]", right bracket
  final val MappingStart = "\\x7B"        // "{", left brace
  final val MappingEnd = "\\x7D"          // "}", right brace
  
  final val Comment = "\\x23"             // "#", hash
  
  final val Anchor = "\\x26"              // "&", ampersand
  final val Alias = "\\x2A"               // "*", asterisk
  final val Tag = "\\x21"                 // "!", exclamation
  
  final val Literal = "\\x7C"             // "|", vertical bar
  final val Folded = "\\x3E"              // ">", greater than
  
  final val SingleQuote = "\\x27"         // "'", single quote
  final val DoubleQuote = "\\x22"         // """, double quote
  
  final val Directive = "\\x25"           // "%", percent
  
  final val Reserved = "\\x40\\x60"       // "@", at or "`", backtick
  
  /** Any indicator characters
    * 
    * {{{
    *   [22] c-indicator ::=   “-” | “?” | “:” | “,” | “[” | “]” | “{” | “}”
    *                        | “#” | “&” | “*” | “!” | “|” | “>” | “'” | “"”
    *                        | “%” | “@” | “`”
    * }}}
    * 
    * @see [[http://yaml.org/spec/1.2/spec.html#c-indicator]]
    */
  final val Indicator = "[" + 
    s"$SequenceEntry$MappingKey$MappingValue$CollectionEntry$SequenceStart$SequenceEnd" +
    s"$MappingStart$MappingEnd$Comment$Anchor$Alias$Tag$Literal$Folded$SingleQuote$DoubleQuote" +
    s"$Directive$Reserved"+ 
  "]"
  
  /** The “[”, “]”, “{”, “}” and “,” indicators denote structure in flow collections. They are therefore
    * forbidden in some cases, to avoid ambiguity in several constructs. This is handled on a case-by-case
    * basis by the relevant productions.
    * 
    * {{{
    *   [23] c-flow-indicator ::= “,” | “[” | “]” | “{” | “}”
    * }}}
    * 
    * @see [[http://yaml.org/spec/1.2/spec.html#c-flow-indicator]]
    */
  final val FlowIndicator = s"[$CollectionEntry$SequenceStart$SequenceEnd$MappingStart$MappingEnd]"
}
