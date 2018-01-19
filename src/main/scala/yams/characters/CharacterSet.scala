package yams.characters

/** The character set used in YAML.
  * 
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2770814]]
  */
trait CharacterSet {
  /** To ensure readability, YAML streams use only the printable subset of the Unicode character set.
    * The allowed character range explicitly excludes:
    * 
    * - the C0 control block ```#x0-#x1F``` (except for TAB ```#x9```, LF ```#xA```, and CR ```#xD``` which are allowed)
    * - DEL ```#x7F```
    * - the C1 control block ```#x80-#x9F``` (except for NEL ```#x85``` which is allowed)
    * - the surrogate block ```#xD800-#xDFFF```, ```#xFFFE```, and ```#xFFFF```.
    * 
    * {{{
    *   [1] c-printable ::=  #x9 | #xA | #xD | [#x20-#x7E]          /* 8 bit */
    *                      | #x85 | [#xA0-#xD7FF] | [#xE000-#xFFFD] /* 16 bit */
    *                      | [#x10000-#x10FFFF]                     /* 32 bit */
    * }}}
    * 
    * On input, a YAML processor must accept all Unicode characters except those explicitly excluded above.
    * 
    * On output, a YAML processor must only produce acceptable characters. Any excluded characters
    * must be presented using escape sequences. In addition, any allowed characters known to be
    * non-printable should also be escaped. This isn’t mandatory since a full implementation would
    * require extensive character property tables.
    */
  final val Printable = "[\\x09\\x0A\\x0D\\x20-\\x7E\\x85\\xA0-\\x{D7FF}\\x{E000}-\\x{FFFD}\\x{10000}-\\x{10FFFF}]"

  /** To ensure JSON compatibility, YAML processors must allow all non-control characters inside quoted
    * scalars. To ensure readability, non-printable characters should be escaped on output, even inside
    * such scalars. Note that JSON quoted scalars cannot span multiple lines or contain tabs, but YAML
    * quoted scalars can.
    * 
    * {{{
    *   [2] nb-json ::= #x9 | [#x20-#x10FFFF]
    * }}}
    */
  final val NoBreakJson = "[\\x09\\x20-\\x{10FFFF}]"
}
