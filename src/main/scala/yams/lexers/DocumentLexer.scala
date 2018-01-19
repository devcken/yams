package yams.lexers

/** The Parser which parses
  *
  */
trait DocumentLexer {
  import yams.nodes.Tag

  /** The marker indicating the directives-end.
    *
    * @see [[http://yaml.org/spec/1.2/spec.html#marker/directives%20end/ directives end marker]]
    */
  private val DirectivesEndMark = """-{3,3}""".r

  protected[lexers] final val DefaultTagDirectives = Map("!" -> "!", "!!" -> Tag.DefaultPrefix)
}
