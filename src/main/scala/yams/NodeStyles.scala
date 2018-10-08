package yams

/*
   Figure 3.6. Kind/Style Combinations (http://yaml.org/spec/1.2/spec.html#id2766911)
   
                                                                  ╔════════════╗
                                                                  ║    Kind    ║
                                                                  ╚════════════╝
                                                                        ↑
                                                    ┌───────────────────┴────────────────┐
                                                    │                             ╔══════╧═════╗
                                                    │                             ║ Collection ║
                                                    │                             ╚════════════╝
                                                    │                                    ↑
                                                    │                          ┌─────────┴────────┐
                                             ╔══════╧═════╗             ╔══════╧═════╗     ╔══════╧═════╗
                                             ║   Scalar   ║             ║  Sequence  ║     ║  Mapping   ║
                                             ╚════════════╝             ╚════════════╝     ╚════════════╝
                                    ┌───────────────────────────────┐ ┌────────────────┐ ┌────────────────┐
                                    │ ┌────────────┐ ╔════════════╗ │ │ ╔════════════╗ │ │ ╔════════════╗ │
                                    │ │   Quoted   │ ║   Plain    ║ │ │ ║  Explicit  ║ │ │ ║  Explicit  ║ │
                                    │ └────────────┘ ╚════════════╝ │ │ ╚════════════╝ │ │ ╚════════════╝ │
                     ╔════════════╗ │        ↑                      │ │                │ │                │
                   ┌─╢    Flow    ║ │        ├──────────────┐       │ │                │ │ ╔════════════╗ │
                   │ ╚════════════╝ │ ╔══════╧═════╗ ╔══════╧═════╗ │ │                │ │ ║Single Pair ║ │
                   │                │ ║   Double   ║ ║   Single   ║ │ │                │ │ ╚════════════╝ │
   ╔════════════╗  │                │ ╚════════════╝ ╚════════════╝ │ │                │ │                │
   ║   Style    ║ ←┤                └───────────────────────────────┘ └────────────────┘ └────────────────┘
   ╚════════════╝  │                ┌───────────────────────────────┐ ┌────────────────┐ ┌────────────────┐
                   │                │        ╔════════════╗         │ │ ╔════════════╗ │ │ ╔════════════╗ │
                   │ ╔════════════╗ │        ║  Literal   ║         │ │ ║ Next Line  ║ │ │ ║ Next Line  ║ │
                   └─╢   Block    ║ │        ╚════════════╝         │ │ ╚════════════╝ │ │ ╚════════════╝ │
                     ╚════════════╝ │        ╔════════════╗         │ │ ╔════════════╗ │ │ ╔════════════╗ │
                                    │        ║   Folded   ║         │ │ ║  In-Line   ║ │ │ ║  In-Line   ║ │
                                    │        ╚════════════╝         │ │ ╚════════════╝ │ │ ╚════════════╝ │
                                    └───────────────────────────────┘ └────────────────┘ └────────────────┘
 */

/**
  * Each node is presented in some style, depending on its kind. The node style is a presentation detail 
  * and is not reflected in the serialization tree or representation graph.
  * 
  * There are two groups of styles. Block styles use indentation to denote structure; In contrast, 
  * flow styles styles rely on explicit indicators.
  * 
  * @see [[http://yaml.org/spec/1.2/spec.html#id2766446]]
  */
sealed trait Style
sealed trait Kind {
  def style: Style
}
case object Flow extends Style
case object Block extends Style

/** YAML provides a rich set of scalar styles. These styles offer a range of trade-offs between 
  * expressive power and readability. Flow scalar styles include the plain style and two quoted styles, 
  * the single-quoted style and the double-quoted style. Block scalar styles include the literal style 
  * and the folded style.
  *
  * @param style [[Flow]] or [[Block]]
  */
sealed abstract class Scalar(override val style: Style) extends Kind
case object DoubleQuoted extends Scalar(Flow)
case object SingleQuoted extends Scalar(Flow)
case object Plain extends Scalar(Flow)
case object Literal extends Scalar(Block)
case object Folded extends Scalar(Block)
