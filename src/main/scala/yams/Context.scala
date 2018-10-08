package yams

/** The context allows productions to tweak their behavior according to their surrounding.
  * YAML supports two groups of contexts, distinguishing between block styles and flow styles.
  *
  * In block styles, indentation is used to delineate structure. To capture human perception of
  * indentation the rules require special treatment of the “-” character, used in block sequences.
  * Hence in some cases productions need to behave differently inside block sequences (block-in context)
  * and outside them (block-out context).
  *
  * In flow styles, explicit indicators are used to delineate structure. These styles can be viewed
  * as the natural extension of JSON to cover tagged, single-quoted and plain scalars. Since the latter 
  * have no delineating indicators, they are subject to some restrictions to avoid ambiguities. These 
  * restrictions depend on where they appear:
  *
  * - as implicit keys directly inside a block mapping (block-key)
  * - as implicit keys inside a flow mapping (flow-key)
  * - as values inside a flow collection (flow-in)
  * - as values outside one (flow-out)
  *
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2769866]]
  */
sealed trait Context

case object FlowIn extends Context
case object FlowOut extends Context
case object FlowKey extends Context
case object BlockIn extends Context
case object BlockOut extends Context
case object BlockKey extends Context
