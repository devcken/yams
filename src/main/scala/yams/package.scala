package object yams {
  type Context = YamlContext
  
  final case object FlowIn extends Context { val name = "flow in" }
  final case object FlowOut extends Context { val name = "flow out" }
  final case object BlockIn extends Context { val name = "block in" }
  final case object BlockOut extends Context { val name = "block out" }
  final case object BlockKey extends Context { val name = "block key" }
  final case object FlowKey extends Context { val name = "flow key" }

  implicit class RichRegex(val regex: scala.util.matching.Regex) {
    def matches(s: CharSequence): Boolean = regex.pattern.matcher(s).matches
    def find(s: CharSequence): Boolean = regex.pattern.matcher(s).find
  }

  implicit class CharSequenceToInt(cs: CharSequence) {
    def toInt(radix: Int): Int = Integer.parseInt(cs.toString, radix)
  }
}
