package object yams {
  implicit class RichRegex(val regex: scala.util.matching.Regex) {
    def matches(s: CharSequence): Boolean = regex.pattern.matcher(s).matches
    def find(s: CharSequence): Boolean = regex.pattern.matcher(s).find
  }

  implicit class CharSequenceToInt(cs: CharSequence) {
    def toInt(radix: Int): Int = Integer.parseInt(cs.toString, radix)
  }

  private val LineSeparator = scala.util.Properties.lineSeparator

  implicit class BufferedSourceExtension(val source: scala.io.BufferedSource) {
    def lines: List[String] = {
      def gatherLine(iterator: Iterator[Char], acc: StringBuilder = new StringBuilder): StringBuilder = {
        if (iterator.hasNext) {
          val ch = iterator.next()
          acc.append(ch)

          ch match {
            case '\r' if LineSeparator.length == 2 => acc.append(iterator.next)
            case '\r' | '\n' => acc
            case _ => gatherLine(iterator, acc)
          }
        } else acc
      }

      val iterator = source.iter

      (for (_ <- Stream range (0, Int.MaxValue)) yield gatherLine(iterator).toString)
        .takeWhile(x => !x.isEmpty).toList
    }
  }
}
