package yams.helper

import org.scalatest.FreeSpec

/** Extension for [[FreeSpec]] with [[ExampleReader]] 
  * 
  * @author Leejun Choi
  * @since 0.1
  */
class YamsSpec extends FreeSpec with ExampleReader

/** Provide help methods to read example from resources.
  *
  * @author Leejun Choi
  * @since 0.1
  */
trait ExampleReader {
  def readAll(p: String): String = io.Source.fromResource(p).getLines().mkString("\n")

  def readLine(p: String, l: Int): String = io.Source.fromResource(p).getLines.toList(l)
  def readLines(p: String, r: Range): String = io.Source.fromResource(p).getLines().slice(r.start, r.end + 1).mkString("\n")
}
