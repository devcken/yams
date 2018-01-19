package yams.reader

/** Tests for [[BomReader]].
  * 
  * @author Leejun Choi
  * @since 0.1
  * @see [[http://yaml.org/spec/1.2/spec.html#id2771184]]
  */
class BomReaderTest extends org.scalatest.FreeSpec {
  private val Expected = "test"

  List(
    "utf-8",
    "utf-16be", "utf-16le",
    "utf-32be", "utf-32le"
  ).foreach(p => p.toUpperCase in {
    val stream = BomReader.read(this.getClass.getClassLoader.getResourceAsStream(s"reader/$p.txt"))
    assert(stream.source.toString == Expected)
  })
}
