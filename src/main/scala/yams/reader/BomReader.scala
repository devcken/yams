package yams.reader

/** Reader for reading The Byte Order Marks from streams, files, etc.
  * 
  * The byte order mark is a unicode character, U+FEFF, whose appearance as a magic number 
  * at the start of a text stream can signal several things to a program consuming the text:
  * 
  * - What byte order, or endianness, the text stream is stored in;
  * - The fact that the text stream is Unicode, to a high level of confidence;
  * - Which of several Unicode encodings that text stream is encoded as.
  * 
  * BOM use is optional, and, if used, should appear at the start of the text stream.
  * 
  * @see [[http://yaml.org/spec/1.2/spec.html#id2771184]]
  * @see [[https://en.wikipedia.org/wiki/Byte_order_mark]].
  * 
  * @author Leejun Choi
  * @since 0.1
  */
object BomReader extends com.typesafe.scalalogging.LazyLogging {
  import util.parsing.input.StreamReader
  
  /** Max size of BOM. */
  private val BomSize = 4

  /** {{{
    *   [3] c-byte-order-mark ::= #xFEFF
    * }}}
    * 
    * @see [[http://yaml.org/spec/1.2/spec.html#c-byte-order-mark]]
    */
  @deprecated("`c-byte-order-mark` isn't subject to parse. This value was declared in a symbolical meaning.", "0.0")
  final val ByteOrderMark = "[\\x{FEFF}]"

  def read(stream: java.io.InputStream): StreamReader = {
    this.read(new java.io.PushbackInputStream(stream, BomSize))
  }

  /** Reads the given stream, including the byte array representing the Byte Order Mark, and Returns
    * the string from which the BOM is removed.
    * 
    * @param stream the stream to read
    */
  private def read(stream: java.io.PushbackInputStream): StreamReader = {
    import java.nio.charset.Charset.forName
    
    val bom = new Array[Byte](BomSize)
    val read = stream.read(bom, 0, bom.length)

    val (charset, unread) = bom match {
      case Array(0, 0, -2, -1)      /* 00 00 FE FF */ => (forName("UTF-32BE"), read - 4)
      case Array(-1, -2, 0, 0)      /* FF FE 00 00 */ => (forName("UTF-32LE"), read - 4)
      case Array(-2, -1, _, _)      /* FE FF       */ => (forName("UTF-16BE"), read - 2)
      case Array(-1, -2, _, _)      /* FF FE       */ => (forName("UTF-16LE"), read - 2)
      case Array(-17, -69, -65, _)  /* EF BB BF    */ => (forName("UTF-8"), read - 3)
      case _                        /* (default)   */ => (forName("UTF-8"), read)
    }
    
    logger.debug(charset + bom.map(" 0x%02x" format _).mkString)
    
    if (unread > 0) {
      stream.unread(bom, read - unread, unread)
    }
    
    StreamReader(new java.io.InputStreamReader(stream, charset.newDecoder()))
  }
}
