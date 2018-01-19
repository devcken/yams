package yams.util.encode

/** URI characters for tags, as specified in [[http://www.ietf.org/rfc/rfc2396.txt RFC2396]], with 
  * the addition of the “[” and “]” for presenting IPv6 addresses as proposed in 
  * [[http://www.ietf.org/rfc/rfc2732.txt RFC2732]].
  *
  * By convention, any URI characters other than the allowed printable ASCII characters are first 
  * encoded in UTF-8, and then each byte is escaped using the “%” character. The YAML processor must 
  * not expand such escaped characters. Tag characters must be preserved and compared exactly as 
  * presented in the YAML stream, without any processing.
  *
  * The ns-dec-digit and the ns-ascii-letter is 'safe'. They don't need to be explicitly specified.
  * And the same applies to '%'.
  *
  * @see [[http://yaml.org/spec/1.2/spec.html#id2784064]]
  *
  */
object UriEncoder {
  private val SafeChars = "-=;?:@&=+$,_.!~*'()[]/"
  private val CharSetName = "UTF-8"
  
  private val Utf8Decoder: java.nio.charset.CharsetDecoder =
    java.nio.charset.Charset.forName(CharSetName)
      .newDecoder().onMalformedInput(java.nio.charset.CodingErrorAction.REPORT)
  private val UriEscaper: UnicodeEscaper = PercentEscaper(SafeChars, plusForSpace = false)

  def encode(s: String): String = UriEscaper.escape(s)

  def decode(b: java.nio.ByteBuffer): String = Utf8Decoder.decode(b).toString
  def decode(s: String): String = java.net.URLDecoder.decode(s, CharSetName)
}
