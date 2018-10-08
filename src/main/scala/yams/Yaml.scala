package yams

import java.io.InputStream

import scala.util.parsing.input.Position

/**
  * The processor for loading and dumping the YAML document.
  *
  * Translating between native data structures and a character stream is done in several logically distinct stages,
  * each with a well defined input and output data model, as shown in the following diagram:
  *
  * @see <a href="http://yaml.org/spec/1.2/spec.html#id2762140">Figure 3.1. Processing Overview</a>
  *
  * This class provide two primary processes: load and dump.
  *
  */
class Yaml {
  def load(s: String) = ???
  def load(s: InputStream) = ???

  def dump[A](data: A): String = ???
}

/*
           Application ¦ YAML
           ------------¦--------------------------- Dump ----------------------------->
                       ¦
                ╭ Represent ╮               ╭ Serialize ╮    ╭ Present ╮
   ╔══════════════════╗¦╔══════════════════════════╗ ╔═══════════════╗ ╔═════════════════════════════╗
   ║      Native      ║¦║      Representation      ║ ║ Serialization ║ ║         Presentation        ║
   ║ (Data Structure) ║¦║       (Node Graph)       ║ ║  (Event Tree) ║ ║      (Character Stream)     ║
   ╟------------------╢¦╟--------------------------╢ ╟---------------╢ ╟-----------------------------╢
   ║      opaque      ║¦║           tags           ║ ║    anchors    ║ ║       styles, comments      ║
   ║     program      ║¦║ mapping/sequence/scalar, ║ ║    aliases,   ║ ║     directives, spacing     ║
   ║       data       ║¦║ canonical string values  ║ ║   key order   ║ ║ formatted string values,... ║
   ╚══════════════════╝¦╚══════════════════════════╝ ╚═══════════════╝ ╚═════════════════════════════╝
                ╰ Construct ╯                ╰ Compose ╯      ╰ Parse ╯
                       ¦
            <----------¦--------------------------- Load ------------------------------
 */

case class YamlLoadError(position: Position, message: String)
