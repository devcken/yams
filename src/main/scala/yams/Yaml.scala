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
trait YamlContext {
  val name: String
}
