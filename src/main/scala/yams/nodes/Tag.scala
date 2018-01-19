package yams.nodes

import yams.util.encode.UriEncoder

/*
* 10.1. Failsafe Schema
*
* The failsafe schema is guaranteed to work with any YAML document. It is therefore the recommended schema
* for generic YAML tools. A YAML processor should therefore support this schema, at least as an option.
*
 */

/** 10.1.1.1. Generic Mapping
  *
  * Represents an associative container, where each key is unique in the association and mapped to exactly one value.
  * YAML places no restrictions on the type of keys; in particular, they are not restricted to being scalars.
  * Example bindings to native types include Perl’s hash, Python’s dictionary, and Java’s Hashtable.
  *
  * URI: tag:yaml.org,2002:map
  * Kind: Mapping
  *
  */
object MapTag extends Tag("map")

/** 10.1.1.2. Generic Sequence
  *
  * Represents a collection indexed by sequential integers starting with zero. Example bindings to native types
  * include Perl’s array, Python’s list or tuple, and Java’s array or Vector.
  *
  * URI: tag:yaml.org,2002:seq
  * Kind: Sequence
  *
  */
object SeqTag extends Tag("seq")

/** 10.1.1.3. Generic String
  *
  * Represents a Unicode string, a sequence of zero or more Unicode characters. This type is usually bound to
  * the native language’s string type, or, for languages lacking one (such as C), to a character array.
  *
  * URI: tag:yaml.org,2002:str
  * Kind: Scalar
  *
  */
object StrTag extends Tag("str")

/*
* 10.2. JSON Schema
*
* The JSON schema is the lowest common denominator of most modern computer languages, and allows parsing JSON files.
* A YAML processor should therefore support this schema, at least as an option. It is also strongly recommended
* that other schemas should be based on it.
*
 */

/** 10.2.1.1. Null
  *
  * Represents the lack of a value. This is typically bound to a native null-like value (e.g., undef in Perl,
  * None in Python). Note that a null is different from an empty string. Also, a mapping entry with some key and
  * a null value is valid, and different from not having that key in the mapping.
  *
  * URI: tag:yaml.org,2002:null
  * Kind: Scalar
  *
  */
object NullTag extends Tag("null")

/** 10.2.1.2. Boolean
  *
  * Represents a true/false value. In languages without a native Boolean type (such as C), is usually bound to
  * a native integer type, using one for true and zero for false.
  *
  * URI: tag:yaml.org,2002:bool
  * Kind: Scalar
  *
  */
object BoolTag extends Tag("bool")

/** 10.2.1.3. Integer
  *
  * Represents arbitrary sized finite mathematical integers. Scalars of this type should be bound to
  * a native integer data type, if possible.
  *
  * Some languages (such as Perl) provide only a “number” type that allows for both integer and
  * floating-point values. A YAML processor may use such a type for integers, as long as they round-trip properly.
  *
  * In some languages (such as C), an integer may overflow the native type’s storage capability.
  * A YAML processor may reject such a value as an error, truncate it with a warning, or find some other manner
  * to round-trip it. In general, integers representable using 32 binary digits should safely round-trip
  * through most systems.
  *
  * URI: tag:yaml.org,2002:int
  * Kind: Scalar
  *
  */
object IntTag extends Tag("int")

/** 10.2.1.4. Floating Point
  *
  * Represents an approximation to real numbers, including three special values (positive and negative infinity,
  * and “not a number”).
  *
  * Some languages (such as Perl) provide only a “number” type that allows for both integer and
  * floating-point values. A YAML processor may use such a type for floating-point numbers, as long as
  * they round-trip properly.
  *
  * Not all floating-point values can be stored exactly in any given native type. Hence a float value may change
  * by “a small amount” when round-tripped. The supported range and accuracy depends on the implementation,
  * though 32 bit IEEE floats should be safe. Since YAML does not specify a particular accuracy, using
  * floating-point mapping keys requires great care and is not recommended.
  *
  * URI: tag:yaml.org,2002:float
  * Kind: Scalar
  *
  */
object FloatTag extends Tag("float")

/*
* 10.3. Core Schema
*
* The Core schema is an extension of the JSON schema, allowing for more human-readable presentation of
* the same types. This is the recommended default schema that YAML processor should use unless instructed
* otherwise. It is also strongly recommended that other schemas should be based on it.
*
* 10.3.1. Tags
*
* The core schema uses the same tags as the JSON schema.
*
 */

/*
* 10.4. Other Schemas
*
* Language-Independent Types for YAML Version 1.1
* see http://yaml.org/type/
*
 */

/** Ordered Mapping Tag
  *
  * Ordered sequence of key:value pairs without duplicates.
  *
  * A common type used for modeling is the ordered list of named values, where duplicates are not allowed.
  * For example, this is the basic collection data type used by the PHP language.
  *
  * Most programming languages do not have a built-in native data type for supporting ordered maps.
  * Such data types are usually provided by libraries. If no such data type is available, an application may
  * resort to loading an “!!omap” into a native array of hash tables containing one key each.
  *
  * The “!!omap” tag may be given explicitly. Alternatively, the application may choose to implicitly type
  * a sequence of single-key mappings to ordered maps. In this case, an explicit “!seq” transfer must be
  * given to sequences of single-key mappings that do not represent ordered maps.
  *
  * URI: tag:yaml.org,2002:omap
  * Kind: Sequence
  *
  * @see [[http://yaml.org/type/omap.html]]
  */
object OmapTag extends Tag("omap")

/** Pairs Tag
  *
  * Ordered sequence of key:value pairs allowing duplicates.
  *
  * A common type used for modeling is the ordered list of named values, allowing duplicates.
  * For example, this is one of the basic constructs used to model XML data. YAML provides a shorthand notation
  * for representing this data type as a sequence of one-key mappings.
  *
  * Most programming languages do not have a built-in native data type for supporting pairs. Such data types are
  * usually provided by libraries. If no such data type is available, an application may resort to loading !!pairs
  * into a native array of hash tables containing one key each.
  *
  * The !!pairs tag may be given explicitly. Alternatively, the application may choose to implicitly type
  * a sequence of single-key mappings to pairs. In this case, an explicit !!seq transfer must be given to
  * sequences of single-key mappings that do not represent pairs.
  *
  * URI: tag:yaml.org,2002:pairs
  * Kind: Sequence
  *
  * @see [[http://yaml.org/type/pairs.html]]
  *
  */
object PairsTag extends Tag("pairs")

/** Set Tag
  *
  * Unordered set of non-equal values.
  *
  * A set is an unordered collection of nodes such that no two nodes are equal. While sets are a fundamental
  * mathematical type, YAML only supports them as the domains of a mapping. Hence the YAML syntax for a set is
  * a mapping with all-null values.
  *
  * Most programming languages do not have a built-in native data type for supporting sets. Such data types are
  * usually provided by libraries. If no such data type is available, an application may resort to loading
  * a “!!set” into a native hash table where all values are null.
  *
  * The “!!set” tag may be given explicitly. Alternatively, the application may choose to implicitly type mappings
  * with all-null values to sets. In this case, an explicit “!!map” transfer must be given to mappings that have
  * all-null values but do not represent sets.
  *
  * URI: tag:yaml.org,2002:set
  * Kind: Mapping
  *
  * @see [[http://yaml.org/type/set.html]]
  *
  */
object SetTag extends Tag("set")

/** Binary Tag
  *
  * A sequence of zero or more octets (8 bit values).
  *
  * Binary data is serialized using the base64 format as defined by RFC2045 (MIME), with the following notes:
  *
  * - The content is not restricted to lines of 76 characters or less.
  * - Characters other than the base64 alphabet, line breaks and white space are considered an error.
  *
  * Base64 is the recommended way to store opaque binary data in YAML files. Note that many forms of binary data
  * have internal structure that may benefit from being represented as YAML nodes (e.g. the Java serialization format).
  *
  * URI: tag:yaml.org,2002:binary
  * Kind: Scalar
  *
  * @see [[http://yaml.org/type/binary.html]]
  *
  */
object BinaryTag extends Tag("binary")

/** Merge Tag
  *
  * Specify one or more mappings to be merged with the current one.
  *
  * The “<<” merge key is used to indicate that all the keys of one or more specified maps should be inserted
  * into the current map. If the value associated with the key is a single mapping node, each of its key/value
  * pairs is inserted into the current mapping, unless the key already exists in it. If the value associated with
  * the merge key is a sequence, then this sequence is expected to contain mapping nodes and each of these nodes
  * is merged in turn according to its order in the sequence. Keys in mapping nodes earlier in the sequence
  * override keys specified in later mapping nodes.
  *
  * URI: tag:yaml.org,2002:merge
  * Kind: Scalar
  *
  * @see [[http://yaml.org/type/merge.html]]
  *
  */
object MergeTag extends Tag("merge")

/** Timestamp Tag
  *
  * A point in time.
  *
  * A timestamp value represents a single point in time. This can be serialized using a subset of the ISO8601
  * format and the formats proposed by the W3C note on datetime. In addition, a more relaxed format is also
  * supported for enhanced readability, using white space separation.
  *
  * If the time zone is omitted, the timestamp is assumed to be specified in UTC. The time part may be omitted
  * altogether, resulting in a date format. In such a case, the time part is assumed to be
  * 00:00:00Z (start of day, UTC).
  *
  * URI: tag:yaml.org,2002:timestamp
  * Kind: Scalar
  *
  * @see [[http://yaml.org/type/timestamp.html]]
  *
  */
object TimestampTag extends Tag("timestamp")

/** Value Tag
  *
  * Specify the default value of a mapping.
  *
  * The “=” key is used to denote the "default value" of a mapping. In some cases, it is useful to evolve
  * a schema so that a scalar value is replaced with a mapping. A processor may present a "scalar value"
  * method that provides the value directly if the node is a scalar or returns the value of this key if
  * the node is a mapping. If applications only access the scalar value through this interface then the schema
  * may freely grow over time replacing scalar values with richer data constructs without breaking older
  * processing systems.
  *
  * URI: tag:yaml.org,2002:value
  * Kind: Scalar
  *
  * @see [[http://yaml.org/type/value.html]]
  *
  */
object ValueTag extends Tag("value")

/** Yaml Tag
  *
  * Keys for encoding YAML in YAML.
  *
  * YAML encoding keys are used to denote YAML structure information. The in-memory representation of
  * these keys must be different from any value in any other type family. Specifically, these in-memory
  * values must not be implemented as strings. Normally, the encoding keys should not be used in serialized
  * YAML documents; the encoded YAML node is serialized instead.
  *
  * Encoding is useful when a YAML processor encounters a valid YAML value of an unknown tag.
  * For a schema-specific application, this is not different from encountering any other valid YAML document
  * that does not satisfy the schema. Such an application may safely use a processor that rejects any value of
  * any unknown tag, or discards the tag property with an appropriate warning and parses the value as if
  * the property was not present.
  *
  * For a schema-independent application (for example, a hypothetical YAML pretty print application),
  * this is not an option. Processors used by such applications should encode the value instead. This may be
  * done by wrapping the value in a mapping containing encoding keys. The “!” key denotes the unsupported tag.
  * In some cases it may be necessary to encode anchors and alias nodes as well. The “&” and “*” keys are used
  * for this purpose.
  *
  * Encoding should be reversed on output, allowing the schema-independent application to safely round-trip
  * any valid YAML document. In-memory, the encoded data may be accessed and manipulated in a standard way
  * using the three basic data types (mapping, sequence and scalar), allowing limited processing to be applied
  * to arbitrary YAML data.
  *
  * URI: tag:yaml.org,2002:yaml
  * Kind: Scalar
  *
  * @see [[http://yaml.org/type/yaml.html]]
  *
  */
object YamlTag extends Tag("yaml")

object Tag {
  final val DefaultPrefix = "tag:yaml.org,2002:"
}

class Tag(tag: Option[String]) extends Ordered[Tag] {
  /** A constructor for default-prefixed tags.
    *
    * @param tag
    */
  protected[nodes] def this(tag: String) {
    this(Some(s"${Tag.DefaultPrefix}$tag"))
  }

  val value = tag match {
    case Some(x) if x.length == 0 => throw new IllegalArgumentException("Tag must not be empty.")
    case Some(x) if x.trim.length != x.length => throw new IllegalArgumentException("Tag must not contain leading or trailing spaces.")
    case Some(x) => UriEncoder.encode(x)
    case _ => throw new NullPointerException("Tag must be provided.")
  }

  override def compare(that: Tag): Int = ???
}
