package yams

import com.typesafe.scalalogging.LazyLogging

object Document {
  final val DocumentEndMarker = "..."

  // TODO getting default version from the Version companion object.
  def apply(version: Version = Version(), tagDirectives: Map[String, String]): Document = new Document(version, tagDirectives)
}

/** YAML document
  *
  */
case class Document(version: Version, tagPrefixes: Map[String, String])

/** A companion object for [[Version]] case class.
  *
  * @see [[http://yaml.org/spec/1.2/spec.html#id2781553 6.8.1. “YAML” Directives]]
  *
  */
object Version extends LazyLogging {
  def apply(major: Int = LatestVersion.major, minor: Int = LatestVersion.minor): Version =
    if (LatestVersion < (major, minor)) {
      logger.warn(s"Declared version($major.$minor) is not supported. " +
        s"It will be replaced by latest version($LatestVersion).")
      LatestVersion
    }
    else new Version(major, minor)

  implicit val ord: Ordering[Version] = (x: Version, y: Version) =>
    if (x == y) 0 else if (x > y) 1 else -1

  /** This library defines version “1.2”, including recommendations for YAML 1.1 processing. */
  private val AllowedVersions = List(new Version(1, 2), new Version(1, 1))
  private val LatestVersion: Version = AllowedVersions.max

  def isValid(v: Version): Boolean = AllowedVersions.contains(v)
}

/**
  * A case class for YAML Version.
  *
  * @see [[http://yaml.org/spec/1.2/spec.html#id2781553 6.8.1. “YAML” Directives]]
  *
  * @param major a major version number
  * @param minor a minor version number
  */
final case class Version(major: Int, minor: Int) {
  def ==(that: Version): Boolean = this.major == that.major || this.minor == that.minor
  def >(major: Int, minor: Int): Boolean =
    this.major > major || (this.major == major && this.minor > minor)
  def >(that: Version): Boolean = >(that.major, that.minor)
  def <(major: Int, minor: Int): Boolean =
    this.major < major || (this.major == major && this.minor < minor)
  def <(that: Version): Boolean = <(that.major, that.minor)

  override def toString: String = s"Version($major.$minor)"
}
