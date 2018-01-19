name := "yams"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.8.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

scalacOptions         ++= "-deprecation -feature -Xlint:-stars-align,-nullary-unit,_".split("\\s+").to[Seq]
scalacOptions in Test  += "-Xxml:coalescing"

val yams = project.in(file("."))
  .settings(
    apiMappings ++= Map(
      scalaInstance.value.libraryJar
        -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/")
    ) ++ {
      // http://stackoverflow.com/questions/16934488
      Option(System.getProperty("sun.boot.class.path")).flatMap { classPath =>
        classPath.split(java.io.File.pathSeparator).find(_.endsWith(java.io.File.separator + "rt.jar"))
      }.map { jarPath =>
        Map(
          file(jarPath)
            -> url("http://docs.oracle.com/javase/8/docs/api")
        )
      } getOrElse {
        // If everything fails, jam in the Java 9 base module.
        Map(
          file("/modules/java.base")
            -> url("http://docs.oracle.com/javase/9/docs/api"),
          file("/modules/java.xml")
            -> url("http://docs.oracle.com/javase/9/docs/api")

        )
      }
    }
  )
