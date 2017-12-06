name := "millfork"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.3"

resolvers += Resolver.mavenLocal

libraryDependencies += "com.lihaoyi" %% "fastparse" % "1.0.0"

libraryDependencies += "org.apache.commons" % "commons-configuration2" % "2.2"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

// these two not in Maven Central or any other public repo
// get them from the following links or just build millfork without tests:
// https://github.com/sethm/symon
// https://github.com/andrew-hoffman/halfnes/tree/061

libraryDependencies += "com.loomcom.symon" % "symon" % "1.3.0-SNAPSHOT" % "test"

libraryDependencies += "com.grapeshot" % "halfnes" % "061" % "test"

mainClass in Compile := Some("millfork.Main")

assemblyJarName := "millfork.jar"

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "millfork.buildinfo"
  )