name := "millfork"

version := "0.3.23-SNAPSHOT"

scalaVersion := "2.12.11"

resolvers += Resolver.mavenLocal

libraryDependencies += "com.lihaoyi" %% "fastparse" % "1.0.0"

libraryDependencies += "org.apache.commons" % "commons-configuration2" % "2.2"

libraryDependencies += "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.9.0"

libraryDependencies += "net.liftweb" %% "lift-json" % "3.4.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

val testDependencies = Seq(
  "com.codingrodent.microprocessor" % "Z80Processor" % "2.0.2" % "test",
  // see: https://github.com/NeatMonster/Intel8086
  "NeatMonster" % "Intel8086" % "1.0" % "test" from "https://github.com/NeatMonster/Intel8086/raw/master/IBMPC.jar",
  // these three are not in Maven Central or any other public repo
  // get them from the following links or just build millfork without tests:
  // https://github.com/sethm/symon/tree/71905fdb1998ee4f142260879504bc46cf27648f
  // https://github.com/andrew-hoffman/halfnes/tree/061
  // https://github.com/trekawek/coffee-gb/tree/coffee-gb-1.0.0
  // https://github.com/sorenroug/osnine-java/tree/b77349a6c314e1362e69b7158c385ac6f89b7ab8
  "com.loomcom.symon" % "symon" % "1.3.0-SNAPSHOT" % "test",
  "com.grapeshot" % "halfnes" % "061" % "test",
  "eu.rekawek.coffeegb" % "coffee-gb" % "1.0.0" % "test",
  "roug.org.osnine" % "osnine-core" % "2.0-SNAPSHOT" % "test",
  "org.graalvm.sdk" % "graal-sdk" % "20.2.0" % "test",
  "org.graalvm.js" % "js" % "20.2.0" % "test",
  "org.graalvm.js" % "js-scriptengine" % "20.2.0" % "test"
)

val includesTests = System.getProperty("skipTests") == null

libraryDependencies ++=(
  if (includesTests) {
    println("Including test dependencies")
    testDependencies
  } else {
    Seq[ModuleID]()
  }
)

(if (!includesTests) {
  // Disable assembling tests
  sbt.internals.DslEntry.fromSettingsDef(test in assembly := {})
} else {
  sbt.internals.DslEntry.fromSettingsDef(Seq[sbt.Def.Setting[_]]())
})

mainClass in Compile := Some("millfork.Main")

assemblyJarName := "millfork.jar"

lazy val root = (project in file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "millfork.buildinfo"
  )

import sbtassembly.AssemblyKeys

val releaseDist =
  TaskKey[File]("release-dist", "Creates a distributable zip file.")

releaseDist := {
  val jar = AssemblyKeys.assembly.value
  val base = Keys.baseDirectory.value
  val target = Keys.target.value
  val name = Keys.name.value
  val version = Keys.version.value
  val distDir = target / (name + "-" + version)
  val releasesDir = base / "releases"
  val zipFile = releasesDir / (name + "-" + version + ".zip")
  IO.delete(zipFile)
  IO.delete(distDir)
  IO.createDirectory(releasesDir)
  IO.createDirectory(distDir)
  IO.copyFile(jar, distDir / jar.name)
  IO.copyFile(base / "LICENSE", distDir / "LICENSE")
  IO.copyFile(
    base / "src/3rd-party-licenses.txt",
    distDir / "3rd-party-licenses.txt"
  )
  IO.copyFile(base / "CHANGELOG.md", distDir / "CHANGELOG.md")
  IO.copyFile(base / "README.md", distDir / "README.md")
  IO.copyFile(base / "COMPILING.md", distDir / "COMPILING.md")
  def copyDir(name: String): Unit = {
    IO.createDirectory(distDir / name)
    IO.copyDirectory(base / name, distDir / name)
  }
  copyDir("include")
  copyDir("docs")
  def entries(f: File): List[File] =
    f :: (if (f.isDirectory) IO.listFiles(f).toList.flatMap(entries) else Nil)
  IO.zip(
    entries(distDir).map(d =>
      (d, d.getAbsolutePath.substring(distDir.getParent.length + 1))
    ),
    zipFile
  )
  IO.delete(distDir)
  zipFile
}
