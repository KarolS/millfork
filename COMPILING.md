## Build instructions

#### Java compatibility note

It is recommended to use JDK 1.8 for building Millfork.  
If you have multiple Java versions installed, enter  
`set JAVA_HOME=PATH_TO_JAVA\bin    ` (Windows)  
`export JAVA_HOME=PATH_TO_JAVA/bin ` (Mac/Linux)  
(where PATH_TO_JAVA is a path to one of your JDK installations)
into the command line before running sbt to choose a JDK.

### Building without tests

Setting up the test suite for Millfork is tricky, so if you don't need the tests, you can remove them.

#### Prerequisites

* JDK 1.8

* sbt

#### Steps

* delete the `src/test` directory

* remove all test dependencies from `build.sbt`: 

      "org.scalatest" %% "scalatest"
      "com.codingrodent.microprocessor" % "Z80Processor"
      "NeatMonster" % "Intel8086"
      "com.loomcom.symon" % "symon"
      "com.grapeshot" % "halfnes"
      "eu.rekawek.coffeegb" % "coffee-gb"
      "roug.org.osnine" % "osnine-core"

* navigate to the project directory 

* run `sbt compile` to compile the project

* run `sbt assembly` to build the executable jar file, it should appear in `target/scala-2.12`

### Building with tests

Test suite is useful if you plan on modifying the compiler. Some test dependencies need manual installation.

#### Prerequisites

* JDK 1.8 with Nashorn (tests don't work on newer versions)

* sbt

* Maven

#### Steps

* download the dependencies that are not available in a central repository:

        https://github.com/sethm/symon/tree/71905fdb1998ee4f142260879504bc46cf27648f
        https://github.com/andrew-hoffman/halfnes/tree/061
        https://github.com/trekawek/coffee-gb/tree/coffee-gb-1.0.0
        https://github.com/sorenroug/osnine-java/tree/1b4e059c5886fe01e8901c70684f7eedefe65010
        
* for each of them, run `maven package` and `maven install`

* navigate to the project directory 

* run `sbt compile` to compile the project

* run `sbt assemble` to build the executable jar file, it should appear in `target/scala-2.12`

### Building a native executable

This is experimental. 

#### Prerequisites

* an executable jar with Millfork (see above)

* GraalVM 19 or later

* native-image

* other tools required by native-image, appropriate for your operating system and GraalVM version (for example, Windows SDK 7.1 on Windows) 

* a lot of RAM

#### Steps

* navigate to the directory containing the jar, most likely `target/scala-2.12`

* run `native-image -jar millfork.jar` to build a native executable for your operating system