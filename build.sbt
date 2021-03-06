import AssemblyKeys._

import SonatypeKeys._

sonatypeSettings


name := "regexphonetic"

organization := "com.github.luzhuomi"

version := "0.1.1-SNAPSHOT"

// scalaVersion := "2.9.2"


resolvers += "Apache HBase" at "https://repository.apache.org/content/repositories/releases"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Maven Repository" at "http://mvnrepository.com/artifact/"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.4"


seq(assemblySettings: _*)


mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case PathList("log4j.properties") => MergeStrategy.discard
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case _ => MergeStrategy.last // leiningen build files
  }
}


publishMavenStyle := true



publishArtifact in Test := false

pomIncludeRepository := { _ => false }


pomExtra := (
  <url>http://luzhuomi.github.com/regexphonetic</url>
  <licenses>
    <license>
      <name>BSD-style</name>
      <url>http://www.opensource.org/licenses/bsd-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:luzhuomi/scalazparsec.git</url>
    <connection>scm:git:git@github.com:luzhuomi/regexphonetic.git</connection>
  </scm>
  <developers>
    <developer>
      <id>luzhuomi</id>
      <name>Kenny Zhuo Ming Lu</name>
      <url>http://sites.google.com/site/luzhuomi</url>
    </developer>
  </developers>)
