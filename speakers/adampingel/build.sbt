name := "lambdaconf2015"

version := "0.1.0-SNAPSHOT"

organization := "org.axle-lang"

scalaVersion := "2.11.6"

resolvers += "sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

//resolvers += "spray repo" at "http://repo.spray.io"

libraryDependencies ++= Seq(
  "org.axle-lang" %% "axle-core" % "0.2.0-SNAPSHOT",
  "org.axle-lang" %% "axle-visualize" % "0.2.0-SNAPSHOT",
  "org.axle-lang" %% "axle-games" % "0.2.0-SNAPSHOT",
  "org.axle-lang" %% "axle-jblas" % "0.2.0-SNAPSHOT",
  "org.axle-lang" %% "axle-spark" % "0.2.0-SNAPSHOT"
)
