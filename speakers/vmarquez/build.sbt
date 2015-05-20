name := "save your stack talk"

version := ".1-SNAPSHOT"

scalaVersion := "2.11.4"

resolvers ++= Seq("Sonatype Nexus releases" at "https://oss.sonatype.org/content/repositories/releases",
                 "Sonatype Nexus snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
                "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")

libraryDependencies ++= Seq("org.scalaz" % "scalaz-effect_2.11" %  "7.2.0-M1", "joda-time" % "joda-time" % "2.5")

initialCommands in console := "import scalaz._;import Scalaz._;import scala.concurrent.Future; import scala.reflect.runtime.universe.reify; import scala.concurrent.ExecutionContext.Implicits.global;"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:higherKinds")
