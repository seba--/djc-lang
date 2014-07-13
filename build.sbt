name := "djc-lang"

version := "0.0.0"

scalaVersion := "2.11.1"

scalacOptions ++= List(
	"-deprecation",
	"-encoding", "UTF-8",
	"-unchecked",
	"-feature",
	"-target:jvm-1.6",
	"-language:implicitConversions",
	"-language:reflectiveCalls",
	"-Xlint"
)

resolvers ++= Seq()

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"

org.scalastyle.sbt.ScalastylePlugin.Settings

// Create a default Scala style task to run with tests
//lazy val testScalaStyle = taskKey[Unit]("testScalaStyle")

//testScalaStyle := {
//  org.scalastyle.sbt.PluginKeys.scalastyle.toTask("").value
//}

//(test in Test) <<= (test in Test) dependsOn testScalaStyle
