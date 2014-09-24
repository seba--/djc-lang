name := "djc-lang"

version := "0.0.0"

scalaVersion := "2.11.2"

scalacOptions ++= List(
	"-deprecation",
	"-encoding", "UTF-8",
	"-unchecked",
	"-feature",
	"-language:implicitConversions",
	"-language:reflectiveCalls",
	"-Xlint"
)

resolvers ++= Seq()

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.0-R4" 

//javaFX related stuff
unmanagedJars in Compile += Attributed.blank(file(System.getenv("JAVA_HOME") + "/jre/lib/ext/jfxrt.jar"))

fork in run := true

org.scalastyle.sbt.ScalastylePlugin.Settings

// Create a default Scala style task to run with tests
//lazy val testScalaStyle = taskKey[Unit]("testScalaStyle")

//testScalaStyle := {
//  org.scalastyle.sbt.PluginKeys.scalastyle.toTask("").value
//}

//(test in Test) <<= (test in Test) dependsOn testScalaStyle
