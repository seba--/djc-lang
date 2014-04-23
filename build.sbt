name := "djc-lang"

version := "0.0.0"

scalaVersion := "2.11.0"

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

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.1.3" % "test"

