name := "scalafun"

version := "0.1"

scalaVersion := "2.13.0"
scalaOptions ++= Seq("-language:implicitConversions", "-deprecation")
libraryDependencies += "com.novovode" % "junit-interface" % "0.11" % Test
// Used for base64 encoding
libraryDependencies += "commons-codec" % "commons-codec" % "1.10"

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")
