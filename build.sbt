name := "c4"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

testOptions in Test += Tests.Argument("-oF")
