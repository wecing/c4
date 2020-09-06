name := "c4"
version := "1.0"

scalaVersion := "2.13.3"
scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"

PB.targets in Compile := Seq(
  scalapb.gen(flatPackage = true) -> (sourceManaged in Compile).value / "scala"
)
PB.protoSources in Compile := Seq(
  sourceDirectory.value / "main" / "resources"
)

testOptions in Test += Tests.Argument("-oF")

assemblyJarName in assembly := "parser.jar"
