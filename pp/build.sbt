name := "c4"

version := "1.0"

scalaVersion := "2.12.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
libraryDependencies += "org.typelevel" %% "cats" % "0.9.0"

PB.targets in Compile := Seq(
  scalapb.gen(flatPackage = true) -> (sourceManaged in Compile).value / "scala"
)
PB.protoSources in Compile := Seq(
  sourceDirectory.value / "main" / "resources"
)

testOptions in Test += Tests.Argument("-oF")
