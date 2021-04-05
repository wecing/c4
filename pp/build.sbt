name := "c4"
version := "1.0"

scalaVersion := "2.13.3"
scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "com.github.vbmacher" % "java-cup-runtime" % "11b"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

PB.targets in Compile := Seq(
  scalapb.gen(flatPackage = true) -> (sourceManaged in Compile).value / "scala"
)
PB.protoSources in Compile := Seq(
  sourceDirectory.value / "main" / "resources"
)

sourceGenerators in Compile += Def.task {
  val jar = sourceDirectory.value / "main" / "resources" / "java-cup-11b.jar"
  val cup = sourceDirectory.value / "main" / "resources" / "parser.cup"
  val outDir = (sourceManaged in Compile).value / "java" / "c4" / "ast"

  val cacheFn = FileFunction.cached(outDir / ".cache") { _ =>
    scala.sys.process.Process("mkdir -p " + outDir).!
    scala.sys.process.Process(
      "java -jar " + jar + " -destdir " + outDir +
      " -parser C4Parser -symbols C4Symbols " + cup).!
    Set(outDir / "C4Parser.java", outDir / "C4Symbols.java")
  }
  cacheFn(Set(cup)).toSeq
}.taskValue

testOptions in Test += Tests.Argument("-oF")

assemblyJarName in assembly := "parser.jar"
