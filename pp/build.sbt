name := "c4"
version := "1.0"

scalaVersion := "3.0.0"
scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "com.github.vbmacher" % "java-cup-runtime" % "11b"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.6.1"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0"

Compile / PB.targets := Seq(
  scalapb.gen(flatPackage = true) -> (Compile / sourceManaged).value / "scala"
)
Compile / PB.protoSources := Seq(
  sourceDirectory.value / "main" / "resources"
)

Compile / sourceGenerators += Def.task {
  val jar = sourceDirectory.value / "main" / "resources" / "java-cup-11b.jar"
  val cup = sourceDirectory.value / "main" / "resources" / "parser.cup"
  val outDir = (Compile / sourceManaged).value / "java" / "c4" / "ast"

  val cacheFn = FileFunction.cached(outDir / ".cache") { _ =>
    scala.sys.process.Process("mkdir -p " + outDir).!
    scala.sys.process.Process(
      "java -jar " + jar + " -destdir " + outDir +
      " -parser C4Parser -symbols C4Symbols " + cup).!
    Set(outDir / "C4Parser.java", outDir / "C4Symbols.java")
  }
  cacheFn(Set(cup)).toSeq
}.taskValue

Test / testOptions += Tests.Argument("-oF")

assembly / assemblyJarName := "parser.jar"
