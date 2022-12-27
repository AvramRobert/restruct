val scala3Version = "3.2.1"
val parserCombinatorVersion = "2.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "restruct",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.scala-lang.modules" %% "scala-parser-combinators" % parserCombinatorVersion),
  )
