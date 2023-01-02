scalaVersion := "3.2.1"

name := "unnamed_lang"
version := "0.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.14" % Test,
  "org.scala-lang.modules" % "scala-parser-combinators_3" % "2.1.1",
)
