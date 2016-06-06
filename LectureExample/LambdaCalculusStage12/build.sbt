name := "LambdaCalculusStage12"

organization := "de.tuberlin.uebb"

version := "0.1.0"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-Xmax-classfile-name", "100")

scalacOptions ++= Seq("-deprecation", "-feature")

seq(com.typesafe.sbt.SbtStartScript.startScriptForClassesSettings: _*)

libraryDependencies +=
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

libraryDependencies ++= Seq(
	"com.googlecode.kiama" %% "kiama" % "1.7.0"
)
