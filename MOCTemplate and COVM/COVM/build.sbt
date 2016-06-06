name := "COVM"

organization := "de.tuberlin.uebb"

version := "0.1.1"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-deprecation", "-feature")

seq(com.typesafe.sbt.SbtStartScript.startScriptForClassesSettings: _*)

libraryDependencies +=
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

libraryDependencies +=
  "com.github.scopt" %% "scopt" % "3.2.0"
