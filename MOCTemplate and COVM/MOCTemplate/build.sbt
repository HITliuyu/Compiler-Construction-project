name := "MOC"

organization := "de.tuberlin.uebb"

version := "0.1.0"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-deprecation", "-feature")

seq(com.typesafe.sbt.SbtStartScript.startScriptForClassesSettings: _*)

libraryDependencies +=
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

libraryDependencies +=
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies +=
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3"

libraryDependencies +=
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3"
