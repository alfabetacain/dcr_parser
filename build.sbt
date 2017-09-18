scalaVersion := "2.12.3"

version := "1.0"

name := "DCR Parser"

organization := "dk.itu.dcr"

libraryDependencies += "org.scala-lang" % "scala-xml" % "2.11.0-M4"
val scalaTestVersion = "3.0.1"
libraryDependencies += "org.scalactic" %% "scalactic" % scalaTestVersion
libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test"

