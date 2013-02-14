scalacOptions += "-unchecked"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

organization := "fr.iscpif"

name := "viability-languages"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.0"

scalaVersion in ThisBuild := "2.10.0"

//libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.2.0"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2"
