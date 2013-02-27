scalacOptions += "-unchecked"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

organization := "fr.iscpif"

name := "viability-cabbage"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.0"

scalaVersion in ThisBuild := "2.10.0"

libraryDependencies += "fr.iscpif.cabbage" % "cabbage" % "1.0"

//resolvers += "Local Maven Repository" at ""+Path.userHome.asFile.toURI.toURL+".m2/repository"

//libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.2.0"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2"

resolvers += "Good Will" at "http://maven.iscpif.fr/public/"
