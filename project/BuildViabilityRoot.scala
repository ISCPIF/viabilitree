import sbt._
import Keys._

object ViabilityRootBuild extends Build { 

 override def settings = super.settings ++ Seq(scalaVersion := "2.10.2")

 lazy val all = Project(id = "all", base = file(".")) aggregate(kdtree, visualisation, viability, lotkavoltera) dependsOn(kdtree, visualisation, viability, lotkavoltera)
 
 lazy val kdtree = Project(id = "kdtree", base = file("kdtree"))
 
 lazy val visualisation = Project(id = "visualisation", base = file("visualisation")) dependsOn(kdtree) settings (
   libraryDependencies ++= Seq("com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2")
   )

 lazy val viability = Project(id = "viability", base = file("viability")) dependsOn(kdtree)

 lazy val lotkavoltera = Project(id = "lotkavoltera", base = file("example/lotkavoltera")) dependsOn(viability)

} 

