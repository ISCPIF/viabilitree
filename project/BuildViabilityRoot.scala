import sbt._
import Keys._

object ViabilityRootBuild extends Build { 

 override def settings = super.settings ++ Seq(scalaVersion := "2.10.2")

 lazy val all = Project(id = "all", base = file(".")) aggregate(kdtree, visualisation, viability, lotkavoltera, consumer, population) dependsOn(kdtree, visualisation, viability, lotkavoltera, cyclic, consumer)
 
 lazy val kdtree = Project(id = "kdtree", base = file("kdtree"))
 
 lazy val visualisation = Project(id = "visualisation", base = file("visualisation")) dependsOn(kdtree) settings (
   libraryDependencies ++= Seq("com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2")
   )

 lazy val viability = Project(id = "viability", base = file("viability")) dependsOn(kdtree)

 lazy val lotkavoltera = Project(id = "lotkavoltera", base = file("example/lotkavoltera")) dependsOn(viability, visualisation, differential)
 
  lazy val cyclic = Project(id = "cyclic", base = file("example/cyclic")) dependsOn(viability, visualisation, differential)

  lazy val differential = Project(id = "differential", base = file("example/differential"))
 
  lazy val consumer = Project(id = "consumer", base = file("example/consumer")) dependsOn(viability, visualisation, differential)

  lazy val population = Project(id = "population", base = file("example/population")) dependsOn(viability, visualisation, differential)
} 

