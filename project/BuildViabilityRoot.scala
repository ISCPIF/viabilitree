import sbt._
import Keys._

object ViabilityRootBuild extends Build { 

 lazy val all = Project(id = "all", base = file(".")) aggregate(kdtree, visualisation) dependsOn(kdtree, visualisation)
 
 lazy val kdtree = Project(id = "kdtree", base = file("kdtree"))
 
 lazy val visualisation = Project(id = "visualisation", base = file("visualisation")) dependsOn(kdtree) settings (
   libraryDependencies ++= Seq("com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2")
   )

 lazy val viability = Project(id = "viability", base = file("viability")) dependsOn(kdtree)

 lazy val languages = Project(id = "languages", base = file("languages"))

 lazy val viabilityLanguages = Project(id = "viabilityLanguages", base = file("viabilityLanguages")) dependsOn(languages, viability, kdtree)

 lazy val viabilityCabbage = Project(id = "viabilityCabbage", base = file("viabilityCabbage")) dependsOn(viability, kdtree)


}

