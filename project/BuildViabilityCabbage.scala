import sbt._
import Keys._


object ViabilityCabbageBuild extends Build {
  lazy val root = Project(id = "viabilityCabbage", base = file("viabilityCabbage")) aggregate(kdtree)//, cabbage) dependsOn(kdtree)//, cabbage)

  lazy val kdtree = Project(id = "kdtree", base = file("kdtree")) //dependsOn(languages)  

  //lazy val cabbage = Project(id = "cabbage", base = file("../ExternalCode/cabbage")) //dependsOn(kdtree)


version := "1.0-SNAPSHOT"

}

