import sbt._
import Keys._

object ViabilityLanguagesBuild extends Build {
 lazy val root = Project(id = "viabilityLanguages", base = file("viabilityLanguages")) aggregate(languages, kdtree) dependsOn(languages, kdtree)

 lazy val languages = Project(id = "languages", base = file("languages"))  

 lazy val kdtree = Project(id = "kdtree", base = file("kdtree")) 

  

version := "1.0-SNAPSHOT"

}

