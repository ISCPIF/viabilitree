import sbt._
import Keys._

object ViabilityRootBuild extends Build { 

 //lazy val viabilityRoot = Project(id = "viabilityRoot", base = file(".")) aggregate(viabilityLanguages, viabilityCabbage) dependsOn(viabilityLanguages, viabilityCabbage, kdtree)

 lazy val kdtree = Project(id = "kdtree", base = file("kdtree"))
 
 lazy val viability = Project(id = "viability", base = file("viability")) dependsOn(kdtree)

 lazy val languages = Project(id = "languages", base = file("languages"))

 lazy val viabilityLanguages = Project(id = "viabilityLanguages", base = file("viabilityLanguages")) dependsOn(languages, viability, kdtree)

 lazy val viabilityCabbage = Project(id = "viabilityCabbage", base = file("viabilityCabbage")) dependsOn(viability, kdtree)

}

