import sbt._
import Keys._

object HelloBuild extends Build {
  lazy val root = Project(id = "root", base = file(".")) aggregate(kdtree, languages, cabbage) dependsOn(kdtree, languages, cabbage)

  lazy val kdtree = Project(id = "kdtree", base = file("kdtree")) //dependsOn(languages, cabbage)

  lazy val languages = Project(id = "languages", base = file("languages")) //dependsOn(root)

  lazy val cabbage = Project(id = "cabbage", base = file("cabbage")) //dependsOn(kdtree)


version := "1.0-SNAPSHOT"

}
