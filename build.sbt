
import com.typesafe.sbt.pgp.PgpKeys._

organization := "fr.iscpif"

name := "viability"

packagedArtifacts in file(".") := Map.empty

publish in file(".") := {}

publishSigned := {}

scalariformSettings

