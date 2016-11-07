
organization := "fr.iscpif"

name := "viability"

def settings =
  Seq(
    scalaVersion := "2.11.8",
    javacOptions in (Compile, compile) ++= Seq("-source", "1.7", "-target", "1.7"),
    scalacOptions += "-target:jvm-1.7",
    publishArtifact := false
  )

scalariformSettings

lazy val defaultSettings =
  settings ++ Seq(
    organization := "fr.iscpif.viability",
    publishArtifact := true,
    publishTo := isSnapshot { snapshot =>
      val nexus = "https://oss.sonatype.org/"
      if (snapshot) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2")
    }.value,
    pomIncludeRepository := { _ => false},
    licenses := Seq("Affero GPLv3" -> url("http://www.gnu.org/licenses/")),
    homepage := Some(url("https://github.com/ISCPIF/viabilitree")),
    //scmInfo := Some(ScmInfo("scm:git:git.iscpif.fr/viability", "scm:git:git@git.iscpif.fr:viability.git")),
    // To sync with Maven central, you need to supply the following information:
    pomExtra := {
      <!-- Developer contact information -->
        <developers>
          <developer>
            <id>romainreuillon</id>
            <name>Romain Reuillon</name>
            <url>https://github.com/romainreuillon/</url>
          </developer>
          <developer>
            <id>isabelle</id>
            <name>Isabelle Alvarez</name>
          </developer>
          <developer>
            <id>ricardo</id>
            <name>Ricardo De Aldama</name>
          </developer>
        </developers>
    }
  )


/* ---- Viablity -----*/

lazy val kdtree = Project(id = "kdtree", base = file("kdtree"), settings = defaultSettings) settings (
  libraryDependencies ++= monocle
  ) dependsOn(geometry)

lazy val export = Project(id = "export", base = file("export"), settings = defaultSettings) dependsOn(kdtree, viability) settings (
  libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3",
  libraryDependencies += "com.thoughtworks.xstream" % "xstream" % "1.4.7"
  )

lazy val viability = Project(id = "viability", base = file("viability"), settings = defaultSettings) dependsOn(kdtree, model)

lazy val model = Project(id = "model", base = file("model"), settings = defaultSettings) settings (
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.4.1"
  ) dependsOn(geometry)

lazy val strategy = Project(id = "strategy", base = file("strategy"), settings = defaultSettings) dependsOn(viability, geometry, kdtree)

lazy val geometry = Project(id = "geometry", base = file("geometry"), settings = defaultSettings)


/*----- Exemples -----*/

lazy val lotkavoltera = Project(id = "lotkavoltera", base = file("example/lotkavoltera")) dependsOn(viability, export, model)

lazy val cyclic = Project(id = "cyclic", base = file("example/cyclic")) dependsOn(viability, export, model)

lazy val consumer = Project(id = "consumer", base = file("example/consumer")) dependsOn(viability, export, model)

lazy val population = Project(id = "population", base = file("example/population")) dependsOn(viability, export, model)

lazy val lake = Project(id = "lake", base = file("example/lake")) dependsOn(viability, export, model, strategy)

lazy val bilingual = Project(id = "bilingual", base = file("example/bilingual")) dependsOn(viability, export, model)


/*----- Libraries ------ */

lazy val monocleVersion = "0.5.1"

lazy val monocle = Seq(
  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion
)

