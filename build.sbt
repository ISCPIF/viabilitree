
organization := "fr.iscpif"
name := "viabilitree"

scalaVersion in ThisBuild := "2.12.4"

publishTo in ThisBuild := isSnapshot { snapshot =>
  val nexus = "https://oss.sonatype.org/"
  if (snapshot) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}.value

pomIncludeRepository in ThisBuild := { _ => false}

licenses in ThisBuild := Seq("Affero GPLv3" -> url("http://www.gnu.org/licenses/"))

homepage in ThisBuild := Some(url("https://github.com/ISCPIF/viabilitree"))

scmInfo in ThisBuild := Some(ScmInfo(url("https://github.com/ISCPIF/viabilitree.git"), "scm:git:git@github.com:ISCPIF/viabilitree.git"))
// To sync with Maven central, you need to supply the following information:
pomExtra in ThisBuild := {
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

libraryDependencies in ThisBuild += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies in ThisBuild += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

scalariformSettings(autoformat = true)

lazy val defaultSettings =
  Seq(
    organization := "fr.iscpif.viabilitree",
    publishArtifact := true,
    addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
  )

/* ---- Viability -----*/



lazy val kdtree = Project(id = "kdtree", base = file("kdtree")) settings(defaultSettings: _*) settings (
  libraryDependencies ++= monocle,
  libraryDependencies += cats,
  libraryDependencies += simulacrum)

lazy val approximation = Project(id = "approximation", base = file("approximation")) settings(defaultSettings: _*) dependsOn(kdtree)

lazy val export = Project(id = "export", base = file("export")) settings(defaultSettings: _*) dependsOn(kdtree, approximation, viability) settings (
  libraryDependencies += "com.github.pathikrit" %% "better-files" % "2.17.1",
  libraryDependencies += "com.thoughtworks.xstream" % "xstream" % "1.4.7")

lazy val viability = Project(id = "viability", base = file("viability")) settings(defaultSettings: _*) dependsOn(kdtree, approximation, model)

lazy val model = Project(id = "model", base = file("model"))  settings(defaultSettings: _*) settings (
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1")

lazy val strategy = Project(id = "strategy", base = file("strategy")) settings(defaultSettings: _*) dependsOn(viability, kdtree)

/*----- Exemples -----*/

//lazy val lotkavoltera = Project(id = "lotkavoltera", base = file("example/lotkavoltera")) settings(settings: _*) dependsOn(viability, export, model)

//lazy val cyclic = Project(id = "cyclic", base = file("example/cyclic")) settings(settings: _*) dependsOn(viability, export, model)

//lazy val consumer = Project(id = "consumer", base = file("example/consumer")) settings(settings: _*) dependsOn(viability, export, model)

lazy val population =
  Project(id = "population", base = file("example/population")) settings(
    publishArtifact := false,
    OsgiKeys . exportPackage := Seq ( "viabilitree.*", "fr.iscpif.population.*"),
    OsgiKeys . importPackage := Seq ( "*;resolution:=optional" ),
    OsgiKeys . privatePackage := Seq ( "*" ),
    OsgiKeys . requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))"""") dependsOn(viability, export, model) enablePlugins(SbtOsgi)

lazy val lake = Project(id = "lake", base = file("example/lake")) settings(publishArtifact := false) dependsOn(viability, export, model, strategy)

lazy val bilingual = Project(id = "bilingual", base = file("example/bilingual")) settings(publishArtifact := false) dependsOn(viability, export, model)

lazy val circle = Project(id = "circle", base = file("example/circle")) settings(publishArtifact := false) dependsOn(kdtree, export)

lazy val raz13 = Project(id = "raz13", base = file("example/raz13")) settings(publishArtifact := false) dependsOn(viability, export, model, strategy)

/*----- Libraries ------ */

lazy val monocleVersion = "1.3.2"

lazy val monocle = Seq(
  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion
)

lazy val cats = "org.typelevel" %% "cats-core" % "1.0.1"

lazy val simulacrum = "com.github.mpilquist" %% "simulacrum" % "0.11.0"




