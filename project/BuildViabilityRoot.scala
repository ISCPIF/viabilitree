import sbt._
import Keys._
import com.typesafe.sbt.osgi.SbtOsgi.{OsgiKeys, osgiSettings}

object ViabilityRootBuild extends Build { 

  override def settings = 
    super.settings ++ Seq(scalaVersion := "2.10.3")

  lazy val kdtree = Project(id = "kdtree", base = file("kdtree")) settings (
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.3"
  )
 
  lazy val visualisation = Project(id = "visualisation", base = file("visualisation")) dependsOn(kdtree) settings (
    libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2"
  )

  lazy val viability = Project(id = "viability", base = file("viability")) dependsOn(kdtree)

  lazy val lotkavoltera = Project(id = "lotkavoltera", base = file("example/lotkavoltera")) dependsOn(viability, visualisation, differential)
 
  lazy val cyclic = Project(id = "cyclic", base = file("example/cyclic")) dependsOn(viability, visualisation, differential)

  lazy val differential = Project(id = "differential", base = file("example/differential"))
 
  lazy val consumer = Project(id = "consumer", base = file("example/consumer")) dependsOn(viability, visualisation, differential)

  lazy val population = Project(id = "population", base = file("example/population"), settings = osgi) dependsOn(viability, visualisation, differential)

  lazy val lake = Project(id = "lake", base = file("example/lake"), settings = osgi) dependsOn(viability, visualisation, differential)

  lazy val bilingual = Project(id = "bilingual", base = file("example/bilingual"), settings = osgi) dependsOn(viability, visualisation, differential)

  def osgi = Project.defaultSettings ++ exports(Seq("fr.iscpif.*"), Seq("*;resolution:=optional"), Seq("!scala.*", "*"))

  def exports(packages: Seq[String] = Seq(), imports: Seq[String] = Nil, privates: Seq[String] = Nil) = osgiSettings ++ Seq(
    OsgiKeys.importPackage := imports,
    OsgiKeys.privatePackage := privates,
    OsgiKeys.exportPackage := packages
  )
}

