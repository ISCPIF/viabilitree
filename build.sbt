version := "1.0-SNAPSHOT"

scalaVersion := "2.10.1"

libraryDependencies += "fr.iscpif.cabbage" % "cabbage" % "1.0"
 
resolvers += "Local Maven Repository" at ""+Path.userHome.asFile.toURI.toURL+".m2/repository" 

resolvers += "Good Will" at "http://maven.iscpif.fr/public/"
