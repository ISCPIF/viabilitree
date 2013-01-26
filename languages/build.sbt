scalacOptions += "-unchecked"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

organization := "fr.iscpif"

name := "languages"

version := "1.0-SNAPSHOT"

libraryDependencies ++= List(
              "org.apache.commons" % "commons-math3" % "3.0"
            //"org.scalanlp" % "breeze-math_2.9.2" % "0.1",
            //"org.scalanlp" % "breeze-learn_2.9.2" % "0.1",
            //"org.scalanlp" % "breeze-process_2.9.2" % "0.1",
            //"org.scalanlp" % "breeze-viz_2.9.2" % "0.1"              
)

resolvers ++= Seq(
            // other resolvers here
            // if you want to use snapshot builds (currently 0.2-SNAPSHOT), use this.            
            "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
            "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
            "Test" at "https://oss.sonatype.org/content/groups/scala-tools/"	    
)

resolvers += "Local Maven Repository" at "file://home/aldama/.m2/repository"

version := "1.0-SNAPSHOT"
//version := "0.10"

//scalaVersion := "2.9.2"
scalaVersion := "2.10.0"

scalaVersion in ThisBuild := "2.10.0"
