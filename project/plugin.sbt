resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.9.2")

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.8.0")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.0")

addSbtPlugin( "com.typesafe.sbt" % "sbt-osgi" % "0.9.2" )

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.6")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "2.0")
