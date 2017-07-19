resolvers += Classpaths.sbtPluginReleases

resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"

addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.1.4")

//addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.4.2")
