name := "SpaceTime"

version := "1.0"

organization := "ETHZ"

scalaVersion := "2.11.8"

scalaSource in Compile <<= baseDirectory(_ / "src/main")

scalaSource in Test <<= baseDirectory(_ / "src/test")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-library" % _ % "compile")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _ % "compile")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.2" // cross CrossVersion.full"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.0.0-M2"

libraryDependencies += "com.sksamuel.scrimage" %% "scrimage-core" % "2.1.7"

libraryDependencies += "com.sksamuel.scrimage" %% "scrimage-io-extra" % "2.1.7"

libraryDependencies += "com.sksamuel.scrimage" %% "scrimage-filters" % "2.1.7"

libraryDependencies += "org.tinyjee.jgraphx" % "jgraphx" % "3.4.1.3"

libraryDependencies += "ch.epfl.lara" % "scife_2.11" % "1.2.9"

libraryDependencies += "com.github.benhutchison" %% "scalaswingcontrib" % "1.7"

libraryDependencies += "com.github.wookietreiber" %% "scala-chart" % "latest.integration"

libraryDependencies += "com.github.wendykierp" % "JTransforms" % "3.1"

libraryDependencies += "com.itextpdf" % "itextpdf" % "5.5.6"

// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false
