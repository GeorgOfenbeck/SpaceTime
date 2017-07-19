name := "SpaceTimedotty"

version := "1.0"

organization := "ETHZ"

scalaVersion := "0.2.0-RC1"
//scalaVersion := "2.11.11"

scalaSource in Compile <<= baseDirectory(_ / "src/main")

scalaSource in Test <<= baseDirectory(_ / "src/test")

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.scala-lang" % "scala-library" % scalaVersion.value


// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false
