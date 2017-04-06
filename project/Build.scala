import sbt._
import java.io.File

object LMSBuild extends Build {
  System.setProperty("showSuppressedErrors", "false")

  lazy val lms = Project("LMS", file("."))
}
