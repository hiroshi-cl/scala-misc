name := "scala-macro"

val defaultSetting = Seq(
  organization := "jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi",
  version := "1.0-SNAPSHOT",
  scalaVersion := "2.11.4",
  javacOptions ++= Seq("-source", "1.8"),
  scalacOptions ++= Seq("-feature", "-deprecation", "-optimise", "-unchecked", "-explaintypes", "-Xlint"),
  // additional optimise
  scalacOptions ++= Seq("-Yclosure-elim", "-Yconst-opt", "-Ydead-code", "-Yinline", "-Yinline-handlers"),
  resolvers ++= Seq(
    "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
  ),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % "2.11.4",
    "org.scala-lang" % "scala-library" % "2.11.4",
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.1" % "test"
  )
)

lazy val infixify = project.settings(defaultSetting:_*)

lazy val macro_base = project.dependsOn(infixify).settings(defaultSetting:_*)

lazy val byname_macro = project.dependsOn(infixify, macro_base).settings(defaultSetting:_*)

