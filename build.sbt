name := "scala-macro"

val defaultSetting = Seq(
  organization := "jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi",
  version := "1.0-SNAPSHOT",
  scalaVersion := "2.11.6",
  javacOptions  ++= Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions ++= Seq("-feature", "-deprecation", "-optimise", "-unchecked", "-explaintypes", "-Xlint"),
  // additional optimise
  //scalacOptions ++= Seq("-Yclosure-elim", "-Yconst-opt", "-Ydead-code", "-Yinline", "-Yinline-handlers"),
  resolvers ++= Seq(
    "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
  ),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    "org.scala-lang" % "scala-library" % scalaVersion.value,
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.1" % "test",
    "junit" % "junit" % "4.12" % "test"
  )
)

lazy val infixify = project.settings(defaultSetting:_*)

lazy val macro_base = project.dependsOn(infixify).settings(defaultSetting:_*)

lazy val java8_adapter = project.dependsOn(infixify, macro_base).settings(defaultSetting: _*)

lazy val byname_macro = project.dependsOn(infixify, macro_base).settings(defaultSetting:_*)

lazy val cps_macro = project.dependsOn(infixify, macro_base, byname_macro).settings(defaultSetting: _*)

lazy val cps_macro_new = project.dependsOn(infixify, macro_base, byname_macro).settings(defaultSetting: _*)

lazy val ticket_macro = project.dependsOn(infixify, macro_base, byname_macro).settings(defaultSetting: _*)

lazy val dsls = project.dependsOn(byname_macro, cps_macro, ticket_macro).settings(defaultSetting: _*)
