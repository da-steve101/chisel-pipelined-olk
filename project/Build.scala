import sbt._

object MyBuild extends Build {

  lazy val root = Project("root", file(".")) dependsOn(cla)
  lazy val cla = RootProject(uri("git://github.com/djmmoss/cla.git"))

}
