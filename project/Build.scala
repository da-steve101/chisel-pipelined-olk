import sbt._

object MyBuild extends Build {

  lazy val root = Project("root", file(".")) dependsOn(chisel)
  lazy val chisel = RootProject(uri("git://github.com/da-steve101/chisel.git"))

}
