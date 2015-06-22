import sbt._

object MyBuild extends Build {

  lazy val root = Project("root", file(".")) dependsOn(cla) dependsOn(chisel)
  lazy val cla = RootProject(uri("git://github.com/djmmoss/cla.git"))
  lazy val chisel = RootProject(uri("git://github.com/da-steve101/chisel.git"))

}
