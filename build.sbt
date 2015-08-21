organization := "usyd.edu.au"

version := "0.1"

name := "chisel-pipelined-olk"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-language:reflectiveCalls")

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.2.1"
libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.6"
