organization := "usyd.edu.au"

version := "0.1"

name := "chisel-pipelined-olk"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-language:reflectiveCalls")
val chiselVersion = System.getProperty("chiselVersion", "latest.release")
resolvers += "typesafe" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= ( if (chiselVersion != "None" ) ("edu.berkeley.cs" %% "chisel" % chiselVersion) :: Nil; else Nil)
libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.2.1"
libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.7"
