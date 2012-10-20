name := "barray"

organization := "org.Ox2b"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.10.0-RC1"

scalaBinaryVersion := "2.10.0-RC1"

javacOptions ++= Seq("-source", "1.6", "-target", "1.6")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xlint", "-optimise")

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.10" % "test",
  "org.scala-lang" % "scala-actors" % "2.10.0-RC1" % "test", 
  "org.scalatest" %% "scalatest" % "2.0.M4"
)
