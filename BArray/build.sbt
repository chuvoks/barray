name := "barray"

organization := "org.Ox2b"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.10.0-M7"

javacOptions ++= Seq("-source", "1.7", "-target", "1.7")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xlint", "-optimise")

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE17)

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.10" % "test",
   "org.scalatest" % "scalatest_2.10.0-M7" % "1.9-2.10.0-M7-B1"
)
