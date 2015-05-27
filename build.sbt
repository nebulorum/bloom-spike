name := "bloom-spike"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.6"

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases"),
  Resolver.typesafeRepo("releases")
)

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  "com.googlecode.kiama" % "kiama_2.11" % "2.0.0-SNAPSHOT"
)