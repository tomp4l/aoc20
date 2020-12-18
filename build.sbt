name := "aoc20"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.13.4"

lazy val Fs2Version = "3.0.0-M6"

libraryDependencies += "co.fs2" %% "fs2-core" % Fs2Version
libraryDependencies += "co.fs2" %% "fs2-io" % Fs2Version
libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"

addCompilerPlugin(
  "org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full,
)

scalacOptions -= "-Xlint:nullary-override"
