name := "aoc20"
version := "0.0.1-SNAPSHOT"

scalaVersion := "3.0.0-RC3"

lazy val Fs2Version = "3.0.2"

libraryDependencies += "co.fs2" %% "fs2-core" % Fs2Version
libraryDependencies += "co.fs2" %% "fs2-io" % Fs2Version

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.8" % "test"

scalacOptions ++= {
  Seq(
    "-feature",
    // disabled during the migration
    // "-Xfatal-warnings"
    "-unchecked",
    "-source:3.0",
  )
}

Global / onChangedBuildSource := ReloadOnSourceChanges
