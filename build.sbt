ThisBuild / scalaVersion := "3.3.4"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "au.provokke"

lazy val root = (project in file("."))
  .settings(
    name := "trellis",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.2" % Test,
    scalacOptions ++= Seq("-deprecation", "-feature")
  )
