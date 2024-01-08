ThisBuild / organization := "dkoeplin"
ThisBuild / scalaVersion := "2.13.12"
ThisBuild / version      := "1.0.0"

val swing = "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
val scalatest = "org.scalatest" %% "scalatest" % "3.2.15" % Test

lazy val root = (project in file("."))
  .settings(
    name := "AoC",
    libraryDependencies += swing,
    libraryDependencies += scalatest
  )