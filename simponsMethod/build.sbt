ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

def file(str: String) = ???

lazy val root = (project in file("."))
  .settings(
    name := "simponsMethod"
  )
