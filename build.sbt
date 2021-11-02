ThisBuild / scalaVersion := "2.13.6"

ThisBuild / organization := "com.example"

lazy val hello = (project in file("."))
  .settings(
    name := "Hello",
    libraryDependencies += "org.jsoup" % "jsoup" % "1.14.3"
  )