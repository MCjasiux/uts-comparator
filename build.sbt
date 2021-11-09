ThisBuild / scalaVersion := "2.13.6"

ThisBuild / organization := "com.example"

lazy val hello = (project in file("."))
  .settings(
    name := "Hello",
    libraryDependencies ++= Seq(
      "org.jsoup" % "jsoup" % "1.14.3",
      "org.scalanlp" %% "breeze" % "1.2"
    )
  )