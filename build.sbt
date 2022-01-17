ThisBuild / scalaVersion := "2.13.6"

ThisBuild / organization := "pl.edu.agh"
version := "1.0"

mainClass := Some("agh.ScalaFXHelloWorld")

fork := true

scalacOptions ++= Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-feature")

lazy val hello = (project in file("."))
  .settings(
    name := "uts-comparator"
  )

  libraryDependencies += "org.jsoup" % "jsoup" % "1.14.3"
  libraryDependencies += "org.scalanlp" %% "breeze" % "1.2"
  libraryDependencies += "org.scalafx" %% "scalafx" % "17.0.1-R26"
  libraryDependencies += "com.lihaoyi" %% "upickle" % "0.9.5"
libraryDependencies ++= {
  // Determine OS version of JavaFX binaries
  lazy val osName = System.getProperty("os.name") match {
    case n if n.startsWith("Linux") => "linux"
    case n if n.startsWith("Mac") => "mac"
    case n if n.startsWith("Windows") => "win"
    case _ => throw new Exception("Unknown platform!")
  }
  Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
    .map(m => "org.openjfx" % s"javafx-$m" % "16" classifier osName)
    
}