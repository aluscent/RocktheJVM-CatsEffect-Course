ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "RocktheJVM-CatsEffect"
  )

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.5.0",
  "org.typelevel" %% "cats-core" % "2.9.0"
)
