ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "cats"
  )

libraryDependencies += "org.typelevel" %% "cats-core" % "2.8.0"

//scalacOptions ++= Seq(
//  "-Xfatal-warnings",
//)
