name := "catscorelearn"

version := "0.1"

scalaVersion := "2.13.6"

libraryDependencies +=
  "org.typelevel" %% "cats-core" % "2.1.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.2.9"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9"
scalacOptions ++= Seq(
  "-Xfatal-warnings"
)
