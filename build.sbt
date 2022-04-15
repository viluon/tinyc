name := "tinyc"

version := "0.1"

scalaVersion := "2.13.8"

idePackagePrefix := Some("me.viluon.tinyc")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.11"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % "test"
libraryDependencies += "org.typelevel" %% "simulacrum" % "1.0.1"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"
libraryDependencies += "org.typelevel" %% "kittens" % "2.3.2"

// For simulacrum
scalacOptions += "-Ymacro-annotations"
