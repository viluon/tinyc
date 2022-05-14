import scala.language.postfixOps
import scala.sys.process._

name := "tinyc"

version := "0.1"

scalaVersion := "2.13.8"

idePackagePrefix := Some("me.viluon.tinyc")

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.11"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % "test"
libraryDependencies += "org.typelevel" %% "simulacrum" % "1.0.1"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"
libraryDependencies += "org.typelevel" %% "kittens" % "2.3.2"
libraryDependencies += "org.jetbrains" % "annotations" % "23.0.0"
libraryDependencies += "com.monovore" %% "decline" % "2.2.0"

// For simulacrum
scalacOptions += "-Ymacro-annotations"

lazy val buildNativeDeps = taskKey[Unit]("Build native dependencies")
buildNativeDeps := {
  "./build-native-deps.sh" !
}

fork := true
javaOptions += "-Djava.library.path=lib/"
