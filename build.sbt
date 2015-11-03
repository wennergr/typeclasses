name := "typeclasses"

version := "1.0"

scalaVersion := "2.11.5"

autoCompilerPlugins := true

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

libraryDependencies ++= Seq(
  "com.github.mpilquist" %% "simulacrum" % "0.3.0",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
)

