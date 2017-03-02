name := "testing-scala"

version := "0.1"

scalaVersion := "2.12.1"

libraryDependencies ++=  Seq(
  "org.scalatest" %% "scalatest" % "3.0.+" % "test" withSources() withJavadoc(),
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test" withSources() withJavadoc()
)
