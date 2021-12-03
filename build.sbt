val scala3Version = "3.0.1"

inThisBuild(
  List(
    version := "1.0.0",
    scalaVersion := scala3Version,
    scalafmtOnCompile := true
  )
)

lazy val root = project
  .in(file("."))
  .aggregate(shared, d1)
  .settings(
    name := "Advent of Code 2021"
  )

lazy val shared = project
  .in(file("shared"))
  .settings(
    name := "Shared"
  )

lazy val d1 = project
  .in(file("d1"))
  .dependsOn(shared)
  .settings(
    name := "Day 1"
  )

addCommandAlias("cd", "project")
addCommandAlias("ls", "projects")
addCommandAlias("c", "compile")
addCommandAlias("rel", "reload")
addCommandAlias("r", "run")