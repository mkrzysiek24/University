inThisBuild(
  List(
    scalaVersion := "2.12.18",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    coverageEnabled := true
  )
)

lazy val commonSettings = Seq(
  organization := "com.uni",
  version := "0.1.0",
  scalaVersion := "2.12.18",
  semanticdbVersion := "4.9.7",
  scalacOptions := Seq(
    "-deprecation",
    "-encoding", "utf-8",
    "-explaintypes",
    "-feature",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xlint",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  ),
  Compile / scalafmtOnCompile := true,
  scalafixOnCompile := true
)

lazy val root = (project in file("."))
  .aggregate(core, blackjack)
  .settings(commonSettings)
  .settings(
    name := "CardGames"
  )

lazy val core = (project in file("core"))
  .settings(commonSettings)
  .settings(
    name := "Core",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.12" % Test
    )
  )

lazy val blackjack = (project in file("blackjack"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "Blackjack",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.12" % Test
    )
  )