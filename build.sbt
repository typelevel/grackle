val attoVersion                 = "0.7.0"
val catsVersion                 = "2.0.0"
val catsTestkitScalaTestVersion = "1.0.0-M1"
val circeVersion                = "0.12.1"
val jawnVersion                 = "0.14.2"
val kindProjectorVersion        = "0.10.3"

inThisBuild(Seq(
  homepage := Some(url("https://github.com/gemini-hlsw/gsp-graphql")),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % kindProjectorVersion),
  scalaVersion := "2.13.0"
) ++ gspPublishSettings)

lazy val core = project
  .in(file("modules/core"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(
    name := "gsp-graphql",
    libraryDependencies ++= Seq(
      "org.tpolecat"  %% "atto-core"              % attoVersion,
      "org.typelevel" %% "cats-core"              % catsVersion,
      "io.circe"      %% "circe-core"             % circeVersion,
      "io.circe"      %% "circe-literal"          % circeVersion,
      "org.typelevel" %% "jawn-parser"            % jawnVersion,
      "org.typelevel" %% "cats-testkit"           % catsVersion % "test",
      "org.typelevel" %% "cats-testkit-scalatest" % catsTestkitScalaTestVersion % "test",
    )
  )

