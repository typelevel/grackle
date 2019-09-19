val attoVersion                 = "0.7.0"
val catsVersion                 = "2.0.0"
val catsEffectVersion           = "2.0.0"
val catsTestkitScalaTestVersion = "1.0.0-M1"
val circeVersion                = "0.12.1"
val circeOpticsVersion          = "0.12.0"
val doobieVersion               = "0.8.2"
val jawnVersion                 = "0.14.2"
val kindProjectorVersion        = "0.10.3"
val log4catsVersion             = "1.0.0"
val slf4jVersion                = "1.7.28"

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
      "org.tpolecat"      %% "atto-core"              % attoVersion,
      "org.typelevel"     %% "cats-core"              % catsVersion,
      "org.typelevel"     %% "cats-effect"            % catsEffectVersion,
      "io.circe"          %% "circe-core"             % circeVersion,
      "io.circe"          %% "circe-literal"          % circeVersion,
      "io.circe"          %% "circe-optics"           % circeOpticsVersion,
      "org.typelevel"     %% "jawn-parser"            % jawnVersion,

      "io.chrisdavenport" %% "log4cats-slf4j"         % log4catsVersion % "test",
      "org.slf4j"         %  "slf4j-simple"           % slf4jVersion % "test",
      "org.tpolecat"      %% "doobie-core"            % doobieVersion % "test",
      "org.tpolecat"      %% "doobie-postgres"        % doobieVersion % "test",
      "org.typelevel"     %% "cats-testkit"           % catsVersion % "test",
      "org.typelevel"     %% "cats-testkit-scalatest" % catsTestkitScalaTestVersion % "test"
    )
  )
