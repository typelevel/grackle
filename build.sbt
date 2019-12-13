val attoVersion                 = "0.7.2"
val catsVersion                 = "2.0.0"
val catsEffectVersion           = "2.0.0"
val catsTestkitScalaTestVersion = "1.0.0-RC1"
val circeVersion                = "0.12.3"
val circeOpticsVersion          = "0.12.0"
val doobieVersion               = "0.8.6"
val http4sVersion               = "0.21.0-M6"
val jawnVersion                 = "0.14.3"
val kindProjectorVersion        = "0.10.3"
val logbackVersion              = "1.2.3"
val log4catsVersion             = "1.0.1"
val slf4jVersion                = "1.7.29"

inThisBuild(Seq(
  homepage := Some(url("https://github.com/gemini-hlsw/gsp-graphql")),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % kindProjectorVersion),
  scalaVersion := "2.13.1"
) ++ gspPublishSettings)

lazy val commonSettings = Seq(
  //scalacOptions --= Seq("-Wunused:params", "-Wunused:imports", "-Wunused:patvars", "-Wdead-code", "-Wunused:locals", "-Wunused:privates"),
  libraryDependencies ++= Seq(
    "org.typelevel"     %% "cats-testkit"           % catsVersion % "test",
    "org.typelevel"     %% "cats-testkit-scalatest" % catsTestkitScalaTestVersion % "test"
  )
)

lazy val noPublishSettings = Seq(
  skip in publish := true
)

lazy val modules: List[ProjectReference] = List(
  core,
  doobie,
  demo
)

lazy val `gsp-graphql` = project.in(file("."))
  .settings(commonSettings)
  .settings(noPublishSettings)
  .aggregate(modules:_*)
  .disablePlugins(RevolverPlugin)

lazy val core = project
  .in(file("modules/core"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-core",
    libraryDependencies ++= Seq(
      "org.tpolecat"      %% "atto-core"              % attoVersion,
      "org.typelevel"     %% "cats-core"              % catsVersion,
      "io.circe"          %% "circe-core"             % circeVersion,
      "io.circe"          %% "circe-literal"          % circeVersion,
      "io.circe"          %% "circe-optics"           % circeOpticsVersion,
      "org.typelevel"     %% "jawn-parser"            % jawnVersion,
    )
  )

lazy val doobie = project
  .in(file("modules/doobie"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-doobie",
    libraryDependencies ++= Seq(
      "org.typelevel"     %% "cats-effect"            % catsEffectVersion,
      "io.chrisdavenport" %% "log4cats-slf4j"         % log4catsVersion,
      "org.tpolecat"      %% "doobie-core"            % doobieVersion,

      "org.tpolecat"      %% "doobie-postgres"        % doobieVersion % "test",
      "org.slf4j"         %  "slf4j-simple"           % slf4jVersion % "test"
    )
  )

lazy val demo = project
  .in(file("demo"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core, doobie)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-demo",
    libraryDependencies ++= Seq(
      "org.typelevel"     %% "cats-effect"            % catsEffectVersion,
      "io.chrisdavenport" %% "log4cats-slf4j"         % log4catsVersion,
      "ch.qos.logback"    %  "logback-classic"        % logbackVersion,
      "org.tpolecat"      %% "doobie-core"            % doobieVersion,
      "org.tpolecat"      %% "doobie-postgres"        % doobieVersion,
      "org.http4s"        %% "http4s-blaze-server"    % http4sVersion,
      "org.http4s"        %% "http4s-blaze-client"    % http4sVersion,
      "org.http4s"        %% "http4s-circe"           % http4sVersion,
      "org.http4s"        %% "http4s-dsl"             % http4sVersion
    )
  )
