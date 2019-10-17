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
  doobie
)

lazy val `gsp-graphql` = project.in(file("."))
  .settings(commonSettings)
  .settings(noPublishSettings)
  .aggregate(modules:_*)

lazy val core = project
  .in(file("modules/core"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-core",
    //scalacOptions --= Seq("-Wunused:params", "-Wunused:imports"),
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
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-doobie",
    //scalacOptions --= Seq("-Wunused:params", "-Wunused:imports"),
    libraryDependencies ++= Seq(
      "org.typelevel"     %% "cats-effect"            % catsEffectVersion,
      "io.chrisdavenport" %% "log4cats-slf4j"         % log4catsVersion,
      "org.slf4j"         %  "slf4j-simple"           % slf4jVersion,
      "org.tpolecat"      %% "doobie-core"            % doobieVersion,

      "org.tpolecat"      %% "doobie-postgres"        % doobieVersion % "test"
    )
  )
