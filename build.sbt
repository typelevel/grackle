val attoVersion                 = "0.8.0"
val catsVersion                 = "2.1.1"
val catsEffectVersion           = "2.1.3"
val catsTestkitScalaTestVersion = "1.0.1"
val circeVersion                = "0.13.0"
val circeOpticsVersion          = "0.13.0"
val doobieVersion               = "0.9.0"
val http4sVersion               = "0.21.4"
val jawnVersion                 = "1.0.0"
val kindProjectorVersion        = "0.10.3"
val logbackVersion              = "1.2.3"
val log4catsVersion             = "1.1.1"
val shapelessVersion            = "2.3.3"
val testContainersVersion       = "0.37.0"

inThisBuild(Seq(
  homepage := Some(url("https://github.com/gemini-hlsw/gsp-graphql")),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % kindProjectorVersion),
  scalaVersion := "2.13.2"
) ++ gspPublishSettings)

lazy val commonSettings = Seq(
  //scalacOptions --= Seq("-Wunused:params", "-Wunused:imports", "-Wunused:patvars", "-Wdead-code", "-Wunused:locals", "-Wunused:privates", "-Wunused:implicits"),
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
  generic,
  demo
)

lazy val `gsp-graphql` = project.in(file("."))
  .settings(commonSettings)
  .settings(noPublishSettings)
  .aggregate(modules:_*)
  .disablePlugins(RevolverPlugin)
  .settings(
    makeSite := { (makeSite in docs).value }
  )

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
      "io.circe"          %% "circe-parser"           % circeVersion,
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
    fork in Test := true,
    parallelExecution in Test := false,
    libraryDependencies ++= Seq(
      "org.typelevel"     %% "cats-effect"            % catsEffectVersion,
      "io.chrisdavenport" %% "log4cats-slf4j"         % log4catsVersion,
      "org.tpolecat"      %% "doobie-core"            % doobieVersion,
      "org.tpolecat"      %% "doobie-postgres"        % doobieVersion,

      "ch.qos.logback"    %  "logback-classic"        % logbackVersion % "test",
      "com.dimafeng"      %% "testcontainers-scala-scalatest"  % testContainersVersion % "test",
      "com.dimafeng"      %% "testcontainers-scala-postgresql" % testContainersVersion % "test",
    )
  )

lazy val generic = project
  .in(file("modules/generic"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-generic",
    libraryDependencies ++= Seq(
      "org.tpolecat"      %% "atto-core"              % attoVersion,
      "org.typelevel"     %% "cats-core"              % catsVersion,
      "io.circe"          %% "circe-core"             % circeVersion,
      "io.circe"          %% "circe-literal"          % circeVersion,
      "io.circe"          %% "circe-optics"           % circeOpticsVersion,
      "io.circe"          %% "circe-parser"           % circeVersion,
      "org.typelevel"     %% "jawn-parser"            % jawnVersion,
      "com.chuusai"       %% "shapeless"              % shapelessVersion,
    )
  )

lazy val demo = project
  .in(file("demo"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core, generic)
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

lazy val docs = project
  .in(file("docs"))
  .enablePlugins(ParadoxSitePlugin)
  .settings(
    paradoxTheme         := Some(builtinParadoxTheme("generic")),
    previewLaunchBrowser := false,
    paradoxProperties ++= Map(
      "scala-versions"          -> (crossScalaVersions in core).value.map(CrossVersion.partialVersion).flatten.map(_._2).mkString("2.", "/", ""),
      "org"                     -> organization.value,
      "scala.binary.version"    -> s"2.${CrossVersion.partialVersion(scalaVersion.value).get._2}",
      "core-dep"                -> s"${(core / name).value}_2.${CrossVersion.partialVersion(scalaVersion.value).get._2}",
      "version"                 -> version.value
    )
  )
