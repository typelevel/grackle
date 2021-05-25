val attoVersion                 = "0.9.5"
val catsVersion                 = "2.6.1"
val catsEffectVersion           = "2.4.1"
val catsTestkitScalaTestVersion = "2.1.5"
val doobieVersion               = "0.13.2"
val fs2Version                  = "2.5.6"
val http4sVersion               = "0.21.22"
val kindProjectorVersion        = "0.11.3"
val literallyVersion            = "1.0.2"
val logbackVersion              = "1.2.3"
val log4catsVersion             = "1.3.1"
val skunkVersion                = "0.0.28"
val shapelessVersion            = "2.3.4"
val sourcePosVersion            = "1.0.0"
val testContainersVersion       = "0.39.5"
val typenameVersion             = "1.0.0"

val Scala2 = "2.13.5"
val Scala3 = "3.0.0"

inThisBuild(Seq(
  homepage := Some(url("https://github.com/gemini-hlsw/gsp-graphql")),
  scalaVersion := Scala2,
  crossScalaVersions := Seq(Scala2, Scala3),
))

lazy val commonSettings = Seq(
  //scalacOptions --= Seq("-Wunused:params", "-Wunused:imports", "-Wunused:patvars", "-Wdead-code", "-Wunused:locals", "-Wunused:privates", "-Wunused:implicits"),
  libraryDependencies ++= Seq(
    "org.typelevel"     %% "cats-testkit"           % catsVersion % "test",
    "org.typelevel"     %% "cats-testkit-scalatest" % catsTestkitScalaTestVersion % "test"
  ) ++ Seq(
    compilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full),
  ).filterNot(_ => scalaVersion.value.startsWith("3.")),
  headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
  headerLicense  := Some(HeaderLicense.Custom(
    """|Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
        |For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
        |""".stripMargin
  ))
)

lazy val noPublishSettings = Seq(
  publish / skip := true
)

lazy val modules: List[ProjectReference] = List(
  core,
  circe,
  sql,
  doobie,
  skunk,
  generic,
  demo
)

lazy val `gsp-graphql` = project.in(file("."))
  .settings(commonSettings)
  .settings(noPublishSettings)
  .aggregate(modules:_*)
  .disablePlugins(RevolverPlugin)
  .settings(
    makeSite := { (docs / makeSite).value }
  )

lazy val core = project
  .in(file("modules/core"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-core",
    libraryDependencies ++= {
      val circeVersion = scalaVersion.value match {
        case Scala3 => "0.14.0-M7"
        case Scala2 => "0.13.0"
      }
      Seq(
        "org.tpolecat"      %% "atto-core"              % attoVersion,
        "org.typelevel"     %% "cats-core"              % catsVersion,
        "org.typelevel"     %% "literally"              % literallyVersion,
        "io.circe"          %% "circe-core"             % circeVersion,
        "io.circe"          %% "circe-parser"           % circeVersion,
        "org.tpolecat"      %% "typename"               % typenameVersion,
        "org.tpolecat"      %% "sourcepos"              % sourcePosVersion,
        "co.fs2"            %% "fs2-core"               % fs2Version,
      )
    }
  )

lazy val circe = project
  .in(file("modules/circe"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-circe",
  )

lazy val sql = project
  .in(file("modules/sql"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(circe)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-sql",
    libraryDependencies ++= Seq(
      "org.typelevel"     %% "log4cats-slf4j"         % log4catsVersion,
      "ch.qos.logback"    %  "logback-classic"        % logbackVersion % "test",
      "com.dimafeng"      %% "testcontainers-scala-scalatest"  % testContainersVersion % "test",
      "com.dimafeng"      %% "testcontainers-scala-postgresql" % testContainersVersion % "test",
    )
  )

lazy val doobie = project
  .in(file("modules/doobie"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(sql % "test->test;compile->compile", circe)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-doobie",
    Test / fork := true,
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "doobie-core"           % doobieVersion,
      "org.tpolecat" %% "doobie-postgres-circe" % doobieVersion,
    )
  )

lazy val skunk = project
  .in(file("modules/skunk"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(sql % "test->test;compile->compile", circe)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-skunk",
    Test / fork := true,
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(
      "org.tpolecat"      %% "skunk-core"  % skunkVersion,
      "org.tpolecat"      %% "skunk-circe" % skunkVersion,
    )
  )

lazy val generic = project
  .in(file("modules/generic"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    publish / skip := scalaVersion.value.startsWith("3."),
    name := "gsp-graphql-generic",
    libraryDependencies ++= Seq(
      "com.chuusai"       %% "shapeless"              % shapelessVersion
    ).filterNot(_ => scalaVersion.value.startsWith("3."))
  )

// TODO: re-enable when http4s is available for 3.0.0-RC3
lazy val demo = project
  .in(file("demo"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core, generic)
  .settings(commonSettings)
  .settings(
    publish / skip := true,
    name := "gsp-graphql-demo",
    libraryDependencies ++= Seq(
      "org.typelevel"     %% "log4cats-slf4j"         % log4catsVersion,
      "ch.qos.logback"    %  "logback-classic"        % logbackVersion,
      "org.tpolecat"      %% "doobie-core"            % doobieVersion,
      "org.tpolecat"      %% "doobie-postgres"        % doobieVersion,
      "org.http4s"        %% "http4s-blaze-server"    % http4sVersion,
      "org.http4s"        %% "http4s-blaze-client"    % http4sVersion,
      "org.http4s"        %% "http4s-circe"           % http4sVersion,
      "org.http4s"        %% "http4s-dsl"             % http4sVersion
    ).filterNot(_ => scalaVersion.value.startsWith("3."))
  )

lazy val docs = project
  .in(file("docs"))
  .enablePlugins(ParadoxSitePlugin)
  .settings(
    paradoxTheme         := Some(builtinParadoxTheme("generic")),
    previewLaunchBrowser := false,
    paradoxProperties ++= Map(
      "scala-versions"          -> (core / crossScalaVersions).value.map(CrossVersion.partialVersion).flatten.map(_._2).mkString("2.", "/", ""),
      "org"                     -> organization.value,
      "scala.binary.version"    -> s"2.${CrossVersion.partialVersion(scalaVersion.value).get._2}",
      "core-dep"                -> s"${(core / name).value}_2.${CrossVersion.partialVersion(scalaVersion.value).get._2}",
      "version"                 -> version.value
    )
  )
