import nl.zolotko.sbt.jfr.{JfrRecording, JfrRecorderOptions}
import scala.concurrent.duration.DurationInt

val catsVersion                 = "2.8.0"
val catsParseVersion            = "0.3.8"
val catsEffectVersion           = "3.1.1"
val catsTestkitScalaTestVersion = "2.1.5"
val circeVersion                = "0.14.2"
val doobieVersion               = "1.0.0-RC2"
val flywayVersion               = "9.3.0"
val fs2Version                  = "3.1.1"
val http4sVersion               = "0.23.15"
val http4sBlazeVersion          = "0.23.12"
val kindProjectorVersion        = "0.13.2"
val literallyVersion            = "1.1.0"
val logbackVersion              = "1.2.11"
val log4catsVersion             = "2.4.0"
val skunkVersion                = "0.3.1"
val shapeless2Version           = "2.3.10"
val shapeless3Version           = "3.2.0"
val sourcePosVersion            = "1.0.1"
val testContainersVersion       = "0.40.10"
val typenameVersion             = "1.0.0"

val Scala2 = "2.13.8"
val Scala3 = "3.1.3"
ThisBuild / scalaVersion        := Scala2
ThisBuild / crossScalaVersions  := Seq(Scala2, Scala3)

ThisBuild / tlBaseVersion    := "0.6"
ThisBuild / organization     := "edu.gemini"
ThisBuild / organizationName := "Association of Universities for Research in Astronomy, Inc. (AURA)"
ThisBuild / startYear        := Some(2019)
ThisBuild / licenses         += (("BSD-3-Clause", new URL("https://opensource.org/licenses/BSD-3-Clause")))
ThisBuild / developers       := List(
  Developer("milessabin", "Miles Sabin", "miles@milessabin.com", url("http://milessabin.com/blog")),
  Developer("tpolecat",   "Rob Norris",  "rnorris@gemini.edu",   url("http://www.tpolecat.org")),
)

ThisBuild / tlCiReleaseBranches     := Seq("main")
ThisBuild / tlSonatypeUseLegacyHost := false
ThisBuild / githubWorkflowBuild     ~= { steps =>
  WorkflowStep.Sbt(
    commands = List("headerCheckAll"),
    name = Some("Check Headers"),
  ) +: steps
}

lazy val commonSettings = Seq(
  //scalacOptions --= Seq("-Wunused:params", "-Wunused:imports", "-Wunused:patvars", "-Wdead-code", "-Wunused:locals", "-Wunused:privates", "-Wunused:implicits"),
  libraryDependencies ++= Seq(
    "org.typelevel"     %% "cats-testkit"           % catsVersion % "test",
    "org.typelevel"     %% "cats-testkit-scalatest" % catsTestkitScalaTestVersion % "test"
  ) ++ Seq(
    compilerPlugin("org.typelevel" %% "kind-projector" % kindProjectorVersion cross CrossVersion.full),
  ).filterNot(_ => tlIsScala3.value),
  headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
  headerLicense  := Some(HeaderLicense.Custom(
    """|Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
        |For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
        |""".stripMargin
  ))
)

lazy val modules: List[ProjectReference] = List(
  core,
  circe,
  sql,
  doobie,
  skunk,
  generic,
  demo,
  benchmarks,
  profile
)

lazy val `gsp-graphql` = project.in(file("."))
  .settings(commonSettings)
  .enablePlugins(NoPublishPlugin)
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
    libraryDependencies ++=
      Seq(
        "org.typelevel"     %% "cats-parse"             % catsParseVersion,
        "org.typelevel"     %% "cats-core"              % catsVersion,
        "org.typelevel"     %% "literally"              % literallyVersion,
        "io.circe"          %% "circe-core"             % circeVersion,
        "io.circe"          %% "circe-parser"           % circeVersion,
        "org.tpolecat"      %% "typename"               % typenameVersion,
        "org.tpolecat"      %% "sourcepos"              % sourcePosVersion,
        "co.fs2"            %% "fs2-core"               % fs2Version,
      )
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
  .dependsOn(core % "test->test;compile->compile", circe)
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
  .in(file("modules/doobie-pg"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(sql % "test->test;compile->compile", circe)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-doobie-pg",
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
    name := "gsp-graphql-generic",
    libraryDependencies += (
      scalaVersion.value match {
        case Scala3 => "org.typelevel" %% "shapeless3-deriving" % shapeless3Version
        case Scala2 => "com.chuusai"   %% "shapeless"           % shapeless2Version
      })
  )

lazy val demo = project
  .in(file("demo"))
  .enablePlugins(NoPublishPlugin, AutomateHeaderPlugin)
  .dependsOn(core, generic, doobie)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-demo",
    libraryDependencies ++= Seq(
      "org.typelevel"     %% "log4cats-slf4j"                  % log4catsVersion,
      "ch.qos.logback"    %  "logback-classic"                 % logbackVersion,
      "org.tpolecat"      %% "doobie-core"                     % doobieVersion,
      "org.tpolecat"      %% "doobie-postgres"                 % doobieVersion,
      "org.tpolecat"      %% "doobie-hikari"                   % doobieVersion,
      "org.http4s"        %% "http4s-blaze-server"             % http4sBlazeVersion,
      "org.http4s"        %% "http4s-blaze-client"             % http4sBlazeVersion,
      "org.http4s"        %% "http4s-circe"                    % http4sVersion,
      "org.http4s"        %% "http4s-dsl"                      % http4sVersion,
      "com.dimafeng"      %% "testcontainers-scala-postgresql" % testContainersVersion,
      "org.flywaydb"      %  "flyway-core"                     % flywayVersion
    )
  )

lazy val benchmarks = project
  .in(file("benchmarks"))
  .dependsOn(core)
  .enablePlugins(NoPublishPlugin, JmhPlugin)

lazy val profile = project
  .in(file("profile"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(core)
  .dependsOn(doobie)
  .settings(commonSettings)
  .settings(
    jfrRecordings := Seq(
      JfrRecording(
        fileName = file("profile.jfr").toPath.some,
        name = "profile".some,
        delay = 5.seconds.some,
        disk = true.some,
        dumpOnExit = true.some,
        duration = 30.seconds.some,
        pathToGcRoots = true.some,
      )
    ),
    fork := true
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
