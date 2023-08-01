import nl.zolotko.sbt.jfr.{JfrRecording, JfrRecorderOptions}
import scala.concurrent.duration.DurationInt

val catsVersion            = "2.9.0"
val catsParseVersion       = "0.3.10"
val catsEffectVersion      = "3.1.1"
val circeVersion           = "0.14.5"
val disciplineMunitVersion = "2.0.0-M3"
val doobieVersion          = "1.0.0-RC4"
val flywayVersion          = "9.21.1"
val fs2Version             = "3.8.0"
val http4sVersion          = "0.23.23"
val http4sBlazeVersion     = "0.23.15"
val jnrUnixsocketVersion   = "0.38.20"
val kindProjectorVersion   = "0.13.2"
val literallyVersion       = "1.1.0"
val logbackVersion         = "1.4.8"
val log4catsVersion        = "2.6.0"
val munitVersion           = "1.0.0-M8"
val munitCatsEffectVersion = "2.0.0-M3"
val skunkVersion           = "0.6.0"
val shapeless2Version      = "2.3.10"
val shapeless3Version      = "3.1.0"
val sourcePosVersion       = "1.1.0"
val typenameVersion        = "1.1.0"
val whaleTailVersion       = "0.0.10"

val Scala2 = "2.13.11"
val Scala3 = "3.3.0"
ThisBuild / scalaVersion        := Scala2
ThisBuild / crossScalaVersions  := Seq(Scala2, Scala3)

ThisBuild / tlBaseVersion    := "0.14"
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
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("11"))

lazy val commonSettings = Seq(
  //scalacOptions --= Seq("-Wunused:params", "-Wunused:imports", "-Wunused:patvars", "-Wdead-code", "-Wunused:locals", "-Wunused:privates", "-Wunused:implicits"),
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit"             % munitVersion % "test",
    "org.scalameta" %%% "munit-scalacheck"  % munitVersion % "test",
    "org.typelevel" %%% "cats-laws"         % catsVersion % "test",
    "org.typelevel" %%% "discipline-munit"  % disciplineMunitVersion % "test",
    "org.typelevel" %%% "munit-cats-effect" % munitCatsEffectVersion % "test",
    "io.circe"      %%% "circe-literal"     % circeVersion % "test",
    "io.circe"      %%% "circe-jawn"        % circeVersion % "test",
  ) ++ Seq(
    compilerPlugin("org.typelevel" %% "kind-projector" % kindProjectorVersion cross CrossVersion.full),
  ).filterNot(_ => tlIsScala3.value),
  headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
  headerLicense  := Some(HeaderLicense.Custom(
    """|Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
       |For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
       |""".stripMargin
  )),
  // Temporary workaround for https://github.com/lampepfl/dotty/issues/15927
  Compile / doc / sources := {
      val old = (Compile / doc / sources).value
      scalaVersion.value match {
        case Scala3 => Seq()
        case Scala2 => old
      }
    }
)

lazy val modules: List[CompositeProject] = List(
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

lazy val `gsp-graphql` = tlCrossRootProject
  .aggregate(modules:_*)
  .disablePlugins(RevolverPlugin)
  .settings(
    makeSite := { (docs / makeSite).value }
  )

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/core"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-core",
    libraryDependencies ++=
      Seq(
        "org.typelevel" %%% "cats-parse"   % catsParseVersion,
        "org.typelevel" %%% "cats-core"    % catsVersion,
        "org.typelevel" %%% "literally"    % literallyVersion,
        "io.circe"      %%% "circe-core"   % circeVersion,
        "org.tpolecat"  %%% "typename"     % typenameVersion,
        "org.tpolecat"  %%% "sourcepos"    % sourcePosVersion,
        "co.fs2"        %%% "fs2-core"     % fs2Version,
      )
  )
  .jsSettings(
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.5.0",
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
  )

lazy val circe = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/circe"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-circe",
  )

lazy val buildInfo = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/build-info"))
  .enablePlugins(BuildInfoPlugin, NoPublishPlugin)
  .settings(commonSettings)
  .settings(
    buildInfoKeys += "baseDirectory" -> (LocalRootProject / baseDirectory).value.toString
  )

lazy val sql = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("modules/sql"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(core % "test->test;compile->compile", circe, buildInfo % Test)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-sql",
    libraryDependencies ++= Seq(
      "io.circe"          %%% "circe-generic"      % circeVersion % "test",
      "co.fs2"            %%% "fs2-io"             % fs2Version % "test",
    )
  )
  .platformsSettings(JVMPlatform, JSPlatform)(
    libraryDependencies ++= Seq(
      "io.chrisdavenport" %%% "whale-tail-manager" % whaleTailVersion % "test",
    )
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "com.github.jnr"    % "jnr-unixsocket"      % jnrUnixsocketVersion % "test"
    )
  )

lazy val doobie = project
  .in(file("modules/doobie-pg"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(sql.jvm % "test->test;compile->compile", circe.jvm)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-doobie-pg",
    Test / fork := true,
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(
      "org.tpolecat"   %% "doobie-core"           % doobieVersion,
      "org.tpolecat"   %% "doobie-postgres-circe" % doobieVersion,
      "org.typelevel"  %% "log4cats-core"         % log4catsVersion,
      "ch.qos.logback" %  "logback-classic"       % logbackVersion % "test"
    )
  )

lazy val skunk = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("modules/skunk"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(sql % "test->test;compile->compile", circe)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-skunk",
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(
      "org.tpolecat" %%% "skunk-core"  % skunkVersion,
      "org.tpolecat" %%% "skunk-circe" % skunkVersion,
    )
  )
  .jvmSettings(
    Test / fork := true
  )
  .jsSettings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
  )

lazy val generic = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/generic"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-generic",
    libraryDependencies += (
      scalaVersion.value match {
        case Scala3 => "org.typelevel" %%% "shapeless3-deriving" % shapeless3Version
        case Scala2 => "com.chuusai"   %%% "shapeless"           % shapeless2Version
      })
  )

lazy val demo = project
  .in(file("demo"))
  .enablePlugins(NoPublishPlugin, AutomateHeaderPlugin)
  .dependsOn(core.jvm, generic.jvm, doobie)
  .settings(commonSettings)
  .settings(
    name := "gsp-graphql-demo",
    libraryDependencies ++= Seq(
      "org.typelevel"     %% "log4cats-slf4j"      % log4catsVersion,
      "ch.qos.logback"    %  "logback-classic"     % logbackVersion,
      "org.tpolecat"      %% "doobie-core"         % doobieVersion,
      "org.tpolecat"      %% "doobie-postgres"     % doobieVersion,
      "org.tpolecat"      %% "doobie-hikari"       % doobieVersion,
      "org.http4s"        %% "http4s-blaze-server" % http4sBlazeVersion,
      "org.http4s"        %% "http4s-blaze-client" % http4sBlazeVersion,
      "org.http4s"        %% "http4s-circe"        % http4sVersion,
      "org.http4s"        %% "http4s-dsl"          % http4sVersion,
      "org.flywaydb"      %  "flyway-core"         % flywayVersion,
      "io.chrisdavenport" %% "whale-tail-manager"  % whaleTailVersion,
      "com.github.jnr"    % "jnr-unixsocket"       % jnrUnixsocketVersion
    )
  )

lazy val benchmarks = project
  .in(file("benchmarks"))
  .dependsOn(core.jvm)
  .enablePlugins(NoPublishPlugin, JmhPlugin)

lazy val profile = project
  .in(file("profile"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(core.jvm)
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
  .enablePlugins(SitePreviewPlugin, ParadoxSitePlugin)
  .settings(
    paradoxTheme         := Some(builtinParadoxTheme("generic")),
    previewLaunchBrowser := false,
    paradoxProperties ++= Map(
      "scala-versions"          -> (core.jvm / crossScalaVersions).value.map(CrossVersion.partialVersion).flatten.map(_._2).mkString("2.", "/", ""),
      "org"                     -> organization.value,
      "scala.binary.version"    -> s"2.${CrossVersion.partialVersion(scalaVersion.value).get._2}",
      "core-dep"                -> s"${(core.jvm / name).value}_2.${CrossVersion.partialVersion(scalaVersion.value).get._2}",
      "version"                 -> version.value
    )
  )
