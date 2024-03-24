import nl.zolotko.sbt.jfr.{JfrRecording, JfrRecorderOptions}
import scala.concurrent.duration.DurationInt

val catsVersion            = "2.10.0"
val catsParseVersion       = "1.0.0"
val catsEffectVersion      = "3.5.4"
val circeVersion           = "0.14.6"
val disciplineMunitVersion = "2.0.0-M3"
val doobieVersion          = "1.0.0-RC5"
val flywayVersion          = "10.10.0"
val fs2Version             = "3.10.1"
val http4sVersion          = "0.23.26"
val jnrUnixsocketVersion   = "0.38.22"
val kindProjectorVersion   = "0.13.3"
val literallyVersion       = "1.1.0"
val logbackVersion         = "1.5.3"
val log4catsVersion        = "2.6.0"
val munitVersion           = "1.0.0-M11"
val munitCatsEffectVersion = "2.0.0-M4"
val skunkVersion           = "0.6.3"
val shapeless2Version      = "2.3.10"
val shapeless3Version      = "3.4.1"
val sourcePosVersion       = "1.1.0"
val typenameVersion        = "1.1.0"
val whaleTailVersion       = "0.0.10"

val Scala2 = "2.13.13"
val Scala3 = "3.3.3"
ThisBuild / scalaVersion        := Scala2
ThisBuild / crossScalaVersions  := Seq(Scala2, Scala3)
ThisBuild / tlJdkRelease        := Some(11)

ThisBuild / tlBaseVersion    := "0.19"
ThisBuild / startYear        := Some(2019)
ThisBuild / licenses         := Seq(License.Apache2)
ThisBuild / developers       := List(
  Developer("milessabin", "Miles Sabin", "miles@milessabin.com", url("http://milessabin.com/blog")),
  Developer("tpolecat",   "Rob Norris",  "rnorris@gemini.edu",   url("http://www.tpolecat.org")),
)

ThisBuild / tlCiScalafmtCheck       := false
ThisBuild / tlCiReleaseBranches     := Seq("main")
ThisBuild / tlSonatypeUseLegacyHost := false
ThisBuild / githubWorkflowBuild     ~= { steps =>
  WorkflowStep.Sbt(
    commands = List("headerCheckAll"),
    name = Some("Check Headers"),
  ) +: steps
}
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("11"))
ThisBuild / tlBspCrossProjectPlatforms := Set(JVMPlatform)

ThisBuild / githubWorkflowAddedJobs +=
  WorkflowJob(
    id = "coverage",
    name = s"Generate coverage report (2.13 JVM only)",
    scalas = Nil,
    sbtStepPreamble = Nil,
    steps = githubWorkflowJobSetup.value.toList ++
      List(
        WorkflowStep.Sbt(List("coverage", "rootJVM/test", "coverageReport")),
        WorkflowStep.Use(UseRef.Public("codecov", "codecov-action", "v3"))
      )
  )


ThisBuild / tlSitePublishBranch := Some("main")

lazy val commonSettings = Seq(
  //scalacOptions --= Seq("-Wunused:params", "-Wunused:imports", "-Wunused:patvars", "-Wdead-code", "-Wunused:locals", "-Wunused:privates", "-Wunused:implicits"),
  scalacOptions ++= Seq("-Xlint:-named-booleans", "-Xlint:-pattern-shadow").filterNot(_ => tlIsScala3.value),
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
    """|Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
       |Copyright (c) 2016-2023 Grackle Contributors
       |
       |Licensed under the Apache License, Version 2.0 (the "License");
       |you may not use this file except in compliance with the License.
       |You may obtain a copy of the License at
       |
       |  http://www.apache.org/licenses/LICENSE-2.0
       |
       |Unless required by applicable law or agreed to in writing, software
       |distributed under the License is distributed on an "AS IS" BASIS,
       |WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
       |See the License for the specific language governing permissions and
       |limitations under the License.
       |""".stripMargin
  ))
)

lazy val modules: List[CompositeProject] = List(
  core,
  circe,
  sql,
  doobie,
  skunk,
  generic,
  docs,
  unidocs,
  demo,
  benchmarks,
  profile
)

lazy val root = tlCrossRootProject
  .aggregate(modules:_*)
  .disablePlugins(RevolverPlugin)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/core"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .settings(commonSettings)
  .settings(
    name := "grackle-core",
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
    name := "grackle-circe",
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
    name := "grackle-sql",
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
    name := "grackle-doobie-pg",
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
    name := "grackle-skunk",
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
    name := "grackle-generic",
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
    name := "grackle-demo",
    coverageEnabled := false,
    libraryDependencies ++= Seq(
      "org.typelevel"     %% "log4cats-slf4j"             % log4catsVersion,
      "ch.qos.logback"    %  "logback-classic"            % logbackVersion,
      "org.tpolecat"      %% "doobie-core"                % doobieVersion,
      "org.tpolecat"      %% "doobie-postgres"            % doobieVersion,
      "org.tpolecat"      %% "doobie-hikari"              % doobieVersion,
      "org.http4s"        %% "http4s-ember-server"        % http4sVersion,
      "org.http4s"        %% "http4s-ember-client"        % http4sVersion,
      "org.http4s"        %% "http4s-circe"               % http4sVersion,
      "org.http4s"        %% "http4s-dsl"                 % http4sVersion,
      "org.flywaydb"      %  "flyway-database-postgresql" % flywayVersion,
      "io.chrisdavenport" %% "whale-tail-manager"         % whaleTailVersion,
      "com.github.jnr"    % "jnr-unixsocket"              % jnrUnixsocketVersion
    )
  )

lazy val benchmarks = project
  .in(file("benchmarks"))
  .dependsOn(core.jvm)
  .enablePlugins(NoPublishPlugin, AutomateHeaderPlugin, JmhPlugin)
  .settings(commonSettings)
  .settings(    
    coverageEnabled := false,
)

lazy val profile = project
  .in(file("profile"))
  .enablePlugins(NoPublishPlugin, AutomateHeaderPlugin)
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
    fork := true,
    coverageEnabled := false,
  )

lazy val docs = project
  .in(file("modules/docs"))
  .enablePlugins(TypelevelSitePlugin, AutomateHeaderPlugin)
  .settings(commonSettings)
  .settings(
    mdocVariables ++= Map("headerVariant" -> "tutorial"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % catsEffectVersion
    ),
    coverageEnabled := false,
  )

// Run repoDocs / mdoc manually to generated README.md from docs/index.md and header.md
lazy val repoDocs = project
  .in(file("repo-docs"))
  .dependsOn(core.jvm, docs)
  .enablePlugins(MdocPlugin, NoPublishPlugin)
  .settings(
    mdocVariables :=
      Map(
        "VERSION"       -> tlLatestVersion.value.getOrElse(version.value),
        "headerVariant" -> "repo"
        ),
    mdocIn  := file("docs/index.md"),
    mdocOut := file("README.md"),
    coverageEnabled := false,
  )

lazy val unidocs = project
  .in(file("unidocs"))
  .enablePlugins(TypelevelUnidocPlugin)
  .settings(
    name := "grackle-docs",
    coverageEnabled := false,
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(
      core.jvm,
      circe.jvm,
      sql.jvm,
      doobie,
      skunk.jvm,
      generic.jvm,
    )
  )
