import nl.zolotko.sbt.jfr.{JfrRecording, JfrRecorderOptions}
import scala.concurrent.duration.DurationInt
import scala.sys.process._

val catsVersion            = "2.11.0"
val catsParseVersion       = "1.0.0"
val catsEffectVersion      = "3.6.3"
val circeVersion           = "0.14.8"
val disciplineMunitVersion = "2.0.0-M3"
val doobieVersion          = "1.0.0-RC12"
val fs2Version             = "3.12.2"
val http4sVersion          = "0.23.33"
val kindProjectorVersion   = "0.13.4"
val literallyVersion       = "1.1.0"
val logbackVersion         = "1.5.32"
val log4catsVersion        = "2.7.1"
val mssqlDriverVersion     = "13.2.1.jre11"
val munitVersion           = "1.0.0-M11"
val munitCatsEffectVersion = "2.1.0"
val munitScalaCheckVersion = "1.0.0-M11"
val oracleDriverVersion    = "23.26.1.0.0"
val skunkVersion           = "0.6.5"
val shapeless2Version      = "2.3.11"
val shapeless3Version      = "3.4.1"
val sourcePosVersion       = "1.1.0"
val typenameVersion        = "1.1.0"

val Scala2 = "2.13.18"
val Scala3 = "3.3.7"

ThisBuild / scalaVersion        := Scala2
ThisBuild / crossScalaVersions  := Seq(Scala2, Scala3)
ThisBuild / tlJdkRelease        := Some(11)

ThisBuild / tlBaseVersion    := "0.26"
ThisBuild / startYear        := Some(2019)
ThisBuild / licenses         := Seq(License.Apache2)
ThisBuild / developers       := List(
  Developer("milessabin", "Miles Sabin", "miles@milessabin.com", url("http://milessabin.com/blog")),
  Developer("tpolecat",   "Rob Norris",  "rnorris@gemini.edu",   url("http://www.tpolecat.org")),
)

ThisBuild / tlFatalWarnings         := true
ThisBuild / tlCiScalafmtCheck       := false
ThisBuild / tlCiReleaseBranches     := Seq("main")
ThisBuild / githubWorkflowBuild     ~= { steps =>
  Seq(
    WorkflowStep.Sbt(
      commands = List("headerCheckAll"),
      name = Some("Check Headers")
    ),
    WorkflowStep.Run(
      commands = List("docker compose up --force-recreate -d --wait --quiet-pull"),
      name = Some("Start up test databases")
    )
  ) ++ steps
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
        WorkflowStep.Run(
          commands = List("docker compose up --force-recreate -d --wait --quiet-pull"),
          name = Some("Start up test databases")
        ),
        WorkflowStep.Sbt(List("coverage", "rootJVM/test", "coverageReport")),
        WorkflowStep.Use(UseRef.Public("codecov", "codecov-action", "v3"))
      )
  )

ThisBuild / tlSitePublishBranch := Some("main")

lazy val allUp = taskKey[Unit]("Start all docker compose services")
lazy val allStop = taskKey[Unit]("Stop all docker compose services")
lazy val pgUp = taskKey[Unit]("Start Postgres")
lazy val pgStop = taskKey[Unit]("Stop Postgres")
lazy val oracleUp = taskKey[Unit]("Start Oracle")
lazy val oracleStop = taskKey[Unit]("Stop Oracle")
lazy val mssqlUp = taskKey[Unit]("Start SQL Server")
lazy val mssqlStop = taskKey[Unit]("Stop SQL Server")

ThisBuild / allUp := runDocker("docker compose up -d --wait --quiet-pull")
ThisBuild / allStop := runDocker("docker compose stop")
ThisBuild / pgUp := runDocker("docker compose up -d --wait --quiet-pull postgres")
ThisBuild / pgStop := runDocker("docker compose stop postgres")
ThisBuild / oracleUp := runDocker("docker compose up -d --wait --quiet-pull oracle")
ThisBuild / oracleStop := runDocker("docker compose stop oracle")
ThisBuild / mssqlUp := runDocker("docker compose up -d --wait --quiet-pull mssql")
ThisBuild / mssqlStop := runDocker("docker compose stop mssql")

def runDocker(cmd: String): Unit = {
  require(cmd.! == 0, s"docker indicated an error")
}

lazy val commonSettings = Seq(
  //scalacOptions --= Seq("-Wunused:params", "-Wunused:imports", "-Wunused:patvars", "-Wdead-code", "-Wunused:locals", "-Wunused:privates", "-Wunused:implicits"),
  scalacOptions -= "-Wunused:privates", // Temporarily disable unused privates warning due to spurious warnings in Scala 3.3.7
  scalacOptions ++= Seq("-Xlint:-pattern-shadow").filterNot(_ => tlIsScala3.value),
  resolvers += Resolver.sonatypeCentralSnapshots,
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit"             % munitVersion % "test",
    "org.scalameta" %%% "munit-scalacheck"  % munitScalaCheckVersion % "test",
    "org.typelevel" %%% "cats-laws"         % catsVersion % "test",
    "org.typelevel" %%% "discipline-munit"  % disciplineMunitVersion % "test",
    "org.typelevel" %%% "munit-cats-effect" % munitCatsEffectVersion % "test",
    "io.circe"      %%% "circe-literal"     % circeVersion % "test",
    "io.circe"      %%% "circe-jawn"        % circeVersion % "test",
    "io.circe"      %%% "circe-parser"      % circeVersion % "test",
  ) ++ Seq(
    compilerPlugin("org.typelevel" %% "kind-projector" % kindProjectorVersion cross CrossVersion.full),
  ).filterNot(_ => tlIsScala3.value),
  headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
  headerLicense  := Some(HeaderLicense.Custom(
    """|Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
       |Copyright (c) 2016-2025 Grackle Contributors
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
  sqlcore,
  sqlpg,
  doobiecore,
  doobiepg,
  doobieoracle,
  doobiemssql,
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
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
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

lazy val sqlcore = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/sql-core"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(core % "test->test;compile->compile", circe, buildInfo % Test)
  .settings(commonSettings)
  .settings(
    name := "grackle-sql-core",
    libraryDependencies ++= Seq(
      "io.circe"          %%% "circe-generic"      % circeVersion % "test",
      "co.fs2"            %%% "fs2-io"             % fs2Version % "test",
    )
  )

lazy val sqlpg = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("modules/sql-pg"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(sqlcore % "test->test;compile->compile", circe)
  .settings(commonSettings)
  .settings(
    name := "grackle-sql-pg",
  )

lazy val doobiecore = project
  .in(file("modules/doobie-core"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(sqlcore.jvm % "test->test;compile->compile", circe.jvm)
  .settings(commonSettings)
  .settings(
    name := "grackle-doobie-core",
    Test / fork := true,
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(
      "org.tpolecat"   %% "doobie-core"     % doobieVersion,
      "org.typelevel"  %% "log4cats-core"   % log4catsVersion,
      "ch.qos.logback" %  "logback-classic" % logbackVersion % "test"
    )
  )

lazy val doobiepg = project
  .in(file("modules/doobie-pg"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(doobiecore % "test->test;compile->compile", sqlpg.jvm % "test->test;compile->compile")
  .settings(commonSettings)
  .settings(
    name := "grackle-doobie-pg",
    Test / fork := true,
    Test / parallelExecution := false,
    Test / testOptions += Tests.Setup(_ => runDocker("docker compose up -d --wait --quiet-pull postgres")),
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "doobie-postgres-circe" % doobieVersion
    )
  )

lazy val doobieoracle = project
  .in(file("modules/doobie-oracle"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(doobiecore % "test->test;compile->compile")
  .settings(commonSettings)
  .settings(
    name := "grackle-doobie-oracle",
    Test / fork := true,
    Test / parallelExecution := false,
    Test / testOptions += Tests.Setup(_ => runDocker("docker compose up -d --wait --quiet-pull oracle")),
    libraryDependencies ++= Seq(
      "com.oracle.database.jdbc" % "ojdbc8" % oracleDriverVersion
    )
  )

lazy val doobiemssql = project
  .in(file("modules/doobie-mssql"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(doobiecore % "test->test;compile->compile")
  .settings(commonSettings)
  .settings(
    name := "grackle-doobie-mssql",
    Test / fork := true,
    Test / parallelExecution := false,
    Test / testOptions += Tests.Setup(_ => runDocker("docker compose up -d --wait --quiet-pull mssql")),
    libraryDependencies ++= Seq(
      "com.microsoft.sqlserver" % "mssql-jdbc" % mssqlDriverVersion
    )
  )

lazy val skunk = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("modules/skunk"))
  .enablePlugins(AutomateHeaderPlugin)
  .disablePlugins(RevolverPlugin)
  .dependsOn(sqlpg % "test->test;compile->compile", circe)
  .settings(commonSettings)
  .settings(
    name := "grackle-skunk",
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(
      "org.tpolecat"  %%% "skunk-core"    % skunkVersion,
      "org.tpolecat"  %%% "skunk-circe"   % skunkVersion,
      "org.typelevel" %%  "log4cats-core" % log4catsVersion
    )
  )
  .jvmSettings(
    Test / fork := true,
    Test / testOptions += Tests.Setup(_ => runDocker("docker compose up -d --wait --quiet-pull postgres")),
    libraryDependencies ++= Seq(
      "ch.qos.logback" % "logback-classic" % logbackVersion % "test"
    )
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

import spray.revolver.Actions._

lazy val demo = project
  .in(file("demo"))
  .enablePlugins(NoPublishPlugin, AutomateHeaderPlugin)
  .dependsOn(buildInfo.jvm, core.jvm, generic.jvm, doobiepg)
  .settings(commonSettings)
  .settings(
    name := "grackle-demo",
    coverageEnabled := false,
    libraryDependencies ++= Seq(
      "org.typelevel"     %% "log4cats-slf4j"      % log4catsVersion,
      "ch.qos.logback"    %  "logback-classic"     % logbackVersion,
      "org.tpolecat"      %% "doobie-core"         % doobieVersion,
      "org.tpolecat"      %% "doobie-postgres"     % doobieVersion,
      "org.tpolecat"      %% "doobie-hikari"       % doobieVersion,
      "org.http4s"        %% "http4s-ember-server" % http4sVersion,
      "org.http4s"        %% "http4s-ember-client" % http4sVersion,
      "org.http4s"        %% "http4s-circe"        % http4sVersion,
      "org.http4s"        %% "http4s-dsl"          % http4sVersion
    ),
    reStart := // Redefine reStart to depend on pgUp
      Def.inputTask(reStart.evaluated)
        .dependsOn(Compile / products)
        .dependsOn(ThisBuild / pgUp)
        .evaluated
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
  .dependsOn(doobiepg)
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
      sqlcore.jvm,
      sqlpg.jvm,
      doobiecore,
      doobiepg,
      doobieoracle,
      doobiemssql,
      skunk.jvm,
      generic.jvm,
    )
  )
