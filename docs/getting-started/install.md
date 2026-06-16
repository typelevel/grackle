# Installation

This page lists the Grackle modules you can depend on and the platforms each supports, so you can set up a `build.sbt` for your project. You need only `grackle-core`; every backend is an optional add-on. To run something end to end, follow the [quick start](quick-start.md) after wiring up the dependency.

## Core dependency

Grackle is published for **Scala 2.13 and 3.3+** under the `org.typelevel` organisation. The only required module is `grackle-core`, which gives you the schema model, the query compiler and interpreter, and the in-memory `ValueMapping` backend:

```scala
// Required: Scala 2.13/3.3+
libraryDependencies += "org.typelevel" %% "grackle-core" % "@VERSION@"
```

The `@VERSION@` placeholder resolves to the latest published version when the site is built; substitute the version you want from the [releases](https://github.com/typelevel/grackle/releases). `grackle-core` pulls in [cats](https://typelevel.org/cats), [cats-effect](https://typelevel.org/cats-effect/), [fs2](https://github.com/typelevel/fs2) and [circe](https://circe.github.io/circe/) transitively, so you do not add those yourself.

## Optional backends

Each backend ships as a separate module. Add only the ones whose data source you map. They all depend on `grackle-core`, so you do not need to list it twice.

```scala
// Optional: in-memory JSON backend backed by circe
libraryDependencies += "org.typelevel" %% "grackle-circe" % "@VERSION@"

// Optional: in-memory generic Scala backend (derives mappings from your ADTs)
libraryDependencies += "org.typelevel" %% "grackle-generic" % "@VERSION@"

// Optional: Postgres via Doobie (JVM only)
libraryDependencies += "org.typelevel" %% "grackle-doobie-pg" % "@VERSION@"

// Optional: Postgres via Skunk
libraryDependencies += "org.typelevel" %% "grackle-skunk" % "@VERSION@"

// Optional: Oracle via Doobie (JVM only)
libraryDependencies += "org.typelevel" %% "grackle-doobie-oracle" % "@VERSION@"

// Optional: SQL Server via Doobie (JVM only)
libraryDependencies += "org.typelevel" %% "grackle-doobie-mssql" % "@VERSION@"
```

| Module | Backend | Use it when |
| --- | --- | --- |
| `grackle-core` | In-memory `ValueMapping` | You map a schema onto plain Scala values you already hold in memory. |
| `grackle-circe` | In-memory `CirceMapping` | Your data is already circe `Json`. |
| `grackle-generic` | In-memory `GenericMapping` | You want a mapping derived from your case classes / sealed traits. |
| `grackle-doobie-pg` | Postgres via [Doobie](https://typelevel.org/doobie/) | You run SQL through Doobie against Postgres. |
| `grackle-doobie-oracle` | Oracle via Doobie | You run SQL through Doobie against Oracle. |
| `grackle-doobie-mssql` | SQL Server via Doobie | You run SQL through Doobie against SQL Server. |
| `grackle-skunk` | Postgres via [Skunk](https://typelevel.org/skunk/) | You prefer Skunk's pure-Scala Postgres protocol. |

For what each backend does and how to choose, see [What is Grackle?](overview.md); for wiring a SQL backend up, see [Use a SQL backend](../how-to/sql-backends.md).

## Platform and cross-build support

Grackle is cross-published for the JVM, [Scala.js](https://www.scala-js.org/) and [Scala Native](https://scala-native.org/). The split is along backend lines:

- **`grackle-core`, `grackle-circe`, `grackle-generic` and `grackle-skunk`** are available on **all three platforms** (JVM, JS, Native), for both Scala 2.13 and Scala 3.
- **The Doobie-based SQL backends — `grackle-doobie-pg`, `grackle-doobie-oracle`, `grackle-doobie-mssql` — are JVM only**, because Doobie's JDBC layer is JVM only. If you cross-build for JS or Native, scope these dependencies to the JVM target (for example with `.jvmSettings` on a cross project) rather than the shared settings.

For a cross-project build, that typically looks like adding `grackle-core` (and any cross-platform backend) to the shared `.settings(...)` and confining the Doobie modules to `.jvmSettings(...)`.

## Imports you'll usually need

Almost every Grackle program starts from two imports: the top-level package, and the `syntax` object that provides the compile-time `schema"""..."""` interpolator and the `.success` / `.failure` helpers on `Result`.

```scala
import grackle._
import grackle.syntax._
```

As you build a mapping you will commonly add the namespaced sub-packages for the query algebra, predicates and elaboration. The doc example mappings, for instance, open with:

```scala
import grackle._
import grackle.syntax._
import grackle.Query._          // Select, Filter, OrderBy, …
import grackle.Predicate._      // Eql, Contains, …
import grackle.Value._          // query-argument values
import grackle.QueryCompiler._  // SelectElaborator, elaboration phases
```

Effectful code additionally imports your effect type — `import cats.effect.IO` is the usual choice. Backends bring their own packages: `import grackle.circe._` for the circe backend, `import grackle.generic._` for generic derivation, and the relevant `grackle.doobie.*` / `grackle.skunk.*` packages for SQL.

## Next steps

- [Quick start: your first query](quick-start.md) — install done, now run a query against an in-memory mapping.
- [What is Grackle?](overview.md) — the compiler/interpreter model and how to pick a backend.
- [Introduction (tutorial)](../tutorial/intro.md) — build a working server step by step.
- [Serve a mapping over HTTP](../how-to/serve-over-http.md) — expose a mapping through http4s.
