```scala mdoc:passthrough
println(grackle.docs.Output.header("@headerVariant@"))
```

## Overview

Grackle is a [GraphQL](https://graphql.org) server written in functional [Scala](https://www.scala-lang.org), built on
the [Typelevel](https://typelevel.org) stack.

It's powered by [cats](https://typelevel.org/cats), [cats-effect](https://typelevel.org/cats-effect/),
[fs2](https://github.com/typelevel/fs2) and [http4s](https://http4s.org/), and supports GraphQL queries, mutations and
subscriptions.

It has an abstract model of data sources implemented in terms of declarative mappings between GraphQL schemas and
backing data, and cursors into that data. It supports in-memory, DB-backed, and effectful data sources.

Grackle is structured as a compiler/interpreter. Queries are type-checked against a GraphQL schema and compiled into
an internal query algebra. The query algebra may be further compiled in a backend-specific way to materialize data. In
particular it can be compiled to efficient SQL and in that regard currently supports Postgres via
[Doobie](https://tpolecat.github.io/doobie/) or [Skunk](https://typelevel.org/skunk/).

Grackle is an [Apache 2.0 licensed](https://www.apache.org/licenses/LICENSE-2.0) Typelevel project and is available
for Scala 2/3 and for [Scala.js](https://www.scala-js.org/) and [Scala Native](https://scala-native.org/en/stable/).

Work has been generously sponsored by
[Aura/Gemini](https://www.aura-astronomy.org/centers/nsfs-oir-lab/gemini-observatory/) and [ITV](https://www.itv.com)
over the last four years.

## Getting Started

- See the [tutorial](https://typelevel.org/grackle) and accompanying [demo](https://github.com/typelevel/grackle/tree/main/demo/src/main).
- Online Scaladoc is available [here](https://javadoc.io/doc/org.typelevel/grackle-core_2.13).
- Ask us anything the in **#grackle** channel on the Typelevel [discord server][grackle-dev].

To add Grackle to your project you should add the following to your `build.sbt`,

```scala
// Required: Scala 2.13/3.3+
libraryDependencies += "org.typelevel" %% "grackle-core" % "@VERSION@"

// Optional: support for in-memory Json backend using circe
libraryDependencies += "org.typelevel" %% "grackle-circe" % "@VERSION@"

// Optional: support for in-memory generic Scala backend using shapeless
libraryDependencies += "org.typelevel" %% "grackle-generic" % "@VERSION@"

// Optional: support for Postgres backend via Doobie (JVM only)
libraryDependencies += "org.typelevel" %% "grackle-doobie-pg" % "@VERSION@"

// Optional: support for Postgres backend via Skunk
libraryDependencies += "org.typelevel" %% "grackle-skunk" % "@VERSION@"
```

## Community

Grackle is proud to be a [Typelevel](https://typelevel.org/) project. We are committed to providing a friendly, safe
and welcoming environment for all, and ask that the community adhere to the [Scala Code of
Conduct](https://www.scala-lang.org/conduct/) in all venues.

Conversations around Grackle are currently happening on [GitHub issues][grackle-issues], [PR
discussions][grackle-pulls], and [discord][grackle-dev].

The Typelevel [discord][grackle-dev] has a **#grackle** channel, as well as channels for related
projects such as **#cats**, **#cats-effect**, **#fs2**, **#doobie** and **#skunk**. If you're new to the Typelevel
ecosystem the **#beginners** channel might also be useful. Please join us!

## Contributing

This project exists thanks to [all the people who
contribute](https://github.com/typelevel/grackle/graphs/contributors).

We welcome all kinds of contribution, including but not limited to,

- documentation improvements, explanatory images/diagrams, fixes in typos, useful links
- refactorings of messy code, build structure, increasing test coverage or quality
- new features and bugfixes (including [bug reports and feature requests][grackle-issues]).

Writing documentation is valuable for learning, so if you find some explanation insufficient, overly complicated or
incorrect, it's a perfect opportunity to make a change to it!

If at any point you run into problems, you can always ask a question on the **#grackle** channel on the Typelevel
[discord server][grackle-dev].

More information, including on how to build locally and submit pull requests, can be found [here](CONTRIBUTING.md).

[grackle-issues]: https://github.com/typelevel/grackle/issues
[grackle-pulls]: https://github.com/typelevel/grackle/pulls
[grackle-dev]: https://discord.gg/GYD4J9w8EK
