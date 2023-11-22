# Contributing

Thanks for taking interest in helping us develop Grackle! If you want to contribute to the project, you can find some
helpful information here.

We welcome all kinds of contribution, including but not limited to,

- documentation improvements, explanatory images/diagrams, fixes in typos, useful links
- refactorings of messy code, build structure, increasing test coverage or quality
- new features and bugfixes (including [bug reports and feature requests][grackle-issues]).

Writing documentation is valuable for learning, so if you find some explanation insufficient, overly complicated or
incorrect, it's a perfect opportunity to make a change to it!

If at any point you run into problems, you can always ask a question on the **#grackle** channel on the Typelevel
[discord server][grackle-dev].

## How to submit a change

If you see something worth adding, make the relevant changes in a fork of the source code and submit a [pull
request][grackle-pulls] to the project. If you don't know what you could help with, take a look at [the issues marked
as "good first issue"][first-issues] or ask on [discord][grackle-dev].

Any contributions are expected to be made in the form of pull requests to the Grackle [repository][grackle-pulls].
Usually it takes one approval for a change to be merged. If it takes too long to get it approved, feel free to ask on
[Discord][grackle-dev].

Grackle is licensed under the Apache Software License 2.0. Opening a pull request is to be considered affirmative
consent to incorporate your changes into the project, granting an unrestricted license to Typelevel to distribute and
derive new work from your changes, as per the contribution terms of ASL 2.0. You also affirm that you own the rights
to the code you are contributing. All contributors retain the copyright to their own work.

Remember to follow the [code of conduct][coc] in online and offline discourse.

## Building the project locally

### Prerequisites

You'll need JDK 11+, [sbt][sbt], [Docker][docker] (for running DB tests) and [Node.js][node] (for running Scala.js
tests).

We use several sbt plugins to build and check the project, including [MiMa (Migration Manager)][mima] and. The
[sbt-typelevel](https://typelevel.org/sbt-typelevel/) plugin does the bulk of the SBT configuration.

### Build process

To compile the code for the whole repository, you can start an interactive sbt shell:

```bash
% sbt                                                                                                        
[info] welcome to sbt 1.9.7 (GraalVM Community Java 17.0.8)
[info] loading settings for project global-plugins from sbt-updates.sbt ...
[info] loading global plugins from /home/miles/.sbt/1.0/plugins
[info] loading settings for project grackle-build-build-build-build from metals.sbt ...
[info] loading project definition from /home/miles/projects/grackle/project/project/project/project
[info] loading settings for project grackle-build-build-build from metals.sbt ...
[info] loading project definition from /home/miles/projects/grackle/project/project/project
[success] Generated .bloop/grackle-build-build-build.json
[success] Total time: 2 s, completed 25 Oct 2023, 12:55:45
[info] loading settings for project grackle-build-build from metals.sbt,plugins.sbt ...
[info] loading project definition from /home/miles/projects/grackle/project/project
[success] Generated .bloop/grackle-build-build.json
[success] Total time: 1 s, completed 25 Oct 2023, 12:55:46
[info] loading settings for project grackle-build from metals.sbt,plugins.sbt ...
[info] loading project definition from /home/miles/projects/grackle/project
[info] loading settings for project root from build.sbt ...
[info] resolving key references (29638 settings) ...
[info] set scmInfo to https://github.com/typelevel/grackle
[info] set current project to root (in build file:/home/miles/projects/grackle/)
[info] sbt server started at local:///home/miles/.sbt/1.0/server/9b2532ef9aeebb6c666d/sock
[info] started sbt server
sbt:root> 
```

Inside the shell, you can compile the sources for the currently selected Scala version using the `compile` command. To
compile the code for all Scala versions enabled in the build, use `+compile`. To include tests, `Test/compile` or
`+Test/compile`, accordingly.


### Testing

To test the code, you can run the `test` command in sbt. If you want the tests on a single platform, you can use
`rootJVM/test`, `rootJS/test`, or `rootNative/test` instead.

It is possible to run a single test suite from a project on a single platform by [executing a more specific
task](https://www.scala-sbt.org/1.x/docs/Testing.html#testOnly), like `coreJVM/testOnly compiler.ScalarsSuite`.

You can list all available projects by executing the `projects` task:

```sbt
sbt:root> projects
[info] In file:/home/miles/projects/grackle/
[info]     benchmarks
[info]     buildInfoJS
[info]     buildInfoJVM
[info]     buildInfoNative
[info]     circeJS
[info]     circeJVM
[info]     circeNative
[info]     coreJS
[info]     coreJVM
[info]     coreNative
[info]     demo
[info]     docs
[info]     doobie
[info]     genericJS
[info]     genericJVM
[info]     genericNative
[info]     profile
[info]   * root
[info]     rootJS
[info]     rootJVM
[info]     rootNative
[info]     skunkJS
[info]     skunkJVM
[info]     skunkNative
[info]     sqlJS
[info]     sqlJVM
[info]     sqlNative
[info]     unidocs
```

Before submitting a change for review, it's worth running some extra checks that will be triggered in Continuous
Integration:

```sbt
sbt:root> prePR
```

If you run into any problems with tests, binary compatibility or other issues, feel free to ask questions on
[discord][grackle-dev].

[grackle-issues]: https://github.com/typelevel/grackle/issues
[grackle-pulls]: https://github.com/typelevel/grackle/pulls
[grackle-dev]: https://discord.gg/GYD4J9w8EK
[coc]: https://github.com/typelevel/grackle/blob/main/CODE_OF_CONDUCT.md
[sbt]: https://www.scala-sbt.org
[mima]: https://github.com/lightbend/mima
[first-issues]: https://github.com/typelevel/grackle/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22
[docker]: https://www.docker.com/
[node]: https://nodejs.org/en/
