# Introduction

This section contains worked, end-to-end examples that you can read start to finish. Each tutorial builds a
complete, runnable GraphQL service, introducing Grackle's ideas in the order you meet them when building something
real. If you just want to get a query running as fast as possible, start with the
[quick start](../getting-started/quick-start.md) instead.

The tutorials build on each other, so we recommend reading them in order:

* **[In-memory model](in-memory-model.md)** — serve a GraphQL API from a plain Scala data structure (the Star Wars
  example). Introduces schemas, mappings, cursors, the query algebra and elaboration.
* **[DB-backed model](db-backed-model.md)** — serve the same style of API from PostgreSQL, letting Grackle compile
  queries directly to SQL (the World example).
* **[Mutations & subscriptions](mutations-subscriptions.md)** — go beyond queries: run effectful writes with
  mutations and stream live updates with subscriptions.

Once you have worked through these, the [how-to guides](../how-to/filtering-ordering-paging.md) cover specific tasks,
the [concepts](../concepts/architecture.md) section explains how Grackle works under the hood, and the
[reference](../reference/schema-sdl.md) section documents every type and signature.
