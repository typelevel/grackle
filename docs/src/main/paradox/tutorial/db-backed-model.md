# DB Backed Model

In this tutorial we are going to implement a GraphQL API for countries and cities of the world using Grackle backed by
a database, ie. provide a mapping for Grackle to read data from PostgreSQL and return it as result of GraphQL queries.

## Running the demo

The demo is packaged as submodule `demo` in the Grackle project. It is a http4s-based application which can be run
from the SBT REPL using `sbt-revolver`,

```
sbt:gsp-graphql> reStart
[info] Application demo not yet started
[info] Starting application demo in the background ...
demo Starting demo.Main.main()
[success] Total time: 0 s, completed 11-Dec-2019 16:55:35
sbt:gsp-graphql> demo [ioapp-compute-0] INFO  o.h.b.c.n.NIO1SocketServerGroup - [...]
demo [ioapp-compute-0] INFO  o.h.s.b.BlazeServerBuilder -
demo   _   _   _        _ _
demo  | |_| |_| |_ _ __| | | ___
demo  | ' \  _|  _| '_ \_  _(_-<
demo  |_||_\__|\__| .__/ |_|/__/
demo              |_|
demo [ioapp-compute-0] INFO  o.h.s.b.BlazeServerBuilder - [...]
```

This application hosts the demo services for in-memory and db-backend models, as well as a web-based GraphQL client
(GraphQL Playground) which can be used to interact with them. You can run the client for db-backend model in your
browser at [http://localhost:8080/playground.html?endpoint=world](http://localhost:8080/playground.html?endpoint=world).

## Query examples

You can use the Playground to run queries against the model. Paste the following into the query field on left,

```graphql
query {
  cities(namePattern: "London") {
    name
    country {
      name
    }
  }
}
```

Click the play button in the centre and you should see the following response on the right,

```json
{
  "data": {
    "cities": [
      {
        "name": "London",
        "country": {
          "name": "United Kingdom"
        }
      },
      {
        "name": "London",
        "country": {
          "name": "Canada"
        }
      }
    ]
  }
}
```

## The Schema

Grackle represents schemas as a Scala value of type `Schema` which can be constructed given a schema text,

@@snip [WorldSchema.scala](/demo/src/main/scala/demo/world/WorldMapping.scala) { #schema }

## Database mapping

The API is backed by mapping to database tables. Grackle contains ready to use integration with
[doobie](https://tpolecat.github.io/doobie/) for accessing SQL database via JDBC and with
[Skunk](https://tpolecat.github.io/skunk/) for accessing PostgreSQL via its native API. In this example we will use
doobie.

Let's start with defining what tables and columns are available in the database model,

@@snip [WorldMapping.scala](/demo/src/main/scala/demo/world/WorldMapping.scala) { #db_tables }

For each column we need to provide its name and doobie codec of type `Meta`. We should also mark if the value is
nullable.

We define the top-level GraphQL fields as `SqlObject` mappings,

@@snip [WorldMapping.scala](/demo/src/main/scala/demo/world/WorldMapping.scala) { #root }

Now, we need to map each type from the GraphQL schema using columns from the database,

@@snip [WorldMapping.scala](/demo/src/main/scala/demo/world/WorldMapping.scala) { #type_mappings }

Each GraphQL type mapping must contain a key. It can contain fields from one table, but it can also contain nested
types which are translated to SQL joins using the provided conditions. `Join(country.code, city.countrycode)` means
joining country and city tables  where `code` in the country table is the same as `countrycode` in the city table.

## The query compiler and elaborator

Similarly to the [in-memory model]((in-memory-model.html#the-query-compiler-and-elaborator)), we need to define an
elaborator to transform query algebra terms into a form that can be then used to translate query algebra terms to SQL
queries,

@@snip [WorldMapping.scala](/demo/src/main/scala/demo/world/WorldMapping.scala) { #elaborator }

## Putting it all together

To expose GraphQL API with http4s we will use the `GraphQLService` and `DemoServer`
from the [in-memory example](in-memory-model.html#the-service).

We use [testcontainers](https://github.com/testcontainers/testcontainers-scala) to run a PostgreSQL database
in the background. The final main method, which starts a dockerized PostgreSQL database, creates the database
schema, writes initial data and exposes GraphQL API for in-memory and db-backend models is below,

@@snip [main.scala](/demo/src/main/scala/demo/Main.scala) { #main }
