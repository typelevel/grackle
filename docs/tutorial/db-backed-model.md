# DB Backed Model

In this section of the tutorial we are going to implement a GraphQL API for countries and cities of the world using
Grackle backed by a database, ie. provide a mapping for Grackle to read data from PostgreSQL and return it as result
of GraphQL queries.

## Running the demo

The demo is packaged as submodule `demo` in the Grackle project. It is a http4s-based application which can be run
from the SBT REPL using `sbt-revolver`,

```
sbt:root> demo/reStart
[info] Application demo not yet started
[info] Starting application demo in the background ...
demo Starting demo.Main.main()
demo INFO  - Ember-Server service bound to address: [::]:8080
```

This application hosts the demo services for in-memory and db-backend models, as well as a web-based GraphQL client
(GraphQL Playground) which can be used to interact with them. You can run the client for db-backend model in your
browser at [http://localhost:8080/playground.html?endpoint=world](http://localhost:8080/playground.html?endpoint=world).

## Query examples

You can use the Playground to run queries against the model. Paste the following into the query field on left,

```yaml
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

Grackle represents schemas as Scala values of type `Schema` which can be constructed given a schema text,

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/world/WorldMapping.scala", "#schema"))
```

The use of the `schema` string interpolator here causes the content of the string literal to be evaluated and checked
as a valid GraphQL schema at compile time.

## Database mapping

The API is backed by mapping to database tables. Grackle contains ready to use integration with
[doobie](https://tpolecat.github.io/doobie/) for accessing SQL database via JDBC and with
[Skunk](https://tpolecat.github.io/skunk/) for accessing PostgreSQL via its native API. In this example we will use
doobie.

Let's start with defining what tables and columns are available in the database model,

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/world/WorldMapping.scala", "#db_tables"))
```

For each column we need to provide its name and doobie codec of type `Meta`. We should also mark if the value is
nullable.

We define the top-level GraphQL fields as `SqlObject` mappings,

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/world/WorldMapping.scala", "#root"))
```

Now, we need to map each type from the GraphQL schema using columns from the database,

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/world/WorldMapping.scala", "#type_mappings"))
```

Each GraphQL type mapping must contain a key. It can contain fields from one table, but it can also contain nested
types which are translated to SQL joins using the provided conditions. `Join(country.code, city.countrycode)` means
joining country and city tables  where `code` in the country table is the same as `countrycode` in the city table.

## The query compiler and elaborator

Similarly to the [in-memory model](in-memory-model.md#the-query-compiler-and-elaborator), we need to define an
elaborator to transform query algebra terms into a form that can be then used to translate query algebra terms to SQL
queries,

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/world/WorldMapping.scala", "#elaborator"))
```

## Putting it all together

To expose the GraphQL API via http4s we will use the `GraphQLService` and `DemoServer` from the [in-memory
example](in-memory-model.md#the-service).

The `run` method starts the dockerized PostgreSQL database, creates the database schema, writes initial data and
exposes the GraphQL API for both the in-memory and the db-backend models,

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/Main.scala", "#main"))
```

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/DemoServer.scala", "#server"))
```
