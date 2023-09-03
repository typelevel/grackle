# DB Backed Model

In this tutorial we are going to implement GraphQL API of countries and cities of the world using Grackle backed by
a database model, i.e. provide mapping for Grackle to read data from PostgreSQL and return it as result of
GraphQL queries.

## Running the demo

The demo is packaged as submodule `demo` in the Grackle project. It is a http4s-based application which can be run
from the SBT REPL using `sbt-revolver`,

```
sbt:gsp-graphql> reStart
[info] Application demo not yet started
[info] Starting application demo in the background ...
demo Starting demo.Main.main()
demo[ERROR] Picked up JAVA_TOOL_OPTIONS:  -Xmx3489m
[success] Total time: 0 s, completed Sep 3, 2023, 5:01:11 AM
demo[ERROR] Sep 03, 2023 5:01:18 AM org.flywaydb.core.internal.license.VersionPrinter printVersionOnly
demo[ERROR] INFO: Flyway Community Edition 9.22.0 by Redgate
demo[ERROR] Sep 03, 2023 5:01:18 AM org.flywaydb.core.internal.license.VersionPrinter printVersion
demo[ERROR] INFO: See release notes here: https://rd.gt/416ObMi
demo[ERROR] Sep 03, 2023 5:01:18 AM org.flywaydb.core.internal.license.VersionPrinter printVersion
demo[ERROR] INFO: 
demo[ERROR] Sep 03, 2023 5:01:18 AM org.flywaydb.core.FlywayExecutor execute
demo[ERROR] INFO: Database: jdbc:postgresql://0.0.0.0:32771/test (PostgreSQL 11.8)
demo[ERROR] Sep 03, 2023 5:01:18 AM org.flywaydb.core.internal.schemahistory.JdbcTableSchemaHistory allAppliedMigrations
demo[ERROR] INFO: Schema history table "public"."flyway_schema_history" does not exist yet
demo[ERROR] Sep 03, 2023 5:01:18 AM org.flywaydb.core.internal.command.DbValidate validate
demo[ERROR] INFO: Successfully validated 1 migration (execution time 00:00.059s)
demo[ERROR] Sep 03, 2023 5:01:18 AM org.flywaydb.core.internal.schemahistory.JdbcTableSchemaHistory create
demo[ERROR] INFO: Creating Schema History table "public"."flyway_schema_history" ...
demo[ERROR] Sep 03, 2023 5:01:18 AM org.flywaydb.core.internal.command.DbMigrate migrateGroup
demo[ERROR] INFO: Current version of schema "public": << Empty Schema >>
demo[ERROR] Sep 03, 2023 5:01:18 AM org.flywaydb.core.internal.command.DbMigrate doMigrateGroup
demo[ERROR] INFO: Migrating schema "public" to version "1 - WorldSetup"
demo[ERROR] Sep 03, 2023 5:01:18 AM org.flywaydb.core.internal.sqlscript.DefaultSqlScriptExecutor printWarnings
demo[ERROR] WARNING: DB: there is already a transaction in progress (SQL State: 25001 - Error Code: 0)
demo[ERROR] Sep 03, 2023 5:01:18 AM org.flywaydb.core.internal.command.DbMigrate logSummary
demo[ERROR] INFO: Successfully applied 1 migration to schema "public", now at version v1 (execution time 00:00.110s)
demo [io-compute-2] INFO  o.h.e.s.EmberServerBuilderCompanionPlatform - Ember-Server service bound to address: [::]:8080 
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

The API is backed by mapping to database tables. Grackle contains ready to use integration
with [doobie](https://tpolecat.github.io/doobie/) for accessing SQL database via JDBC
and with [Skunk](https://tpolecat.github.io/skunk/) for accessing PostgreSQL via its native API. In this example
we will use doobie.

Let's start with defining what tables and columns are available in the database model,

@@snip [WorldMapping.scala](/demo/src/main/scala/demo/world/WorldMapping.scala) { #db_tables }

For each column we need to provide its name and doobie codec. We should also mark if value is nullable.

We define each query as SQL query, as below,

@@snip [WorldMapping.scala](/demo/src/main/scala/demo/world/WorldMapping.scala) { #root }

Now, we need to map each type from GraphQL schema using available columns from database,

@@snip [WorldMapping.scala](/demo/src/main/scala/demo/world/WorldMapping.scala) { #type_mappings }

Each GraphQL must contain key. It can contain fields from one table, but it can also contain nested types which
are translated to SQL joins using provided conditions. `Join(country.code, city.countrycode)` means joining country
and city tables  where `code` in the country table is the same as `countrycode` in the city table.

## The query compiler and elaborator

Similar as in [in-memory model]((in-memory-model.html#the-query-compiler-and-elaborator)), we need to define elaborator
to transform query algebra terms into the form that can be then used as source of SQL queries,

@@snip [WorldMapping.scala](/demo/src/main/scala/demo/world/WorldMapping.scala) { #elaborator }

## Putting it all together

To expose GraphQL API with http4s we will use `GraphQLService` and `DemoServer`
from [in-memory example](in-memory-model.html#the-service).

We will use [testcontainers](https://github.com/testcontainers/testcontainers-scala) to run PostgreSQL database
in the background. The final main method, which starts PostgreSQL database in docker container, creates database
schema, writes initial data and exposes GraphQL API for in-memory and db-backend models is below,

@@snip [main.scala](/demo/src/main/scala/demo/Main.scala) { #main }
