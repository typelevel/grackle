# In-Memory Model

The GraphQL reference implementation defines an API over a simple data model representing characters and films from
the Star Wars series. Because of its appearance in the the reference implementation it is used as the basis for many
GraphQL tutorials, and many GraphQL server implementations provide it as an example. Grackle is no exception.

In this tutorial we are going to implement the Star Wars demo using Grackle backed by an in-memory model, ie. a
simple Scala data structure which captures the information required to service the GraphQL API.

## Running the demo

The demo is packaged as submodule `demo` in the Grackle project. It is an http4s-based application which can be run
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

This application hosts both the demo services and a web-based GraphQL client (GraphQL Playground) which can be used to
interact with them. You can run the client in your browser at
[http://localhost:8080/playground.html](http://localhost:8080/playground.html).

## Query examples

You can use the Playground to run queries against the model. Paste the following into the query field on left,

```json
query {
  hero(episode: EMPIRE) {
    name
    appearsIn
  }
}
```

Click the play button in the centre and you should see the following response on the right,

```json
{
  "data": {
    "hero": {
      "name": "Luke Skywalker",
      "appearsIn": [
        "NEWHOPE",
        "EMPIRE",
        "JEDI"
      ]
    }
  }
}
```

## The Schema

The Star Wars API is described by a GraphQL schema,

```json
type Query {
  hero(episode: Episode!): Character
  character(id: ID!): Character
  human(id: ID!): Human
  droid(id: ID!): Droid
}

enum Episode {
  NEWHOPE
  EMPIRE
  JEDI
}

interface Character {
  id: ID!
  name: String!
  friends: [Character]
  appearsIn: [Episode]!
}

type Droid implements Character {
  id: ID!
  name: String!
  friends: [Character]
  appearsIn: [Episode]!
  primaryFunction: String
}

type Human implements Character {
  id: ID!
  name: String!
  friends: [Character]
  appearsIn: [Episode]!
  homePlanet: String
}
```

Any one of the parametrized fields in the `Query` type may be used as the top level query, with nested queries over
fields of the result type. The structure of the query follows the schema, and the structure of the result follows the
structure of the query. For example,

```
query {
  character(id: 1002) {
    name
    friends {
      name
    }
  }
}
```
yields the result,

```json
{
  "data": {
    "character": {
      "name": "Han Solo",
      "friends": [
        {
          "name": "Luke Skywalker"
        },
        {
          "name": "Leia Organa"
        },
        {
          "name": "R2-D2"
        }
      ]
    }
  }
}
```

Grackle represents schemas as a Scala value of type `Schema`,

@@snip [StarWarsSchema.scala](/demo/src/main/scala/starwars/StarWarsSchema.scala)

## The Scala model

The API is backed by values of an ordinary Scala data types with no Grackle dependencies,

@@snip [StarWarsData.scala](/demo/src/main/scala/starwars/StarWarsData.scala) { #model_types }

The data structure is slightly complicated by the use of by-name arguments for the `friends` members: this is to
support cycles of friendship, eg.,

```json
query {
  character(id: 1000) {
    name
    friends {
      name
      friends {
        name
      }
    }
  }
}
```

yields,

```json
{
  "data": {
    "character": {
      "name": "Luke Skywalker",
      "friends": [
        {
          "name": "Han Solo",
          "friends": [
            {
              "name": "Luke Skywalker"
            },
            ...
        }
    }
  }
}
```

Here the root of the result is "Luke Skywalker" and we loop back to Luke through the mutual friendship with Han Solo.

The values of these data types are created in the usual way for cyclic data structures in Scala, using lazy vals,

@@snip [StarWarsData.scala](/demo/src/main/scala/starwars/StarWarsData.scala) { #model_values }

## The query compiler and elaborator

GraphQL queries are compiled into values of a Scala ADT which represents a query algebra. These query algebra terms
are then transformed in a variety of ways, resulting in a program which can be interpreted against the model to
produce the query result. The process of transforming these values is called _elaboration_, and each elaboration step
simplifies or expands the term to bring it into a form which can be executed directly by the query interpreter.

Grackle's query algebra consists of the following elements,

```scala
sealed trait Query {

object Query {
  case class Select(name: String, args: List[Binding], child: Query = Empty) extends Query
  case class Group(queries: List[Query]) extends Query
  case class Unique(pred: Predicate, child: Query) extends Query
  case class Filter(pred: Predicate, child: Query) extends Query
  case class Component(componentId: String, join: (Cursor, Query) => Result[Query], child: Query) extends Query
  case class Defer(join: (Cursor, Query) => Result[Query], child: Query) extends Query
  case class Wrap(name: String, child: Query) extends Query
  case object Empty extends Query
}
```

A simple query like this,

```json
query {
  character(id: 1000) {
    name
  }
}
```

is first translated into a term in the query algebra of the form,

```scala
Select("character", List(IntBinding("id", 1000)),
  Select("name", Nil)
)
```

This first step is performed without reference to a GraphQL schema, hence the `id` argument is initially inferred to
be of GraphQL type `Int`.

Following this initial translation the Star Wars example has a single elaboration step whose role is to translate the
selection into something executable. Elaboration uses the GraphQL schema and so is able to translate an input value
parsed as an `Int` into a GraphQL `ID`. The semantics associated with this (ie. what an `id` is and how it relates to
the model) is specific to this model, so we have to provide that semantic via some model-specific code,

@@snip [StarWarsData.scala](/demo/src/main/scala/starwars/StarWarsData.scala) { #elaborator }

Extracting out the case for the `character` selector,

```scala
case Select("character", List(IDBinding("id", id)), child) =>
  Select("character", Nil, Unique(FieldEquals("id", id), child)).rightIor
```

we can see that this transforms the previous term as follows,

```scala
Select("character", Nil,
  Unique(FieldEquals("id", 1000), Select("name", Nil)))
)
```

Here the argument to the `character` selector has been translated into a predicate which refines the root data of the
model to the single element which satisifies it via `Unique`. The remainder of the query (`Select("name", Nil)`) is
then within the scope of that constraint. We have eliminated something with model-specific semantics (`character(id:
1000)`) in favour of something universal which can be interpreted directly against the model.

## The query interpreter and cursor

The data required to construct the response to a query is determined by the structure of the query and gives rise to a
more or less arbitrary traversal of the model. To support this Grackle provides a functional `Cursor` abstraction
which points into the model and can nativigate through GraphQL fields, arrays and values as required by a given query.

For in-memory models this abstraction is implemented by `DataTypeCursor` which handles most of the navigation in a
generic way and is customized with a pair of `PartialFunctions`, one to set the starting point in the model for the
root of a given query, and the other to handle navigation through fields.

For the Star Wars model the root `PartialFunction` is of the following form,

@@snip [StarWarsData.scala](/demo/src/main/scala/starwars/StarWarsData.scala) { #root }

The left hand sides of the case expressions correspond to the top-level selection of the query (see the schema above)
and the right hand side yields a pair, consisting of the GraphQL type, and the Scala value which form the root for the
query. When the query is executed, navigation will start from that point and with that expected GraphQL type.

The navigation `PartialFunction` is of the following form,

@@snip [StarWarsData.scala](/demo/src/main/scala/starwars/StarWarsData.scala) { #cursor }

Here the left hand sides of the case expressions are a pair consisting of a Scala value representing the current focus
of the cursor in the model (these are Scala values) and the GraphQL field being navigated through. The right hand side
yields the new focus for the cursor following the navigation step.

## The service

What we've seen so far allows us to compile and execute GraphQL queries against our in-memory model. We now need to
expose that via HTTP. The following shows how we do that for http4s,

@@snip [StarWarsService.scala](/demo/src/main/scala/starwars/StarWarsService.scala) { #service }

The GraphQL specification supports queries both via HTTP GET and POST requests, so we provide routes for both methods.
For queries via GET the query is embedded in the URI query string in the form `... ?query=<URI encoded GraphQL
query>`. For queries via POST, the query is embedded in a JSON value of the form,

```json
{
  "operationName": "Query",
  "query": "character(id: 1000) ..."
}
```

In each of these cases we extract the operation name and query from the request and pass them to the service for
compilation and execution.

Many GraphQL client tools expect servers to be able to respond to a query named `IntrospectionQuery` returning a
representation of the GraphQL schema supported by the endpoint which they use to provide client-side highlighting,
validation, auto completion etc. The demo service provides this as well as normal query execution.

## Putting it all together

Finally we need to run all of this on top of http4s. Here we have a simple `IOApp` running a `BlazeServer` with the
`StarWarsService` defined above, and a `ResourceService` to serve the GraphQL Playground web client,

@@snip [main.scala](/demo/src/main/scala/demo/main.scala) { #server }
