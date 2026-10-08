# Parser Caching

`CachingQueryCompiler` caches the parse of a GraphQL query. It skips the parse on repeat requests where only the variables change. This is useful for long-running servers or applications.

## Quick start

Build the compiler _once_, at server startup. Reuse it for every request.

```scala
import cats.effect.{IO, IOApp}
import grackle.CachingQueryCompiler

object Server extends IOApp.Simple {

  def run: IO[Unit] =
    for {
      compiler <- CachingQueryCompiler[IO](myMapping.compiler) // built once
      _        <- serve(compiler)
    } yield ()
}
```

Pass the compiler into your handler:

```scala
def handle(compiler: CachingQueryCompiler[IO], document: String, variables: Json, requestEnv: Env): IO[Json] =
  for {
    op   <- compiler.compile(document, untypedVars = Some(variables), env = requestEnv)
    res  <- op.flatTraverse(o => myMapping.interpreter.run(o.query, o.rootTpe, requestEnv).compile.lastOrError)
    json <- myMapping.mkResponse(res)
  } yield json
```

`.compile.lastOrError` fits a one-shot query. For subscriptions, use the `Stream` that `Mapping.compileAndRun` returns instead.

By default, the cache holds 1024 documents in memory. When the cache is full, a new document replaces the oldest document.

## What gets cached

The cache key is the document text, matched exactly. Whitespace differences create separate entries.

The cache holds the result of the parse, as a `Result[ParsedDocument]`. `ParsedDocument` is a type alias for a pair of the operations and the fragments of the document.

Parse failures are also cached. A repeat of a malformed document costs one lookup, and the compiler does not parse it again.

The compiler does all other work on each request. This work includes validation, variable coercion, and elaboration.

## Change the size limit

```scala
import grackle.{CachingQueryCompiler, QueryCache}

for {
  cache <- QueryCache[IO](maxSize = 4096)
} yield CachingQueryCompiler[IO](myMapping.compiler, cache)
```

The size limit counts documents, not bytes.

## Use your own store

Implement `QueryCache` to use a different store, for example a store with an expiry time or a size limit in bytes.

```scala
import grackle.{CachingQueryCompiler, QueryCache, Result}
import grackle.QueryParser.ParsedDocument

val myCache: QueryCache[IO] =
  new QueryCache[IO] {
    def get(key: String): IO[Option[Result[ParsedDocument]]] = ???
    def put(key: String, value: Result[ParsedDocument]): IO[Unit] = ???
  }

val compiler = CachingQueryCompiler[IO](myMapping.compiler, myCache)
```

Rules for a custom store:

- The parse does not use the schema. Compilers with different schemas can share a store if they use the same parser configuration.
- `CachingQueryCompiler` does not store an internal error.

## Do not use a remote store

A remote store, for example Redis or Valkey, is possible. Grackle does not supply a serialization of `ParsedDocument`, so you must write one.

We recommend against a remote store. A remote cache hit almost always costs much more than a new parse, typically 2x to 70x as much:

- A network round trip to a cache server takes about 100 to 300 µs. A parse of a typical query takes 3 to 250 µs.
- On each hit, the client must also decode the entry. In our benchmarks, a JSON decode costs 35% to 60% the time of a parse.
- On each miss, the client must also encode the entry and do a second round trip.

If more than one server must share cached documents, give each server its own in-memory cache instead.
