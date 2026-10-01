# Parser/Validation Caching

`CachingQueryCompiler` caches the parse and the document-level validation of a GraphQL query. It skips this work on repeat requests where only the variables change. This is useful for long-running servers or applications.

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

By default, the cache holds 1024 documents. It drops a document one hour after its last use, and it drops the least recently used document when full.

## What gets cached

The cache key is the document text, matched exactly. Whitespace differences create separate entries.

Cached: parsing, fragment and variable validation, field mergeability, compiled variable definitions, and root type checks.

Not cached: variable coercion, directive validation, and elaboration. These depend on the per-request `Env` and variable values, so caching them would leak state between requests.

Parse failures are also cached, so repeat malformed documents cost one lookup and are not re-parsed.

## Change the size limit or TTL

```scala
import scala.concurrent.duration._
import grackle.{CachingQueryCompiler, QueryCache}

for {
  cache <- QueryCache[IO](maxSize = 4096, ttl = 15.minutes)
} yield CachingQueryCompiler[IO](myMapping.compiler, cache)
```

The size limit counts documents, not bytes.

## Use your own store

```scala
import grackle.{CachingQueryCompiler, PreparedDocument, QueryCache, Result}

val myCache: QueryCache[IO] =
  new QueryCache[IO] {
    def get(key: String): IO[Option[Result[PreparedDocument]]] = ???
    def put(key: String, value: Result[PreparedDocument]): IO[Unit] = ???
  }

val compiler = CachingQueryCompiler[IO](myMapping.compiler, myCache)
```

Rules for a custom store:

- Use one store per compiler. Compilers with different schemas must not share a store.
- `PreparedDocument` holds references to the compiler and thus cannot be serialized.
