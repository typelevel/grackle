# Compiler Phases

Grackle compiles GraphQL queries into terms of a query algebra before interpreting them against a mapping. The
compiler runs a sequence of _phases_, each of which transforms the query algebra term, bringing it step by step into
a form which can be executed directly by the query interpreter.

Several phases are built in and always run, among them the elaborators which substitute variables, apply `@skip` and
`@include` directives, inline fragments, and rewrite field selections according to the rules of a mapping's
`SelectElaborator`. In addition, mappings can install custom phases by overriding `compilerPhases`,

```scala
override def compilerPhases: List[QueryCompiler.Phase] =
  super.compilerPhases :+ new MyCustomPhase
```

A phase implements the `Phase` trait from `QueryCompiler`,

```scala
trait Phase {
  def transform(query: Query): Elab[Query]
}
```

`transform` receives the query algebra term produced by the preceding phase and yields a transformed term, or fails,
in the `Elab` monad. `Elab` gives phases access to the schema, the current context and the query's fragment
definitions, and allows compilation to be aborted with one or more GraphQL errors via `Elab.failure`. A phase which
fails prevents the query from executing at all — the client receives an error response with no data.

This makes phases a natural place to enforce global policies on incoming queries. Grackle provides one such policy
phase out of the box: `QuerySizeValidator`.

## Limiting query size with QuerySizeValidator

A GraphQL server which accepts arbitrary queries from untrusted clients needs protection against queries which are
too expensive to execute — deeply nested or extremely broad queries can otherwise consume unbounded resources. The
`QuerySizeValidator` phase rejects queries exceeding a configurable maximum depth or width before they reach the
interpreter,

```scala
override def compilerPhases: List[QueryCompiler.Phase] =
  super.compilerPhases :+ new QuerySizeValidator(maxDepth = 5, maxWidth = 5)
```

_Depth_ is the number of nested selection levels in the query, and _width_ is the total number of leaf fields
selected. Both are computed after fragment spreads have been resolved, so a query cannot evade the limits by
factoring its selections into fragments.

For example, with the Star Wars model from the previous chapter and the limits above, the query,

```yaml
query {
  character(id: "1000") {
    friends {
      friends {
        friends {
          friends {
            friends {
              friends {
                name
              }
            }
          }
        }
      }
    }
  }
}
```

is rejected with,

```json
{
  "errors" : [
    {
      "message" : "Query is too deep: depth is 8 levels, maximum is 5"
    }
  ]
}
```

and a query selecting too many leaf fields is similarly rejected with a `"Query is too wide"` error. A query
exceeding both limits at once is reported as `"Query is too complex"`.

## Limitations

Depth and width are syntactic measures: they are computed from the query text alone and know nothing about the size
of the underlying data. In particular, width does not account for list sizes — a field yielding a thousand elements
contributes to the width just once. `QuerySizeValidator` is therefore a coarse first line of defence rather than a
complete cost model. Guarding against expensive list expansions requires taking field cardinalities and arguments
into account, which can be implemented as a custom phase following the same pattern.
