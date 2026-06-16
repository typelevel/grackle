# Filter, sort and page a field

This how-to wires GraphQL arguments on a list field to Grackle's filtering, ordering and paging machinery: the `Filter`, `OrderBy`, `Offset`, `Limit` and `Count` query nodes, the predicate ADT (`Eql`, `Contains`, …), and the elaboration hooks that install them. It is for developers exposing list endpoints who want server-side filtering, sorting and paging. Everything here is assembled by hand from small `Query` nodes — Grackle has no built-in Relay `Connection` type, so the paging section shows how to build counted and "has-more" paging yourself. For the "why" behind these nodes, see the [predicates reference](../reference/predicates.md) and [filtering & paging nodes reference](../reference/filtering-paging-nodes.md).

## How filtering works

Filtering, ordering and paging are not part of your schema's runtime data — they are query rewrites installed during compilation. You match a field and its argument `Binding`s in a `SelectElaborator` and use `Elab.transformChild` to wrap the field's child query with the nodes you want. The predicate inside a `Filter` is a `Term[Boolean]`: a reified, introspectable function `Cursor => Result[Boolean]`. Because it is reified rather than an opaque Scala function, an in-memory mapping can evaluate it against the `Cursor` while a SQL backend can compile the same predicate into a `WHERE` clause. You build the operands from a schema `Type` with the `Type / "field"` path syntax and compare them against literals with `Const`.

## Filter a list field from a GraphQL argument

The following mapping is a plain in-memory `ValueMapping[IO]` over a `List[Item]` — no database, no SQL. It exposes two filtered list fields and a computed `tagCount` field, and wires both filters in its `selectElaborator`.

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/docs/src/main/scala/grackle/FilterMapping.scala", "#filter"))
```

The schema declares `itemsByTag(tag: ID!): [Item!]!` and `itemsByTagCount(count: Int!): [Item!]!`. Both `ValueField` mappings ignore their argument and return the full `items` list; the filtering happens entirely in the elaborator:

- The `itemsByTag` case matches a `Binding("tag", IDValue(tag))` and rewrites the child to `Filter(Contains(ItemType / "tags", Const(tag)), child)`. `Contains` tests membership of a **list-valued** field, so `ItemType / "tags"` resolves to a list term and `Const(tag)` is the element to look for.
- The `itemsByTagCount` case filters with `Eql(ItemType / "tagCount", Const(count))`. `tagCount` has no backing data — it is a `CursorField` computed from `tags.size` — yet the filter still works, because predicates run against the `Cursor`, not the raw row.

`Eql`/`Contains` require an implicit `Eq[T]` for the field's element type (both are available from cats here); the ordering predicates (`Lt`, `GtEql`, …) and `OrderSelection` require `Order[T]` instead. The element type must match the mapped field type, **including** `Option` — use `IsNull[Int]` over an `Option[Int]` field, `OrderSelection[Option[String]]` over a nullable string, and so on.

Because `FilterMapping` needs no database, you can run a query against it directly:

```scala mdoc:silent
import cats.effect.unsafe.implicits.global
import grackle.docs.FilterMapping

val filtered =
  FilterMapping.compileAndRun("""
    query {
      itemsByTag(tag: "B") { label }
    }
  """).unsafeRunSync()
```

```scala mdoc:passthrough
println("```json")
println(filtered.spaces2)
println("```")
```

Only items whose `tags` list contains `"B"` (`AB` and `BC`) come back — the `Contains` predicate ran server-side, filtering the in-memory list before the response was built.

### The predicate vocabulary

`Filter` holds any `Predicate` (a `Term[Boolean]`), and the predicate ADT covers the usual leaves and combinators. The common ones, with the typeclass each needs:

| Predicate | Use | Requires |
| --- | --- | --- |
| `Eql` / `NEql` | equality / inequality | `Eq[T]` |
| `Lt` / `LtEql` / `Gt` / `GtEql` | ordering comparisons | `Order[T]` |
| `In(term, list)` | membership in a static `List[T]` | `Eq[T]` |
| `Contains(listTerm, elem)` | list field contains an element | `Eq[T]` |
| `IsNull(optTerm, isNull)` | null / not-null test on an `Option` field | — |
| `Matches` / `StartsWith` | regex / prefix on a `String` field | — |
| `And` / `Or` / `Not`, `True` / `False` | boolean combinators | — |

For SQL-style wildcard matching there is also `Like(term, pattern, caseInsensitive)`, but note it lives in the `grackle.sql` module, **not** in core, and is only meaningful against a SQL backend. Its first parameter is a union type `Term[String] | Term[Option[String]]`, so it accepts both nullable and non-nullable string terms. To combine many predicates, fold them with the `Predicate.and` / `Predicate.or` smart constructors (or `And.combineAll`), which short-circuit on `True`/`False` — but guard against an empty list yourself, since an empty `and` collapses to `True`.

## Single-result lookup

To return one element rather than a list, filter to the matching row and wrap the whole thing in `Unique`:

```scala mdoc:compile-only
import grackle.Predicate.{Const, Eql}
import grackle.Query.{Binding, Filter, Unique}
import grackle.QueryCompiler.{Elab, SelectElaborator}
import grackle.Value.IntValue
import grackle.docs.QuickStartMapping.{BookType, QueryType}

val example =
  SelectElaborator {
    case (QueryType, "book", List(Binding("id", IntValue(id)))) =>
      Elab.transformChild(child =>
        Unique(Filter(Eql(BookType / "id", Const(id)), child)))
  }
```

`Unique` turns a list-producing child into a single (optionally absent) result, failing if more than one row matches. `Eql(BookType / "id", Const(id))` is the same predicate pattern as before; only the surrounding `Unique` distinguishes a lookup from a filtered list.

## Compose optional filter, order, offset and limit

When a field takes several **optional** arguments, the clean pattern is to model each argument as a `Query => Result[Query]` transformer and thread them inner-to-outer inside `Elab.transformChild`. Each transformer leaves the query untouched when its argument is absent, applies the corresponding node when present, and returns a `Result.failure` when the argument is invalid — argument validation belongs here in the elaborator, not in the `Query` ADT, which will happily build an `Offset(-1, …)` or `Limit(0, …)`.

The next two snips are from a SQL test mapping (`SqlFilterOrderOffsetLimitMapping`), but the structure is backend-independent. First, the per-argument transformers:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlFilterOrderOffsetLimitMapping.scala", "#fool_mk"))
```

Each `mk*` function pattern-matches the bound `Value`:

- `AbsentValue | NullValue` means the argument was omitted — return the query unchanged with `.success`.
- `mkOffset` accepts `0` (no-op) and any positive `Int` (`Offset(num, query)`), and **rejects** a negative offset with `Result.failure`. `mkLimit` similarly requires a strictly positive limit, rejecting `0`.
- `mkFilter` wraps the child in `Filter(Eql(tpe / "id", Const(id)), query)` when a structured `FilterValue` is supplied.
- `mkOrderBy` takes a callback producing `List[OrderSelection[_]]`; an empty list means "no ordering", otherwise it wraps the child in `OrderBy(OrderSelections(oss), query)`.

Now the elaborator composes them with a `for`-comprehension over `Result`, which short-circuits on the first validation failure:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlFilterOrderOffsetLimitMapping.scala", "#fool_elab"))
```

The order of the binds matters: `mkFilter` first, then `mkOrderBy`, then `mkOffset`, then `mkLimit`, so the resulting nodes nest with `Filter` innermost and `Limit` outermost. The `root` case here is the same pattern minus ordering — it binds only `filter`, `offset` and `limit` — while the `listA`/`listB` cases use the full four. Each of those supplies an `OrderSelection` parameterised on that field's actual mapped type (`Option[String]`, `Option[Int]`) — get the type wrong and you will fail to find an `Order[T]` instance at compile time.

If you have the four pieces as `Option`s already, prefer the `FilterOrderByOffsetLimit` constructor over hand-nesting the nodes. It builds the canonical `Filter → OrderBy → Offset → Limit` nesting for you:

```scala mdoc:compile-only
import grackle.Query.{FilterOrderByOffsetLimit, OrderSelection, Select}
import grackle.docs.QuickStartMapping.BookType

val orderTerm = BookType / "title"
val offset: Option[Int] = Some(0)
val limit: Option[Int] = Some(10)
val child: grackle.Query = Select("books")

val stack =
  FilterOrderByOffsetLimit(
    pred   = None,
    oss    = Some(List(OrderSelection[String](orderTerm, nullsLast = true))),
    offset = offset,
    limit  = limit,
    child  = child)
```

The matching `FilterOrderByOffsetLimit.unapply` lets a SQL backend destructure an arbitrary nesting back into `(pred, oss, offset, limit, child)`. If you hand-nest the nodes in a different order, both that extractor and SQL compilation may fail to recognise the pattern. For SQL backends, pass `nullsLast = nullsHigh` (a per-dialect `Boolean` from `SqlMapping`) rather than hardcoding `true`, so null ordering matches the database and stays consistent with in-memory execution.

## Paging

Grackle ships **no** Relay `Connection` type — there is no `edges`, `node`, `pageInfo`, `endCursor` or `hasNextPage` anywhere in the codebase. You assemble paging by hand from `Offset`, `Limit`, `Count`, `CursorField` and `TransformCursor`, stashing per-request paging state in the elaboration environment with `Elab.env` so that sibling fields (`items`, `total`, `hasMore`) can read it. The two patterns below are the idiomatic recipes.

### Counted paging (offset / limit / total / items)

Model a `PagedCountry` object type with `offset`, `limit`, `total` and `items` fields. The elaborator stashes a `PagingInfo(offset, limit)` in the environment when the list field is selected; each sibling field then reads it back:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlPaging1Mapping.scala", "#paging1"))
```

Walking the pieces:

- `setup(offset, limit)` calls `Elab.env(key -> PagingInfo(offset, limit))` to store the request's paging bounds under a per-config key.
- `elabItems` reads that `PagingInfo` back with `Elab.envE` and rewrites the `items` child with `FilterOrderByOffsetLimit(None, Some(order), Some(offset), Some(limit), child)` — the paged slice.
- `elabTotal` replaces the `total` child with `Count(Select("items", Select(countAttr)))`, which a SQL backend compiles to `COUNT(*)`. A real backing count source is essential here: you cannot recover an accurate total from the (already limited) `items` list.
- `genOffset` / `genLimit` are `CursorField` generators that echo the stored bounds back out, reading them from the cursor's environment.

The `selectElaborator` ties field positions to these hooks: matching `(QueryType, "countries", …)` runs `setup`, while `(PagedCountryType, "items", Nil)` and `(PagedCountryType, "total", Nil)` run `elabItems` / `elabTotal`. Because the state lives in the environment, this nests cleanly — `cities` inside each country pages with its own `CityPaging` config the same way.

### Has-more paging (overfetch and trim)

When you want a `hasMore` boolean instead of (or alongside) a total, overfetch by one row and check whether the extra row materialised. This avoids a second `COUNT` query when `items` are already being fetched:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlPaging3Mapping.scala", "#paging3"))
```

The mechanics:

- `elabItems` computes `lim0 = limit.map(_ + 1)` when `hasHasMore` — fetching one extra row — then builds the slice with `FilterOrderByOffsetLimit`. When the extra row is fetched it wraps the result in `TransformCursor(genItems, items)`.
- `genItems` runs after the rows come back: it reads the list size, and if the result is longer than `limit` it returns a `ListTransformCursor` over `elems.init`, dropping the overfetched row so the client only ever sees `limit` items.
- `elabHasMore` only adds a hidden `Count` attribute when `items` are **not** selected (`whenA(!hasItems)`); `genHasMore` then computes `hasMore` from the trimmed list size if items are present, or from that count otherwise.

The overfetch only happens when both `hasMore` and `items` are requested — the mapping checks `Elab.hasField` to decide. If you compute `hasMore` differently, replicate that conditional and always use `TransformCursor` / `ListTransformCursor` to hide the extra row, or it will leak into the response.

## A note on Relay connections

To restate the caveat, because it is the most common surprise: there is **no** built-in Relay connection helper in Grackle. "Cursor-based" paging here means hand-rolling over `Offset` / `Limit` / `Count` / `TransformCursor` plus, if you want opaque cursors, cursor predicates (`Lt`/`Gt` against an encoded sort key) — not the Relay spec's `edges`/`pageInfo`/`endCursor` shape. If you need a Relay-compliant API, you model the `Connection`, `Edge` and `PageInfo` types in your own schema and elaborate them onto these same primitives.

## See also

- [Predicates & terms reference](../reference/predicates.md) — the full `Term`/`Predicate` ADT and the `Eq[T]` vs `Order[T]` requirements.
- [Filtering & paging query nodes reference](../reference/filtering-paging-nodes.md) — exact signatures for `Filter`, `OrderBy`, `Offset`, `Limit`, `Count`, `TransformCursor` and `FilterOrderByOffsetLimit`.
- [The compiler and elaboration](../concepts/compiler-elaboration.md) — how `SelectElaborator` and `Elab.transformChild` fit into query compilation.
- [Choose and configure a SQL backend](sql-backends.md) — running these mappings against doobie or skunk, where predicates become `WHERE` clauses.
