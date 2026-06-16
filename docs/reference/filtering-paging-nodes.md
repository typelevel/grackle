# Filtering & paging query nodes reference

This page is the reference for the `Query` nodes that filter, sort, page, and count a list-producing child: `Filter`, `OrderBy` (with `OrderSelections` / `OrderSelection`), `Offset`, `Limit`, `Count`, and `TransformCursor`, plus the `FilterOrderByOffsetLimit` assembler and its fixed nesting order. All of these live in `object grackle.Query` (module `grackle-core`, `modules/core/src/main/scala/query.scala`); bring them into scope with `import grackle.Query._`. It is aimed at developers assembling paging stacks inside a `SelectElaborator`. For the predicate leaves a `Filter` holds (`Eql`, `Lt`, `In`, `Contains`, …) and the `Term` / `Path` you sort by, see the [predicates reference](predicates.md); for the rest of the `Query` algebra see the [query algebra reference](query-algebra.md); for a task-oriented walkthrough see [how to filter, order and page](../how-to/filtering-ordering-paging.md).

These nodes wrap a `child` query and compose by nesting. Each is interpretable: the in-memory interpreter evaluates them against a [`Cursor`](cursor.md), while the SQL backend compiles them to `WHERE` / `ORDER BY` / `LIMIT` / `OFFSET` / `COUNT` clauses. None of them is a ready-made Relay connection — there is no built-in `edges` / `pageInfo` / `endCursor` type in Grackle, so counted and cursor-style paging are assembled by hand from the primitives below (see [`FilterOrderByOffsetLimit`](#filterorderbyoffsetlimit) and [`TransformCursor`](#transformcursor)).

## `Filter`

`Filter` retains only the elements of `child` that satisfy a [`Predicate`](predicates.md), then continues with `child`.

| Node | Signature | Semantics |
| --- | --- | --- |
| `Filter` | `case class Filter(pred: Predicate, child: Query) extends Query` | Keeps every element of the list-producing `child` for which `pred` evaluates to `true`. |

`pred` is a `Predicate` (a `Term[Boolean]`), so it can be any leaf or combination from the [predicates reference](predicates.md) — for example `Filter(Eql(CountryType / "code", Const(code)), child)` for an exact match, or `Filter(In(CityType / "language", languages), child)` for set membership. Wrapping a `Filter` in `Unique` turns a list filter into a single-result lookup. The filter is applied during [elaboration](../concepts/compiler-elaboration.md) inside a `SelectElaborator`, typically via `Elab.transformChild`.

## `OrderBy`, `OrderSelections`, `OrderSelection`

Ordering is a single `Query` node, `OrderBy`, carrying an `OrderSelections` list of sort keys. Each key is an `OrderSelection` over a `Term[T]`.

| Type | Signature | Purpose |
| --- | --- | --- |
| `OrderBy` | `case class OrderBy(selections: OrderSelections, child: Query) extends Query` | Orders the list-producing `child` by `selections`. |
| `OrderSelections` | `case class OrderSelections(selections: List[OrderSelection[_]])` | An ordered list of sort keys, applied in priority order. Its `def order(lc: Seq[Cursor]): Seq[Cursor]` sorts an in-memory `Seq[Cursor]`, comparing by each key until one breaks the tie. |
| `OrderSelection` | `case class OrderSelection[T: Order](term: Term[T], ascending: Boolean = true, nullsLast: Boolean = true)` | One sort key over a `Term[T]`. Requires an implicit `Order[T]`. |

An `OrderSelection` declares two members worth noting:

| Member | Signature | Meaning |
| --- | --- | --- |
| `apply` | `def apply(x: Cursor, y: Cursor): Int` | Compares two cursors by this key, honouring `ascending` and `nullsLast`. |
| `subst` | `def subst(term: Term[T]): OrderSelection[T]` | Returns a copy with a different `term` (same direction and null placement). |

The two flags control direction and null placement:

- `ascending` (default `true`) — `true` sorts low-to-high, `false` reverses it.
- `nullsLast` (default `true`) — `true` places nulls after non-null values; `false` places them first. SQL backends do **not** rely on this default: they pass `nullsLast = nullsHigh`, a per-dialect `Boolean` from `SqlMapping`, so null ordering matches the database. Hardcoding `true` can make in-memory and SQL results differ.

`T` must match the field's mapped type, including optionality: order a nullable string field with `OrderSelection[Option[String]](ElemAType / "elemA")`, a non-null integer with `OrderSelection[Int](CountryType / "population")`. Forgetting the `Order[T]` instance, or parameterizing on the wrong static type, is a compile error.

## `Offset` and `Limit`

Offset and limit are two separate nodes; combine them for a page window.

| Node | Signature | Semantics |
| --- | --- | --- |
| `Offset` | `case class Offset(num: Int, child: Query) extends Query` | Drops the first `num` elements of the list-producing `child`. |
| `Limit` | `case class Limit(num: Int, child: Query) extends Query` | Takes the first `num` elements of the list-producing `child`. |

The `Query` ADT does **not** validate `num`. An `Offset(-1, …)` or `Limit(0, …)` is a well-formed node; rejecting out-of-range arguments is the elaborator's job. The example mappings reject `offset < 0` and `limit <= 0` with [`Result.failure`](result-problem.md) before constructing the node — see the [how-to](../how-to/filtering-ordering-paging.md) for that pattern.

## `Count`

`Count` replaces a list with the number of its top-level elements — the building block for a paging `total`.

| Node | Signature | Semantics |
| --- | --- | --- |
| `Count` | `case class Count(child: Query) extends Query` | Computes the number of top-level elements of `child`. Compiles to SQL `COUNT`. |

`Count` *replaces* the node it is built from rather than wrapping a selection, so you typically construct it directly in the elaborator, e.g. `Count(Select("items", Select(countAttr)))`. An accurate total needs a real backing count source — you cannot recover it from an already-`Limit`ed items list, which is why counted paging issues a separate `Count` (or maps `total` to a dedicated column) alongside the limited `items`.

## `TransformCursor`

`TransformCursor` post-processes the result cursor with an arbitrary function. It is the hook for cursor-style "has more" paging, where you overfetch one extra row and then trim it before the client sees it.

| Node | Signature | Semantics |
| --- | --- | --- |
| `TransformCursor` | `case class TransformCursor(f: Cursor => Result[Cursor], child: Query) extends Query` | Computes a continuation [`Cursor`](cursor.md) from the current one via `f`, after `child` has produced it. |

A typical use fetches `limit + 1` rows, then wraps the items query in a `TransformCursor` whose `f` drops the surplus row (using `ListTransformCursor` to rebuild the list cursor with one fewer element). The "extra row exists" fact is what `hasMore` is computed from; the extra row itself is hidden by the transform. If you select only `hasMore` and not `items`, the example mappings skip the overfetch and use a `Count` instead. This is hand-rolled cursor paging built on these primitives — not a Relay connection type, which Grackle does not provide.

## `FilterOrderByOffsetLimit`

`FilterOrderByOffsetLimit` is the idiomatic way to assemble the whole four-node stack from `Option`s, and to destructure it again. It is an `object` with a matching `apply`/`unapply`, not a `Query` node of its own.

| Member | Signature |
| --- | --- |
| `apply` | `def apply(pred: Option[Predicate], oss: Option[List[OrderSelection[_]]], offset: Option[Int], limit: Option[Int], child: Query): Query` |
| `unapply` | `def unapply(q: Query): Option[(Option[Predicate], Option[List[OrderSelection[_]]], Option[Int], Option[Int], Query)]` |

`apply` nests the nodes in a **fixed order** — `Filter` innermost, then `OrderBy`, then `Offset`, then `Limit` outermost — skipping any whose `Option` is `None`:

```text
Limit( Offset( OrderBy( Filter( child ) ) ) )
  └─ limit    └─ offset   └─ oss     └─ pred
   (outermost)                       (innermost)
```

`unapply` peels that same stack back off in reverse, matching an outer `Limit`, then `Offset`, then `OrderBy`, then `Filter`, and returning `None` only if none of the four is present. Interpreters (notably the SQL backend) rely on this canonical shape to recognize a paging stack. **If you hand-nest the four nodes in a different order, `unapply` will not match and SQL compilation may not recognize the pattern** — prefer `FilterOrderByOffsetLimit` over building `Filter` / `OrderBy` / `Offset` / `Limit` by hand.

The following snippet (from a counted-paging SQL mapping) shows `FilterOrderByOffsetLimit` used to build the `items` sub-query and `Count` used to build the `total`, with paging state threaded through the elaboration [environment](context-env.md):

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlPaging1Mapping.scala", "#paging1"))
```

The inner `PagingInfo.elabItems` calls `FilterOrderByOffsetLimit(None, Some(List(OrderSelection(orderTerm, nullsLast = nullsHigh))), Some(offset), Some(limit), child)` — no filter, one order key with the dialect's `nullsHigh`, and the page window — while `elabTotal` replaces the child with `Count(Select("items", Select(countAttr)))`. `setup` stashes a `PagingInfo` under `key` in the `Elab` environment with `Elab.env`, and the sibling `items` and `total` fields read it back with `Elab.envE` (the `elabItems` / `elabTotal` on `PagingConfig` delegate to the stashed `PagingInfo`). The `genOffset` / `genLimit` methods return the stashed `offset` / `limit` as `Result[Int]`; the mapping wires them into `CursorField`s on the paged object so those values are echoed to the client. This mapping targets a SQL backend, so the snippet is shown as source rather than compiled here.

## See also

- [Predicates & terms reference](predicates.md) — the `Predicate` / `Term` / `Path` leaves a `Filter` holds and the terms an `OrderSelection` sorts by.
- [Query algebra reference](query-algebra.md) — the full `Query` ADT these nodes belong to.
- [How to filter, order and page](../how-to/filtering-ordering-paging.md) — the task-oriented recipe that wires these nodes from GraphQL arguments.
- [The compiler and elaboration](../concepts/compiler-elaboration.md) — how `Elab.transformChild` installs these nodes during compilation.
- [Cursor reference](cursor.md) — the focus that `TransformCursor` and `OrderSelection` operate on.
