# SqlMapping reference

This page is the information-oriented reference for Grackle's SQL field mappings: the field-mapping types (`SqlField`, `SqlObject`, `SqlJson`), the `Join` forms, the `ColumnRef`/`TableDef`/`RootDef`/`col` column vocabulary, the `SqlInterfaceMapping`/`SqlUnionMapping`/`SqlDiscriminator` constructors, the `FailedJoin` sentinel, the per-backend `col` signatures for doobie and skunk, and the consistency rules the mapping validator enforces. It is for developers building SQL-backed mappings who want exact signatures rather than a walkthrough — for task-oriented recipes see [Choose and configure a SQL backend](../how-to/sql-backends.md). Every type below lives in the database-agnostic `SqlMappingLike[F]` trait in `modules/sql-core/src/main/scala/SqlMapping.scala` unless noted.

## SqlField

`SqlField` maps a GraphQL leaf (scalar/enum) field to a single SQL column.

```scala
case class SqlField(
    fieldName: String,
    columnRef: ColumnRef,
    key: Boolean = false,
    discriminator: Boolean = false,
    hidden: Boolean = false,
    associative: Boolean = false
)(implicit val pos: SourcePos) extends SqlFieldMapping
```

The four booleans are the flags you tune per column:

| Flag | Default | Meaning |
| --- | --- | --- |
| `key` | `false` | The column is an identity column used to group and assemble rows. Every object-type mapping must have at least one key field (direct or inherited). |
| `hidden` | `false` | The column is fetched and usable for joins/keys/discriminators but omitted from the GraphQL response. Use it on columns that exist only to support the mapping (foreign keys, discriminator columns). |
| `discriminator` | `false` | The column is read by an interface/union [`SqlDiscriminator`](#sqldiscriminator) to choose the concrete type of a row. |
| `associative` | `false` | The key is *not* a database primary key and may repeat across rows (for example a language code shared by many countries), so rows still group correctly. Only valid together with `key = true`. |

## SqlObject

`SqlObject` maps a GraphQL sub-object or list field. Its behaviour is decided entirely by whether you pass any [`Join`](#join)s.

```scala
case class SqlObject(fieldName: String, joins: List[Join])(implicit val pos: SourcePos)
    extends SqlFieldMapping

object SqlObject {
  def apply(fieldName: String, joins: Join*)(implicit pos: SourcePos): SqlObject
}
```

| Form | Semantics |
| --- | --- |
| `SqlObject("synopses")` (no `Join`) | *Embedding* — the child object is built from columns on the parent's own row/table. The child's `SqlField`s point at the parent table, and the child mapping repeats the parent key (usually `hidden = true`) so rows group. |
| `SqlObject("cities", Join(a, b))` | *Joining* — Grackle emits a SQL join from parent columns to child columns and assembles the child from the joined rows. |
| `SqlObject("x", j1, j2, ...)` | *Chained joins* — several `Join`s pass through intermediate (associative) tables. |

The canonical multi-table example is the world mapping. The snip below shows `SqlField` key/hidden flags, one-to-many and many-to-one `SqlObject` joins, and an `associative` key on `Language.language`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlWorldMapping.scala", "#world_typemappings"))
```

Here `Country.code` and `City.id` are `key = true, hidden = true` — fetched to assemble rows, absent from GraphQL output. `SqlObject("cities", Join(country.code, city.countrycode))` is a one-to-many join and `SqlObject("country", Join(city.countrycode, country.code))` is the reverse many-to-one join reusing the same column pair. `Language.language` is `associative = true` because a language code repeats across many countries.

## SqlJson

`SqlJson` maps a GraphQL object/interface field onto a single `jsonb` column. Unlike `SqlObject`, the JSON sub-tree below it is navigated entirely in-process by a `CirceCursor` — no further SQL is issued for nested objects, enums, arrays, inline fragments, or `__typename`.

```scala
case class SqlJson(fieldName: String, columnRef: ColumnRef)(implicit val pos: SourcePos)
    extends SqlFieldMapping {
  def subtree: Boolean = true
}
```

- `subtree = true` signals that child selections are resolved from the decoded JSON, not SQL-mapped.
- Because the sub-tree is a Circe value, you **cannot** join out of, filter on, or order by fields inside it.
- A non-nullable `SqlJson` field whose column is actually NULL/absent errors with `expected jsonb value`.
- The same `jsonb` column can back both a nullable and a non-null GraphQL field (the `record`/`nonNullRecord` pattern in the jsonb tests).

For a worked recipe see [Map a jsonb column with SqlJson](../how-to/jsonb-columns.md).

## Join

A `Join` is one join's ON conditions, expressed as parent → child column pairs.

```scala
case class Join(conditions: List[(ColumnRef, ColumnRef)])

object Join {
  def apply(parent: ColumnRef, child: ColumnRef): Join  // single-pair sugar
}
```

| Form | Produces |
| --- | --- |
| `Join(parentCol, childCol)` | A single ON equality `parentCol = childCol`. |
| `Join(List((p1, c1), (p2, c2)))` | A composite-key join — all pairs are ANDed in the ON clause. Each parent column is usually `key = true`. |
| `SqlObject("x", j1, j2, ...)` | A chain of joins through intermediate tables; each `Join`'s parent/child tables must line up with its neighbours. |

`conditions.head` determines a join's parent and child tables, so an empty condition list is invalid (`NoJoinConditions`), and chained joins whose tables do not line up fail with `MisalignedJoins`/`InconsistentJoinConditions`.

## ColumnRef, TableDef, RootDef, TableName and col

Every mapped column is a `ColumnRef`. You never construct it directly — you call `col(name, codec)` inside a `TableDef` or `RootDef`, which supplies the table name (via an implicit `TableName`), the Scala type name, and the source position.

```scala
case class ColumnRef(
    table: String,
    column: String,
    codec: Codec,
    scalaTypeName: String,
    pos: SourcePos)
```

```scala
case class TableName(name: String)
object TableName {
  val rootName = "<root>"
  val rootTableName = TableName(rootName)
}

class TableDef(name: String) {
  implicit val tableName: TableName = TableName(name)
}

class RootDef {
  implicit val tableName: TableName = TableName.rootTableName
}
```

| Type | Purpose |
| --- | --- |
| `ColumnRef` | A reference to one SQL column plus its codec. **Equality and `hashCode` use only `(table, column)`** — the codec is ignored, so the same physical column referenced with two different codecs compares as equal. |
| `TableDef(name)` | Base class for a per-table column-definition object. Its implicit `TableName` binds enclosed `col(...)` calls to `name`. You subclass it: `object country extends TableDef("country") { ... }`. |
| `RootDef` | Like `TableDef` but binds columns to the synthetic `<root>` table (`TableName.rootTableName`), for computed/aggregate root fields such as `numCountries`. |
| `TableName` | The implicit that `col` reads to know which table a column belongs to. |

The generic `col` and the abstract codec surface it consumes are defined in the backend-neutral test base `SqlTestMapping`; concrete backends fill `TestCodec`/`Codec` with real types. This snip is the full abstract vocabulary (`bool`, `text`, `int4`, `jsonb`, `nullable`, `list`, …) plus the generic `col`:

```scala mdoc:passthrough
println(grackle.docs.Output.snip("modules/sql-core/src/test/scala/SqlTestMapping.scala", "#sql_codecs"))
```

Note that `col[T](colName, codec)` requires an implicit `TableName` (from the enclosing `TableDef`/`RootDef`), a `TypeName[T]`, and a `SourcePos`, and returns a `ColumnRef`. `nullable(c)` wraps a codec to mark the column optional, and `list(c)` lifts it to an array column.

## SqlInterfaceMapping

An interface is mapped with `SqlInterfaceMapping` over a shared (super-) table, plus an [`SqlDiscriminator`](#sqldiscriminator). Concrete subtypes are ordinary `ObjectMapping`s that add their extra columns on the **same** table.

```scala
sealed trait SqlInterfaceMapping extends ObjectMapping with SqlDiscriminatedType

object SqlInterfaceMapping {
  def apply(tpe: NamedType, discriminator: SqlDiscriminator)
           (fieldMappings: FieldMapping*)(implicit pos: SourcePos): ObjectMapping
  def apply(tpe: NamedType, fieldMappings: List[FieldMapping], discriminator: SqlDiscriminator)
           (implicit pos: SourcePos): ObjectMapping
  def apply(path: Path, discriminator: SqlDiscriminator)
           (fieldMappings: FieldMapping*)(implicit pos: SourcePos): ObjectMapping
  def apply(predicate: MappingPredicate, discriminator: SqlDiscriminator)
           (fieldMappings: FieldMapping*)(implicit pos: SourcePos): ObjectMapping
}
```

Note the two common arities: varargs `fieldMappings` with `discriminator` first, **or** the named-argument form `(tpe, fieldMappings: List, discriminator)` used in the tests. The interface mapping holds the shared columns and exactly one `SqlField(..., discriminator = true)`.

## SqlUnionMapping

A union is mapped with `SqlUnionMapping`. All members must share one table; the union mapping holds **only** hidden key and hidden discriminator fields, and each member `ObjectMapping` maps its own column on the shared table.

```scala
sealed trait SqlUnionMapping extends ObjectMapping with SqlDiscriminatedType

object SqlUnionMapping {
  def apply(tpe: NamedType, discriminator: SqlDiscriminator)
           (fieldMappings: FieldMapping*)(implicit pos: SourcePos): ObjectMapping
  def apply(tpe: NamedType, fieldMappings: List[FieldMapping], discriminator: SqlDiscriminator)
           (implicit pos: SourcePos): ObjectMapping
  def apply(path: Path, discriminator: SqlDiscriminator)
           (fieldMappings: FieldMapping*)(implicit pos: SourcePos): ObjectMapping
  def apply(predicate: MappingPredicate, discriminator: SqlDiscriminator)
           (fieldMappings: FieldMapping*)(implicit pos: SourcePos): ObjectMapping
}
```

Every field in the union mapping must be `hidden = true`, and you may not put `SqlObject` sub-objects or `SqlJson` fields directly in it — map those on the member `ObjectMapping`s. See [Map interfaces and unions to SQL](../how-to/interfaces-unions.md) for full examples.

## SqlDiscriminator

Both interface and union mappings take an `SqlDiscriminator`. It has two responsibilities: filtering rows to a subtype (via a `Predicate`) and computing a fetched row's concrete `Type`.

```scala
trait SqlDiscriminator {
  /** A predicate selecting only rows of the given subtype. */
  def narrowPredicate(tpe: Type): Result[Predicate]

  /** The concrete type of the value at the cursor. */
  def discriminate(cursor: Cursor): Result[Type]
}
```

`discriminate` typically reads the discriminator column with `cursor.fieldAs[...]` and maps the value to a subtype `Type`; `narrowPredicate` returns an `Eql(... / "discriminatorField", Const(value))` predicate, and `Result.internalError(...)` for an unknown subtype. An internal error here is raised into the effect `F` — it does not appear in the GraphQL `errors` array. See [Construct, accumulate and report errors](../how-to/errors.md) for the distinction between `InternalError` and the `Problem`s that surface in the response.

## FailedJoin

When an outer join produces no child row, Grackle decodes the non-nullable child columns to the `FailedJoin` sentinel rather than `null`, so it can distinguish a genuinely-absent join target from a SQL `NULL` value.

```scala
// modules/sql-core/src/main/scala/FailedJoin.scala
package grackle.sql

case object FailedJoin
```

Code that inspects raw decoded row arrays must account for `FailedJoin` appearing in place of a missing non-nullable value.

## Backend col signatures: doobie vs skunk

`SqlMappingLike` leaves the `Codec`/`Encoder`/`Fragment` types and the `col` helper abstract; each backend trait fills them. In both, the `Codec` is a `(codec, Boolean)` tuple where the boolean is the nullability flag — but the backends differ in how you specify nullability.

| | doobie (`DoobieMappingLike`) | skunk (`SkunkMappingLike`) |
| --- | --- | --- |
| `Codec` | `(Meta[_], Boolean)` | `(skunk.Codec[_], Boolean)` |
| `Encoder` | `(Put[_], Boolean)` | `skunk.Encoder[_]` |
| `Fragment` | `DoobieFragment` | `skunk.AppliedFragment` |
| `col` | `col(name, Meta[T], nullable = false)` | `col(name, skunk.Codec[T])` |
| Nullability | explicit `nullable` boolean argument | inferred from `Option[T]` via an implicit `IsNullable` (use the codec's `.opt`) |

```scala
// DoobieMappingLike
def col[T](colName: String, codec: Meta[T], nullable: Boolean = false)
          (implicit tableName: TableName, typeName: TypeName[T], pos: SourcePos): ColumnRef

// SkunkMappingLike
def col[T](colName: String, codec: skunk.Codec[T])
          (implicit tableName: TableName, typeName: NullableTypeName[T],
           isNullable: IsNullable[T], pos: SourcePos): ColumnRef
```

So with doobie you write `col("indepyear", Meta[Int], nullable = true)`, while with skunk you write `col("indepyear", int4.opt)` and the optionality is derived from the `Option`-valued codec.

## Validator consistency rules

The mapping validator checks each SQL mapping against the schema and rejects inconsistent mappings with a named error. The most common ones:

| Error | Triggered when |
| --- | --- |
| `NoKeyInObjectTypeMapping` | An object-type mapping has no `key = true` field, direct or inherited (*"Object type mappings must include at least one direct or inherited key field mapping"*). |
| `SplitObjectTypeMapping` / `SplitEmbeddedObjectTypeMapping` | The `SqlField`/`SqlObject` columns of one (non-interface, non-union) object mapping come from more than one table. To span tables, use `SqlObject` + `Join`. |
| `SplitInterfaceTypeMapping` / `SplitUnionTypeMapping` | An interface/union subtype is mapped onto a different table from the interface/union mapping. Subtypes must add columns on the **same** shared table. |
| `NonHiddenUnionFieldMapping` | A field in a union mapping is not `hidden = true`. |
| `IllegalSubobjectInUnionTypeMapping` | An `SqlObject` sub-object is placed directly in a union mapping. |
| `IllegalJsonInUnionTypeMapping` | An `SqlJson` field is placed directly in a union mapping. |
| `NoDiscriminatorInObjectTypeMapping` | An interface/union mapping has no discriminator field. |
| `IllegalPolymorphicDiscriminatorFieldMapping` | The discriminator field is itself polymorphic. |
| `AssocFieldNotKey` | `associative = true` is set on a field that is not also `key = true`. |
| `NoJoinConditions` | An `SqlObject` is given a `Join` with an empty condition list. |
| `MisalignedJoins` / `InconsistentJoinConditions` | Chained multi-`Join` parent → child tables do not line up. |

To run the validator and read these errors, see [Validate a mapping and read the failures](../how-to/validate-mappings.md).

## See also

- [Choose and configure a SQL backend](../how-to/sql-backends.md) — task recipes for doobie/skunk mappings.
- [Map interfaces and unions to SQL](../how-to/interfaces-unions.md) — full interface/union worked examples.
- [Map a jsonb column with SqlJson](../how-to/jsonb-columns.md) — `SqlJson` in practice.
- [Filter, sort and page a field](../how-to/filtering-ordering-paging.md) — predicates and paging over SQL.
- [Validate a mapping and read the failures](../how-to/validate-mappings.md) — running the validator and reading the errors listed above.
- [Mapping types reference](mapping-types.md) — the core (non-SQL) mapping vocabulary.
- [Predicates & terms reference](predicates.md) — building the `Predicate`s a discriminator returns.
