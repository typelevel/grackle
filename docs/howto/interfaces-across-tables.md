# Interfaces across multiple tables

Grackle's SQL module requires all the implementors of a GraphQL interface — and likewise all
the branches of a union — to be represented by rows of a single table, with a column that
discriminates between the subtypes. If the subtypes of your interface live in separate tables,
as is common with pre-existing database schemas, you cannot point an interface mapping at both
tables directly: Grackle's mapping validator rejects the attempt as soon as the mapping is
constructed, reporting that "Interface implementors are split across multiple tables" and that
"All implementors of an interface must map to a single database table."

Until that restriction is lifted, the standard workaround is to merge the tables with a
database view and map the view as if it were the single table Grackle expects. This page walks
through a complete example against PostgreSQL.

## Starting point: one table per subtype

Suppose an existing schema stores films and series separately. The two tables share some
columns (`id`, `title`, `synopsis`) and each has columns of its own,

```sql
CREATE TABLE films (
    id TEXT PRIMARY KEY,
    title TEXT NOT NULL,
    synopsis TEXT,
    rating TEXT
);

CREATE TABLE series (
    id TEXT PRIMARY KEY,
    title TEXT NOT NULL,
    synopsis TEXT,
    number_of_episodes INT
);
```

with some sample data,

```sql
INSERT INTO films (id, title, synopsis, rating) VALUES
  ('f1', 'Film 1', 'A film', 'PG'),
  ('f2', 'Film 2', 'Another film', '15');

INSERT INTO series (id, title, synopsis, number_of_episodes) VALUES
  ('s1', 'Series 1', 'A series', 6);
```

The GraphQL API we want models the common columns as an interface,

```graphql
type Query {
  entities: [Entity!]!
}
interface Entity {
  id: ID!
  title: String!
  synopsis: String
}
type Film implements Entity {
  id: ID!
  title: String!
  synopsis: String
  rating: String
}
type Series implements Entity {
  id: ID!
  title: String!
  synopsis: String
  numberOfEpisodes: Int
}
```

## Merging the tables with a view

A view gives Grackle the single-table shape it needs without changing how the data is stored.
Each branch of a `UNION ALL` contributes one subtype's rows: a string literal supplies the
discriminator column, the shared columns line up, and each subtype-specific column is
NULL-padded in the branches it doesn't belong to,

```sql
CREATE VIEW entities AS
  SELECT
    id,
    'FILM' AS entity_type,
    title,
    synopsis,
    rating,
    NULL :: INT AS number_of_episodes
  FROM films
  UNION ALL
  SELECT
    id,
    'SERIES' AS entity_type,
    title,
    synopsis,
    NULL :: TEXT AS rating,
    number_of_episodes
  FROM series;
```

One thing to check before relying on this: the interface's key must be unique across *all* the
branches. Here film and series ids never collide; if yours can, synthesize a namespaced key in
the view instead, for example `'film:' || id AS id`.

## Mapping the view

From here on nothing is special: the mapping is an ordinary single-table interface mapping, and
never needs to know that `entities` is a view. The `TableDef` describes the view's columns, the
`SqlInterfaceMapping` carries the shared fields, and each implementor adds its own,

```scala
import org.typelevel.doobie.Meta

import grackle._
import grackle.Predicate._
import grackle.doobie.postgres.DoobiePgMapping
import grackle.syntax._

trait EntitiesMapping[F[_]] extends DoobiePgMapping[F] {

  object entities extends TableDef("entities") {
    val id = col("id", Meta[String])
    val entityType = col("entity_type", Meta[String])
    val title = col("title", Meta[String])
    val synopsis = col("synopsis", Meta[String], nullable = true)
    val rating = col("rating", Meta[String], nullable = true)
    val numberOfEpisodes = col("number_of_episodes", Meta[Int], nullable = true)
  }

  val schema =
    schema"""
      type Query {
        entities: [Entity!]!
      }
      interface Entity {
        id: ID!
        title: String!
        synopsis: String
      }
      type Film implements Entity {
        id: ID!
        title: String!
        synopsis: String
        rating: String
      }
      type Series implements Entity {
        id: ID!
        title: String!
        synopsis: String
        numberOfEpisodes: Int
      }
    """

  val QueryType = schema.ref("Query")
  val EntityType = schema.ref("Entity")
  val FilmType = schema.ref("Film")
  val SeriesType = schema.ref("Series")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings = List(
          SqlObject("entities")
        )
      ),
      SqlInterfaceMapping(
        tpe = EntityType,
        discriminator = entityDiscriminator,
        fieldMappings = List(
          SqlField("id", entities.id, key = true),
          SqlField("entityType", entities.entityType, discriminator = true, hidden = true),
          SqlField("title", entities.title),
          SqlField("synopsis", entities.synopsis)
        )
      ),
      ObjectMapping(
        tpe = FilmType,
        fieldMappings = List(
          SqlField("rating", entities.rating)
        )
      ),
      ObjectMapping(
        tpe = SeriesType,
        fieldMappings = List(
          SqlField("numberOfEpisodes", entities.numberOfEpisodes)
        )
      )
    )

  lazy val entityDiscriminator = new SqlDiscriminator {
    def discriminate(c: Cursor): Result[Type] =
      for {
        et  <- c.fieldAs[String]("entityType")
        tpe <- et match {
                 case "FILM"   => FilmType.success
                 case "SERIES" => SeriesType.success
                 case other    => Result.internalError(s"Unexpected entity_type: $other")
               }
      } yield tpe

    def narrowPredicate(subtpe: Type): Result[Predicate] =
      subtpe match {
        case FilmType   => Eql(EntityType / "entityType", Const("FILM")).success
        case SeriesType => Eql(EntityType / "entityType", Const("SERIES")).success
        case _          => Result.internalError(s"Invalid discriminator: $subtpe")
      }
  }
}
```

The discriminator ties the two directions together: `discriminate` inspects the `entity_type`
column to decide which GraphQL type a row is, and `narrowPredicate` produces the SQL predicate
that restricts the view to one subtype when a query asks for it.

## Querying it

```graphql
query {
  entities {
    id
    title
    synopsis
    ... on Film {
      rating
    }
    ... on Series {
      numberOfEpisodes
    }
  }
}
```

yields

```json
{
  "data" : {
    "entities" : [
      { "id" : "f1", "title" : "Film 1", "synopsis" : "A film", "rating" : "PG" },
      { "id" : "f2", "title" : "Film 2", "synopsis" : "Another film", "rating" : "15" },
      { "id" : "s1", "title" : "Series 1", "synopsis" : "A series", "numberOfEpisodes" : 6 }
    ]
  }
}
```

(The order of the rows above reflects PostgreSQL's incidental evaluation order for this
particular `UNION ALL` view, not a guarantee Grackle or the view makes — don't rely on entities
of a given subtype coming back in any particular position relative to the others.)

## Caveats

- Use `UNION ALL`, not `UNION` — the branches cannot produce duplicate rows (they carry
  distinct discriminator values), and `UNION`'s deduplication forces a needless sort of the
  whole view.
- Query performance depends on your database pushing predicates down into the view's branches.
  PostgreSQL does this well for simple `UNION ALL` views like the one above; check your query
  plans if the view grows cleverer.
- Joins from a subtype to other tables work unchanged — join on the view's columns exactly as
  you would on a table's.
- Views like this are typically not updatable, which doesn't matter here: Grackle only ever
  reads from them.
- A union type across multiple tables works exactly the same way — the view just has no shared
  columns to line up, other than the key and the discriminator.
