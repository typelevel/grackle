// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package sql

import cats.data.IorT
import cats.syntax.all._
import edu.gemini.grackle.Query._
import edu.gemini.grackle.util._
import fs2.Stream
import org.tpolecat.sourcepos.SourcePos

/**
 * An extension of `SqlMapping` that provides a top-level mapping equivalent to `SqlRoot` that
 * provides results via the [[https://relay.dev/graphql/connections.htm cursor connection]] pattern.
 *
 * == Overview ==
 *
 * `SqlConnection` is equivalent to `SqlRoot` but adds `Offset` and `Limit` to the underlying
 * `Select` to implement paging, and returns results that include paging metadata.
 *
 * == Schema Requirements ==
 *
 * The mapped schema must include the following types, which are used by all cursor connections:
 *
 * {{{
 *   scalar Cursor
 *
 *   type PageInfo {
 *     hasPreviousPage: Boolean!
 *     hasNextPage: Boolean!
 *     startCursor: Cursor!
 *     endCursor: Cursor!
 *   }
 * }}}
 *
 * The `Cursor` scalar type is specified to be a string, which is the case here. In the current
 * implementation the string encodes the absolute row number of the underlying resultset, but this
 * may not be the case in the future and such values should be treated as opaque.
 *
 * The schema must also include connection and edge types specific to the underlying element type,
 * of the following form:
 *
 * {{{
 *   type CountryConnection {
 *     edges: [WoozleEdge!]!
 *     pageInfo: PageInfo!
 *   }
 *
 *   type CountryEdge {
 *     node: Country!
 *     cursor: Cursor!
 *   }
 * }}}
 *
 * The top-level property for a connection can be parameterized as desired, and should return the
 * non-nullable connection type. Note that it must be a top-level property on the `Query` object;
 * connections nested in other object types are not supported [yet].
 *
 * {{{
 *   type Query {
 *     countries(first: Int, after: Cursor): CountryConnection!
 *   }
 * }}}
 *
 * Such a property should always have a `first` parameter to specify how many edges to return, and
 * will typically have an `after` parameter to allow the user to specify the starting point. Other
 * parameters may appear as desired.
 *
 * == Mapping Requirements ==
 *
 * `SqlConnectionMapping` is equivalent to `SqlMapping` in all respects other than the addition of
 * the `SqlConnection` mapping type.
 *
 * {{{
 *   val typeMappings =
 *     List(
 *       ObjectMapping(
 *         tpe = QueryType,
 *         fieldMappings = List(
 *           SqlConnection("countries"), // Same usage as SqlRoot, but now we get a connection
 *         )
 *       ),
 *       ObjectMapping(
 *         tpe = CountryType,
 *         fieldMappings = List(
 *           SqlField("code", country.code, key = true, hidden = true),
 *           SqlField("name", country.name),
 *           ...
 * }}}
 *
 * The normal elaboration of a `SqlRoot` query will match a top-level `Select`. With `SqlConnection`
 * the equivalent `Select` is buried under `[connection]/edges/node`. For convenience we
 * provide a method to let you elaborate just this part of the query, ignorning the portion dealing
 * with the outer metadata structure.
 *
 * In this example we extract the `first` and `after` arguments for the top-level `Select` using
 * extractor objects (see `WorldConnectionSpec.scala`) and pass them along with the query itself to
 * `SqlConnectionMapping.updateNodeQuery`, which will add corresponding `Offset` and `Limit` nodes
 * to the elaborated query we return, and in turn produce the fully elaborated top-level query,
 * which we return.
 *
 * {{{
 *   override val selectElaborator: QueryCompiler.SelectElaborator =
 *    new QueryCompiler.SelectElaborator(
 *      Map {
 *        QueryType -> {
 *          case q @ Query.Select("countries", List(First(first), After(after)), _) =>
 *            SqlConnectionMapping.updateNodeQuery(q, first, after) {
 *              case Select("node", Nil, child) =>
 *                val ord = Query.OrderBy(OrderSelections(List(OrderSelection(Path.UniquePath[String](List("code"))))), child)
 *                Result(Select("node", Nil, ord))
 *              case o => Result.failure("SqlConnectionMapping.updateNodeQuery didn't match!")
 *            }
 *        }
 *      }
 *    )
 *}}}
 *
 * Note that cursor connection queries must always have an `OrderBy` node, otherwise results may
 * appear in an unpredictable order.
 */
trait SqlConnectionMapping[F[_]] extends SqlMapping[F] {
  import SqlConnectionMapping.{ cursorToOffset, offsetToCursor, updateNodeQuery, findNodeQuery }

  case class SqlConnection(
    fieldName: String,
    orootTpe: Option[Type] = None,
    mutation: Mutation = Mutation.None
  )(implicit val pos: SourcePos) extends RootMapping {

    def withParent(tpe: Type): RootMapping = copy(orootTpe = Some(tpe))

    // The connection type, which is the type of `fieldName`.
    private val findConnType: Result[Type] =
      Result.fromOption(
        orootTpe.flatMap(_.field(fieldName)),
        "SqlConnection: cannot determine the root connection type."
      )

    // The type of `[fieldName]/edges/node`
    private val findNodeType: Result[Type] =
      Result.fromOption(
        for {
          root  <- orootTpe
          self  <- root.field(fieldName)
          edges <- self.field("edges")
        } yield edges.underlying,
        "SqlConnection: can't get the edge type."
      )

    // The `Select("node", ...) node in `query`
    private def nodeQuery(query: Query): Result[Query] =
      Result.fromOption(
        findNodeQuery(query),
        "SqlConnection: Can't find a `node` selection in the provided query."
      )

    // Bit of a hack, but we "just know" that `SqlRoot` hands back a `SqlCursor` that's a list of
    // other `SqlCursor`s and in order to `.copy` them below we have to narrow the type.
    private def narrowToSqlCursor(c: Cursor): Result[SqlCursor] =
      c match {
        case sqlc: SqlCursor => Result(sqlc)
        case other           => Result.failure(s"SqlConnection: Expected SqlCursor, found ${other.getClass.getName}")
      }

    def offset(env: Cursor.Env): Result[Int] =
      env.get[String]("after").fold(Result(0))(cursorToOffset)

    def limit(env: Cursor.Env, child: Query): Query =
      env.get[Int]("first").fold(child)(Limit(_, child))

    def cursor(query: Query, env: Cursor.Env, resultName: Option[String]): Stream[F,Result[(Query, Cursor)]] = {
      def lift[A](fa: Result[A]) = IorT.fromIor[Stream[F,*]](fa)
      for {
        child   <- lift(nodeQuery(query))                   // Find the `Select("node", ...` subquery.
        offset  <- lift(offset(env))                        // Get offset and limit from `env` (we need them in a few places here, which is why we don't do this during elaboration)
        limit   <- lift(env.getResult[Int]("first"))
        paged    = Limit(limit + 1, Offset(offset, child))  // Add `Limit` and `Offset` to the child query (limit + 1 to detect more pages)
        connTpe <- lift(findConnType)                       // Find the GraphQL type of the `node` property
        nodeTpe <- lift(findNodeType)
        sqlRoot  = SqlRoot("node", Some(nodeTpe))           // We delegate to `SqlRoot` here for the hard work
        pair    <- IorT(sqlRoot.cursor(paged, env, None))
        (q, c)   = pair
        child2   = Select("node", Nil, q)                   // Replace the node query with the updated one returned from `SqlRoot`
        qʹ      <- lift(updateNodeQuery(query)(_ => Result(child2)))
        cs      <- lift(c.asList)                           // We should have a list of cursors
        sqlCs   <- lift(cs.traverse(narrowToSqlCursor))     // Which are all `SqlCursor`s, which we need to know here so we can `.copy` them below.
      } yield (
        qʹ,

        // This is the structure we return. It's all static until we get down to `node`, which
        // delegates to the underlying `SqlCursor` for each returned row.
        MapCursor(Cursor.Context(fieldName, resultName, connTpe), env = env)
          .withField("pageInfo", (p, c) =>
            MapCursor(c, Some(p))
              .withField("hasPreviousPage", offset > 0)
              .withField("hasNextPage", cs.length > limit)
              .withField("startCursor", offsetToCursor(if (cs.isEmpty) offset - 1 else offset))
              .withField("endCursor", offsetToCursor(offset + cs.length - 2))
          )
          .withListField("edges", (p, c) =>
            // Our `SqlCursor`s show up here, as a list of edges
            sqlCs.zip(offset to offset + limit - 1).map { case (sqlCursor, index) =>
              MapCursor(c, Some(p))
                .withField("cursor", offsetToCursor(index))
                .withField("node", (p, c) => sqlCursor.copy(parent = Some(p), context = c))
            }
          )

      )
    } .value

  }

}

object SqlConnectionMapping {

  private val CursorPattern = """#<(\d+)>""".r

  private def offsetToCursor(n: Int): String =
    s"#<$n>"

  private def cursorToOffset(s: String): Result[Int] =
    Result.fromOption(CursorPattern.findFirstMatchIn(s).map(_.group(1).toInt), s"Malformed cursor: $s")

  private def findNodeQuery(query: Query): Option[Query] =
    query match {
      case Select("node", _, child) => Some(child)
      case Select(_, _, child)      => findNodeQuery(child)
      case Group(queries)           => queries.collectFirst(Function.unlift(findNodeQuery))
      case GroupList(queries)       => queries.collectFirst(Function.unlift(findNodeQuery))
      case Unique(child)            => findNodeQuery(child)
      case Filter(_, child)         => findNodeQuery(child)
      case Component(_, _, child)   => findNodeQuery(child)
      case Introspect(_, child)     => findNodeQuery(child)
      case Defer(_, child, _)       => findNodeQuery(child)
      case Environment(_, child)    => findNodeQuery(child)
      case Wrap(_, child)           => findNodeQuery(child)
      case Rename(_, child)         => findNodeQuery(child)
      case UntypedNarrow(_, child)  => findNodeQuery(child)
      case Narrow(_, child)         => findNodeQuery(child)
      case Skip(_, _, child)        => findNodeQuery(child)
      case Offset(_, child)         => findNodeQuery(child)
      case Limit(_, child)          => findNodeQuery(child)
      case OrderBy(_, child)        => findNodeQuery(child)
      case Count(_, child)          => findNodeQuery(child)
      case Skipped                  => None
      case Empty                    => None
    }

  private def updateNodeQuery(query: Query)(modify: Query => Result[Query]): Result[Query] = {
    def go(q: Query): Result[Query] =
      q match {
        case sel @ Select("node", _, _)     => modify(sel)
        case Select(name, args, child)       => go(child).map(Select(name, args, _))
        case Group(queries)                  => queries.traverse(go).map(Group(_))
        case GroupList(queries)              => queries.traverse(go).map(GroupList(_))
        case Unique(child)                   => go(child).map(Unique(_))
        case Filter(pred, child)             => go(child).map(Filter(pred, _))
        case Component(mapping, join, child) => go(child).map(Component(mapping, join, _))
        case Introspect(schema, child)       => go(child).map(Introspect(schema, _))
        case Defer(join, child, rootTpe)     => go(child).map(Defer(join, _, rootTpe))
        case Environment(env, child)         => go(child).map(Environment(env, _))
        case Wrap(name, child)               => go(child).map(Wrap(name, _))
        case Rename(name, child)             => go(child).map(Rename(name, _))
        case UntypedNarrow(tpnme, child)     => go(child).map(UntypedNarrow(tpnme, _))
        case Narrow(subtpe, child)           => go(child).map(Narrow(subtpe, _))
        case Skip(sense, cond, child)        => go(child).map(Skip(sense, cond, _))
        case Limit(num, child)               => go(child).map(Limit(num, _))
        case Offset(num, child)              => go(child).map(Offset(num, _))
        case OrderBy(selections, child)      => go(child).map(OrderBy(selections, _))
        case Count(name, child)              => go(child).map(Count(name, _))
        case Skipped                         => Result(Skipped)
        case Empty                           => Result(Empty)
      }
    go(query)
  }

  /**
   * Update the `Select("node", Nil, child)` node buried in `query`, if any, adding `first` and
   * `after` to `query`'s environment, for use when constructing the cursor.
   */
  def updateNodeQuery(query: Query, first: Int, after: Option[String])(modify: Query => Result[Query]): Result[Query] = {
    val env = after.foldLeft(Cursor.Env("first" -> first))((e, cursor) => e.add("after" -> cursor))
    updateNodeQuery(query)(modify).map(Query.Environment(env, _))
  }

}