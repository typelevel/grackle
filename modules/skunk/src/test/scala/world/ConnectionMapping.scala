// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package world

import cats.syntax.all._
import edu.gemini.grackle._
import org.tpolecat.sourcepos.SourcePos
import fs2.Stream
import edu.gemini.grackle.Query._
import edu.gemini.grackle.sql.SqlMapping
import cats.data.IorT

trait SqlConnectionMapping[F[_]] extends SqlMapping[F] {

  case class SqlConnection(
    fieldName: String,
    orootTpe: Option[Type] = None,
    mutation: Mutation = Mutation.None
  )(implicit val pos: SourcePos) extends RootMapping {

    def withParent(tpe: Type): RootMapping = copy(orootTpe = Some(tpe))
    def cursor(n: Int): String = s"#<$n>"

    private val findConnType: Result[Type] =
      Result.fromOption(
        orootTpe.flatMap(_.field(fieldName)),
        "SqlConnection: cannot determine the root connection type."
      )

    private val findNodeType: Result[Type] =
      Result.fromOption(
        for {
          root  <- orootTpe
          self  <- root.field(fieldName)
          edges <- self.field("edges")
        } yield edges.underlying,
        "SqlConnection: can't get the edge type."
      )

    private def nodeQuery(query: Query): Result[Query] =
      Result.fromOption(
        SqlConnectionMapping.findNodeQuery(query),
        "SqlConnection: Can't find a `node` selection in the provided query."
      )

    private def narrowToSqlCursor(c: Cursor): Result[SqlCursor] =
      c match {
        case sqlc: SqlCursor => Result(sqlc)
        case other           => Result.failure(s"SqlConnection: Expected SqlCursor, found ${other.getClass.getName}")
      }

    def cursor(query: Query, env: Cursor.Env, resultName: Option[String]): Stream[F,Result[(Query, Cursor)]] = {
      def lift[A](fa: Result[A]) = IorT.fromIor[Stream[F,*]](fa) // F can't be inferred :-\
      for {
        child   <- lift(nodeQuery(query))
        offset   = 3 // TODO: get from env
        limit    = 5
        paged    = Limit(limit + 1, Offset(offset, child))
        connTpe <- lift(findConnType)
        nodeTpe <- lift(findNodeType)
        listC   <- IorT(SqlRoot("node", Some(nodeTpe)).cursor(paged, env, None)).map(_._2)
        cs      <- lift(listC.asList)
        sqlCs   <- lift(cs.traverse(narrowToSqlCursor))
      } yield (
        query,
        MapCursor(Cursor.Context(fieldName, resultName, connTpe), env = env)
          .withField("pageInfo", (p, c) =>
            MapCursor(c, Some(p))
              .withField("hasPreviousPage", offset > 0)
              .withField("hasNextPage", cs.length > limit)
              .withField("startCursor", cursor(if (cs.isEmpty) offset - 1 else offset))
              .withField("endCursor", cursor(offset + cs.length - 2))
          )
          .withListField("edges", (p, c) =>
            sqlCs.zip(offset to offset + limit - 1).map { case (sqlCursor, index) =>
              MapCursor(c, Some(p))
                .withField("cursor", cursor(index))
                .withField("node", (p, c) => sqlCursor.copy(parent = Some(p), context = c))
            }
          )
      )
    } .value

  }

}

object SqlConnectionMapping {

  def findNodeQuery(query: Query): Option[Query] =
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

  def updateNodeQuery(query: Query, modify: Query => Query): Query = {
    def go(q: Query): Query =
      q match {
        case Select("node", args, child)     => Select("node", args, modify(child))
        case Select(name, args, child)       => Select(name, args, go(child))
        case Group(queries)                  => Group(queries.map(go))
        case GroupList(queries)              => GroupList(queries.map(go))
        case Unique(child)                   => Unique(go(child))
        case Filter(pred, child)             => Filter(pred, go(child))
        case Component(mapping, join, child) => Component(mapping, join, go(child))
        case Introspect(schema, child)       => Introspect(schema, go(child))
        case Defer(join, child, rootTpe)     => Defer(join, go(child), rootTpe)
        case Environment(env, child)         => Environment(env, go(child))
        case Wrap(name, child)               => Wrap(name, go(child))
        case Rename(name, child)             => Rename(name, go(child))
        case UntypedNarrow(tpnme, child)     => UntypedNarrow(tpnme, go(child))
        case Narrow(subtpe, child)           => Narrow(subtpe, go(child))
        case Skip(sense, cond, child)        => Skip(sense, cond, go(child))
        case Limit(num, child)               => Limit(num, go(child))
        case Offset(num, child)              => Offset(num, go(child))
        case OrderBy(selections, child)      => OrderBy(selections, go(child))
        case Count(name, child)              => Count(name, go(child))
        case Skipped                         => Skipped
        case Empty                           => Empty
      }
    go(query)
  }

}