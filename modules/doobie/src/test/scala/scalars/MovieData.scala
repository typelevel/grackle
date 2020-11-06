// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package scalars

import java.time.{Duration, Instant, LocalDate, LocalTime, ZonedDateTime, ZoneOffset}
import java.util.UUID
import scala.util.Try

import cats.{Eq, Order}
import cats.effect.Sync
import cats.implicits._
import doobie.Transactor
import doobie.implicits.javatime.JavaLocalTimeMeta
import doobie.implicits.legacy.localdate._
import doobie.implicits.legacy.instant._
import doobie.postgres.implicits.UuidType
import doobie.util.meta.Meta
import io.circe.Encoder

import edu.gemini.grackle._, doobie._
import Query._, Predicate._, Value._
import QueryCompiler._

object MovieData {
  sealed trait Genre extends Product with Serializable
  object Genre {
    case object Drama extends Genre
    case object Action extends Genre
    case object Comedy extends Genre

    implicit val genreEq: Eq[Genre] = Eq.fromUniversalEquals[Genre]

    def fromString(s: String): Option[Genre] =
      s.trim.toUpperCase match {
        case "DRAMA"  => Some(Drama)
        case "ACTION" => Some(Action)
        case "COMEDY" => Some(Comedy)
        case _ => None
      }

    implicit val genreEncoder: Encoder[Genre] =
      Encoder[String].contramap(_ match {
        case Drama => "DRAMA"
        case Action => "ACTION"
        case Comedy => "COMEDY"
      })

    implicit val genreMeta: Meta[Genre] =
      Meta[Int].timap {
        case 1 => Drama
        case 2 => Action
        case 3 => Comedy
      } {
        case Drama  => 1
        case Action => 2
        case Comedy => 3
      }
  }

  sealed trait Feature
  object Feature {
    case object HD extends Feature
    case object HLS extends Feature

    def fromString(s: String): Feature = (s.trim.toUpperCase: @unchecked) match {
      case "HD" => HD
      case "HLS" => HLS
    }

    implicit def featureEncoder: Encoder[Feature] =
      Encoder[String].contramap(_.toString)
  }

  implicit val durationMeta: Meta[Duration] =
    Meta[Long].timap(Duration.ofMillis)(_.toMillis)

  implicit val zonedDateTimeMeta: Meta[ZonedDateTime] =
    Meta[Instant].timap(i => ZonedDateTime.ofInstant(i, ZoneOffset.UTC))(_.toInstant)

  implicit val featureListMeta: Meta[List[Feature]] =
    Meta.Advanced.array[String]("VARCHAR", "_VARCHAR").imap(_.toList.map(Feature.fromString))(_.map(_.toString).toArray)

  implicit val localDateOrder: Order[LocalDate] =
    Order.from(_.compareTo(_))

  implicit val localTimeOrder: Order[LocalTime] =
    Order.fromComparable[LocalTime]

  implicit val zonedDateTimeOrder: Order[ZonedDateTime] =
    Order.from(_.compareTo(_))

  implicit val durationOrder: Order[Duration] =
    Order.fromComparable[Duration]
}

trait MovieMapping[F[_]] extends DoobieMapping[F] {
  import MovieData._

  val schema =
    Schema(
      """
        type Query {
          movieById(id: UUID!): Movie
          moviesByGenre(genre: Genre!): [Movie!]!
          moviesReleasedBetween(from: Date!, to: Date!): [Movie!]!
          moviesLongerThan(duration: Interval!): [Movie!]!
          moviesShownLaterThan(time: Time!): [Movie!]!
          moviesShownBetween(from: DateTime!, to: DateTime!): [Movie!]!
          longMovies: [Movie!]!
        }
        scalar UUID
        scalar Time
        scalar Date
        scalar DateTime
        scalar Interval
        enum Genre {
          DRAMA
          ACTION
          COMEDY
        }
        enum Feature {
          HD
          HLS
        }
        type Movie {
          id: UUID!
          title: String!
          genre: Genre!
          releaseDate: Date!
          showTime: Time!
          nextShowing: DateTime!
          nextEnding: DateTime!
          duration: Interval!
          categories: [String!]!
          features: [Feature!]!
        }
      """
    ).right.get

  val QueryType = schema.ref("Query")
  val MovieType = schema.ref("Movie")
  val UUIDType = schema.ref("UUID")
  val TimeType = schema.ref("Time")
  val DateType = schema.ref("Date")
  val DateTimeType = schema.ref("DateTime")
  val IntervalType = schema.ref("Interval")
  val GenreType = schema.ref("Genre")
  val FeatureType = schema.ref("Feature")
  val RatingType = schema.ref("Rating")

  import DoobieFieldMapping._

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            DoobieRoot("movieById"),
            DoobieRoot("moviesByGenre"),
            DoobieRoot("moviesReleasedBetween"),
            DoobieRoot("moviesLongerThan"),
            DoobieRoot("moviesShownLaterThan"),
            DoobieRoot("moviesShownBetween"),
            DoobieRoot("longMovies")
          )
      ),
      ObjectMapping(
        tpe = MovieType,
        fieldMappings =
          List(
            DoobieField("id", ColumnRef("movies", "id"), key = true),
            DoobieField("title", ColumnRef("movies", "title")),
            DoobieField("genre", ColumnRef("movies", "genre")),
            DoobieField("releaseDate", ColumnRef("movies", "releasedate")),
            DoobieField("showTime", ColumnRef("movies", "showtime")),
            DoobieField("nextShowing", ColumnRef("movies", "nextshowing")),
            CursorField("nextEnding", nextEnding, List("nextShowing", "duration")),
            DoobieField("duration", ColumnRef("movies", "duration")),
            DoobieField("categories", ColumnRef("movies", "categories")),
            DoobieField("features", ColumnRef("movies", "features")),
            DoobieField("rating", ColumnRef("movies", "rating")),
            CursorAttribute("isLong", isLong, List("duration"))
          )
      ),
      DoobieLeafMapping[UUID](UUIDType),
      DoobieLeafMapping[LocalTime](TimeType),
      DoobieLeafMapping[LocalDate](DateType),
      DoobieLeafMapping[ZonedDateTime](DateTimeType),
      DoobieLeafMapping[Duration](IntervalType),
      DoobieLeafMapping[Genre](GenreType),
      LeafMapping[Feature](FeatureType),
      DoobieLeafMapping[List[Feature]](ListType(FeatureType))
    )

  def nextEnding(c: Cursor): Result[ZonedDateTime] =
    for {
      nextShowing <- c.fieldAs[ZonedDateTime]("nextShowing")
      duration    <- c.fieldAs[Duration]("duration")
    } yield nextShowing.plus(duration)

  def isLong(c: Cursor): Result[Boolean] =
    for {
      duration    <- c.fieldAs[Duration]("duration")
    } yield duration.toHours >= 3

  object UUIDValue {
    def unapply(s: StringValue): Option[UUID] =
      Try(UUID.fromString(s.value)).toOption
  }

  object DateValue {
    def unapply(s: StringValue): Option[LocalDate] =
      Try(LocalDate.parse(s.value)).toOption
  }

  object TimeValue {
    def unapply(s: StringValue): Option[LocalTime] =
      Try(LocalTime.parse(s.value)).toOption
  }

  object DateTimeValue {
    def unapply(s: StringValue): Option[ZonedDateTime] =
      Try(ZonedDateTime.parse(s.value)).toOption
  }

  object IntervalValue {
    def unapply(s: StringValue): Option[Duration] =
      Try(Duration.parse(s.value)).toOption
  }

  object GenreValue {
    def unapply(e: TypedEnumValue): Option[Genre] =
      Genre.fromString(e.value.name)
  }

  override val selectElaborator = new SelectElaborator(Map(
    QueryType -> {
      case Select("movieById", List(Binding("id", UUIDValue(id))), child) =>
        Select("movieById", Nil, Unique(Eql(FieldPath(List("id")), Const(id)), child)).rightIor
      case Select("moviesByGenre", List(Binding("genre", GenreValue(genre))), child) =>
        Select("moviesByGenre", Nil, Filter(Eql(FieldPath(List("genre")), Const(genre)), child)).rightIor
      case Select("moviesReleasedBetween", List(Binding("from", DateValue(from)), Binding("to", DateValue(to))), child) =>
        Select("moviesReleasedBetween", Nil,
          Filter(
            And(
              Not(Lt(FieldPath(List("releaseDate")), Const(from))),
              Lt(FieldPath(List("releaseDate")), Const(to))
            ),
            child
          )
        ).rightIor
      case Select("moviesLongerThan", List(Binding("duration", IntervalValue(duration))), child) =>
        Select("moviesLongerThan", Nil,
          Filter(
            Not(Lt(FieldPath(List("duration")), Const(duration))),
            child
          )
        ).rightIor
      case Select("moviesShownLaterThan", List(Binding("time", TimeValue(time))), child) =>
        Select("moviesShownLaterThan", Nil,
          Filter(
            Not(Lt(FieldPath(List("showTime")), Const(time))),
            child
          )
        ).rightIor
      case Select("moviesShownBetween", List(Binding("from", DateTimeValue(from)), Binding("to", DateTimeValue(to))), child) =>
        Select("moviesShownBetween", Nil,
          Filter(
            And(
              Not(Lt(FieldPath(List("nextShowing")), Const(from))),
              Lt(FieldPath(List("nextShowing")), Const(to))
            ),
            child
          )
        ).rightIor
      case Select("longMovies", Nil, child) =>
        Select("longMovies", Nil, Filter(Eql(AttrPath(List("isLong")), Const(true)), child)).rightIor
    }
  ))
}

object MovieMapping extends DoobieMappingCompanion {
  def mkMapping[F[_] : Sync](transactor: Transactor[F], monitor: DoobieMonitor[F]): MovieMapping[F] =
    new DoobieMapping(transactor, monitor) with MovieMapping[F]
}
