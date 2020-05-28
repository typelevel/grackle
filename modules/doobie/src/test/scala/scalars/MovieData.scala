// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package scalars

import java.time.{Duration, Instant, LocalDate, LocalTime, ZonedDateTime, ZoneOffset}
import java.util.UUID
import scala.util.Try

import cats.{Eq, Order}
import cats.effect.Bracket
import cats.implicits._
import doobie.Transactor
import doobie.implicits.javatime.JavaLocalTimeMeta
import doobie.implicits.legacy.localdate._
import doobie.implicits.legacy.instant._
import doobie.postgres.implicits.UuidType
import doobie.util.meta.Meta
import io.chrisdavenport.log4cats.Logger
import io.circe.Encoder

import edu.gemini.grackle._, doobie._
import Query._, Predicate._, Value._
import QueryCompiler._
import DoobieMapping._, FieldMapping._

object MovieData extends DoobieMapping {
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
        type Movie {
          id: UUID!
          title: String!
          genre: Genre!
          releaseDate: Date!
          showTime: Time!
          nextShowing: DateTime!
          duration: Interval!
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

  val movieMapping =
    ObjectMapping(
      tpe = "Movie",
      key = List(ColumnRef("movies", "id")),
      fieldMappings =
        List(
          "id" -> ColumnRef("movies", "id"),
          "title" -> ColumnRef("movies", "title"),
          "genre" -> ColumnRef("movies", "genre"),
          "releaseDate" -> ColumnRef("movies", "releasedate"),
          "showTime" -> ColumnRef("movies", "showtime"),
          "nextShowing" -> ColumnRef("movies", "nextshowing"),
          "duration" -> ColumnRef("movies", "duration")
        ),
      Nil
    )

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

  implicit val durationMeta: Meta[Duration] =
    Meta[Long].timap(Duration.ofMillis)(_.toMillis)

  implicit val zonedDateTimeMeta: Meta[ZonedDateTime] =
    Meta[Instant].timap(i => ZonedDateTime.ofInstant(i, ZoneOffset.UTC))(_.toInstant)

  implicit val localDateOrder: Order[LocalDate] =
    Order.from(_.compareTo(_))

  implicit val localTimeOrder: Order[LocalTime] =
    Order.fromComparable[LocalTime]

  implicit val zonedDateTimeOrder: Order[ZonedDateTime] =
    Order.from(_.compareTo(_))

  implicit val durationOrder: Order[Duration] =
    Order.fromComparable[Duration]

  val uuidMapping = LeafMapping[UUID](UUIDType)
  val timeMapping = LeafMapping[LocalTime](TimeType)
  val dateMapping = LeafMapping[LocalDate](DateType)
  val dateTimeMapping = LeafMapping[ZonedDateTime](DateTimeType)
  val intervalMapping = LeafMapping[Duration](IntervalType)
  val genreMapping = LeafMapping[Genre](GenreType)

  val objectMappings = List(movieMapping)
  val leafMappings = List(uuidMapping, timeMapping, dateMapping, dateTimeMapping, intervalMapping, genreMapping)
}

import MovieData._

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

object MovieQueryCompiler extends QueryCompiler(MovieData.schema) {
  val QueryType = MovieData.schema.ref("Query")

  val selectElaborator = new SelectElaborator(Map(
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
    }
  ))

  val stagingElaborator = new StagingElaborator(MovieData)

  val phases = List(selectElaborator, stagingElaborator)
}

object MovieQueryInterpreter {
  def fromTransactor[F[_]](xa: Transactor[F])
    (implicit brkt: Bracket[F, Throwable], logger: Logger[F]): DoobieQueryInterpreter[F] =
      new DoobieQueryInterpreter[F](MovieData, xa, logger)
}
