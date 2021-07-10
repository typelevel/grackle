// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package scalars

import java.time.{Duration, LocalDate, LocalTime, OffsetDateTime}
import java.util.UUID

import scala.util.Try

import cats.{Eq, Order}
import cats.effect._
import cats.syntax.all._
import io.circe.Encoder
import skunk.{Codec, Session}
import skunk.codec.all._

import edu.gemini.grackle._, skunk._
import edu.gemini.grackle.syntax._
import Query._, Path._, Predicate._, Value._
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

    // N.B. would be nice to have `eimap` with A => B and B => Either[String, A]
    val codec: Codec[Genre] =
      Codec.simple(
        {
          case Drama  => "1"
          case Action => "2"
          case Comedy => "3"
        }, {
          case "1" => Drama.asRight
          case "2" => Action.asRight
          case "3" => Comedy.asRight
          case n    => s"No such Genre: $n".asLeft
        },
        _root_.skunk.data.Type.int4
      )

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

  // implicit val durationMeta: Meta[Duration] =
  //   Meta[Long].timap(Duration.ofMillis)(_.toMillis)

  // implicit val zonedDateTimeMeta: Meta[OffsetDateTime] =
  //   Meta[Instant].timap(i => OffsetDateTime.ofInstant(i, ZoneOffset.UTC))(_.toInstant)

  // implicit val featureListMeta: Meta[List[Feature]] =
  //   Meta.Advanced.array[String]("VARCHAR", "_VARCHAR").imap(_.toList.map(Feature.fromString))(_.map(_.toString).toArray)

  implicit val localDateOrder: Order[LocalDate] =
    Order.from(_.compareTo(_))

  implicit val localTimeOrder: Order[LocalTime] =
    Order.fromComparable[LocalTime]

  implicit val zonedDateTimeOrder: Order[OffsetDateTime] =
    Order.from(_.compareTo(_))

  implicit val durationOrder: Order[Duration] =
    Order.fromComparable[Duration]
}

trait MovieMapping[F[_]] extends SkunkMapping[F] {
  import MovieData._

  object movies extends TableDef("movies") {
    val id = col("id", uuid)
    val title = col("title", text)
    val genre = col("genre", Genre.codec)
    val releaseDate = col("releasedate", date)
    val showTime = col("showtime", time)
    val nextShowing = col("nextshowing", timestamptz)
    val duration = col("duration", int8.imap(Duration.ofMillis)(_.toMillis))
  }

  val schema =
    schema"""
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

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlRoot("movieById"),
            SqlRoot("moviesByGenre"),
            SqlRoot("moviesReleasedBetween"),
            SqlRoot("moviesLongerThan"),
            SqlRoot("moviesShownLaterThan"),
            SqlRoot("moviesShownBetween"),
            SqlRoot("longMovies")
          )
      ),
      ObjectMapping(
        tpe = MovieType,
        fieldMappings =
          List(
            SqlField("id", movies.id, key = true),
            SqlField("title", movies.title),
            SqlField("genre", movies.genre),
            SqlField("releaseDate", movies.releaseDate),
            SqlField("showTime", movies.showTime),
            SqlField("nextShowing", movies.nextShowing),
            CursorField("nextEnding", nextEnding, List("nextShowing", "duration")),
            SqlField("duration", movies.duration),
            CursorField("isLong", isLong, List("duration"))
          )
      ),
      LeafMapping[UUID](UUIDType),
      LeafMapping[LocalTime](TimeType),
      LeafMapping[LocalDate](DateType),
      LeafMapping[OffsetDateTime](DateTimeType),
      LeafMapping[Duration](IntervalType),
      LeafMapping[Genre](GenreType),
      LeafMapping[Feature](FeatureType),
      LeafMapping[List[Feature]](ListType(FeatureType))
    )

  def nextEnding(c: Cursor): Result[OffsetDateTime] =
    for {
      nextShowing <- c.fieldAs[OffsetDateTime]("nextShowing")
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
    def unapply(s: StringValue): Option[OffsetDateTime] =
      Try(OffsetDateTime.parse(s.value)).toOption
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
        Select("movieById", Nil, Unique(Filter(Eql(UniquePath(List("id")), Const(id)), child))).rightIor
      case Select("moviesByGenre", List(Binding("genre", GenreValue(genre))), child) =>
        Select("moviesByGenre", Nil, Filter(Eql(UniquePath(List("genre")), Const(genre)), child)).rightIor
      case Select("moviesReleasedBetween", List(Binding("from", DateValue(from)), Binding("to", DateValue(to))), child) =>
        Select("moviesReleasedBetween", Nil,
          Filter(
            And(
              Not(Lt(UniquePath(List("releaseDate")), Const(from))),
              Lt(UniquePath(List("releaseDate")), Const(to))
            ),
            child
          )
        ).rightIor
      case Select("moviesLongerThan", List(Binding("duration", IntervalValue(duration))), child) =>
        Select("moviesLongerThan", Nil,
          Filter(
            Not(Lt(UniquePath(List("duration")), Const(duration))),
            child
          )
        ).rightIor
      case Select("moviesShownLaterThan", List(Binding("time", TimeValue(time))), child) =>
        Select("moviesShownLaterThan", Nil,
          Filter(
            Not(Lt(UniquePath(List("showTime")), Const(time))),
            child
          )
        ).rightIor
      case Select("moviesShownBetween", List(Binding("from", DateTimeValue(from)), Binding("to", DateTimeValue(to))), child) =>
        Select("moviesShownBetween", Nil,
          Filter(
            And(
              Not(Lt(UniquePath(List("nextShowing")), Const(from))),
              Lt(UniquePath(List("nextShowing")), Const(to))
            ),
            child
          )
        ).rightIor
      case Select("longMovies", Nil, child) =>
        Select("longMovies", Nil, Filter(Eql(UniquePath(List("isLong")), Const(true)), child)).rightIor
    }
  ))
}

object MovieMapping extends SkunkMappingCompanion {

  def mkMapping[F[_]: Sync](pool: Resource[F, Session[F]], monitor: SkunkMonitor[F]): Mapping[F] =
    new SkunkMapping[F](pool, monitor) with MovieMapping[F]

}
