// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2025 Grackle Contributors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package grackle.sql.test


import java.time.{Duration, LocalDate, LocalTime, OffsetDateTime}
import java.util.UUID

import scala.util.Try

import cats.{Eq, Order}
import cats.implicits._
import io.circe.{Decoder, Encoder}

import grackle._
import syntax._
import Query._
import Predicate._
import Value._
import QueryCompiler._

trait SqlMovieMapping[F[_]] extends SqlTestMapping[F] { self =>

  def genre: TestCodec[Genre]
  def feature: TestCodec[Feature]
  def tagList: TestCodec[List[String]]

  object movies extends TableDef("movies") {
    val id = col("id", uuid)
    val title = col("title", text)
    val genre = col("genre", self.genre)
    val releaseDate = col("releasedate", localDate)
    val showTime = col("showtime", localTime)
    val nextShowing = col("nextshowing", offsetDateTime)
    val duration = col("duration", self.duration)
    val categories = col("categories", list(varchar))
    val features = col("features", list(feature))
    val tags = col("tags", tagList)
  }

  val schema =
    schema"""
        type Query {
          movieById(id: UUID!): Movie
          moviesByGenre(genre: Genre!): [Movie!]!
          moviesByGenres(genres: [Genre!]): [Movie!]!
          moviesReleasedBetween(from: Date!, to: Date!): [Movie!]!
          moviesLongerThan(duration: Interval!): [Movie!]!
          moviesShownLaterThan(time: Time!): [Movie!]!
          moviesShownBetween(from: DateTime!, to: DateTime!): [Movie!]!
          longMovies: [Movie!]!
          allMovies: [Movie!]!
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
          tags: [String!]!
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

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            SqlObject("movieById"),
            SqlObject("moviesByGenre"),
            SqlObject("moviesByGenres"),
            SqlObject("moviesReleasedBetween"),
            SqlObject("moviesLongerThan"),
            SqlObject("moviesShownLaterThan"),
            SqlObject("moviesShownBetween"),
            SqlObject("longMovies"),
            SqlObject("allMovies")
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
            SqlField("categories", movies.categories),
            SqlField("features", movies.features),
            CursorField("isLong", isLong, List("duration"), hidden = true),
            SqlField("tags", movies.tags)
          )
      ),
      LeafMapping[UUID](UUIDType),
      LeafMapping[LocalTime](TimeType),
      LeafMapping[LocalDate](DateType),
      LeafMapping[OffsetDateTime](DateTimeType),
      LeafMapping[Duration](IntervalType),
      LeafMapping[Genre](GenreType),
      LeafMapping[Feature](FeatureType)
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
    def unapply(e: EnumValue): Option[Genre] =
      Genre.fromString(e.name)
  }

  object GenreListValue {
    def unapply(gs: ListValue): Option[List[Genre]] =
      gs.elems.traverse {
        case GenreValue(g) => Some(g)
        case _ => None
      }
  }

  override val selectElaborator = SelectElaborator {
    case (QueryType, "movieById", List(Binding("id", UUIDValue(id)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(MovieType / "id", Const(id)), child)))
    case (QueryType, "moviesByGenre", List(Binding("genre", GenreValue(genre)))) =>
      Elab.transformChild(child => Filter(Eql(MovieType / "genre", Const(genre)), child))
    case (QueryType, "moviesByGenres", List(Binding("genres", GenreListValue(genres)))) =>
      Elab.transformChild(child => Filter(In(MovieType / "genre", genres), child))
    case (QueryType, "moviesReleasedBetween", List(Binding("from", DateValue(from)), Binding("to", DateValue(to)))) =>
      Elab.transformChild(child =>
        Filter(
          And(
            Not(Lt(MovieType / "releaseDate", Const(from))),
            Lt(MovieType / "releaseDate", Const(to))
          ),
          child
        )
      )
    case (QueryType, "moviesLongerThan", List(Binding("duration", IntervalValue(duration)))) =>
      Elab.transformChild(child =>
        Filter(
          Not(Lt(MovieType / "duration", Const(duration))),
          child
        )
      )
    case (QueryType, "moviesShownLaterThan", List(Binding("time", TimeValue(time)))) =>
      Elab.transformChild(child =>
        Filter(
          Not(Lt(MovieType / "showTime", Const(time))),
          child
        )
      )
    case (QueryType, "moviesShownBetween", List(Binding("from", DateTimeValue(from)), Binding("to", DateTimeValue(to)))) =>
      Elab.transformChild(child =>
        Filter(
          And(
            Not(Lt(MovieType / "nextShowing", Const(from))),
            Lt(MovieType / "nextShowing", Const(to))
          ),
          child
        )
      )
    case (QueryType, "longMovies", Nil) =>
      Elab.transformChild(child => Filter(Eql(MovieType / "isLong", Const(true)), child))
  }

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

    implicit val genreEncoder: io.circe.Encoder[Genre] =
      Encoder[String].contramap(_ match {
        case Drama => "DRAMA"
        case Action => "ACTION"
        case Comedy => "COMEDY"
      })

    def fromInt(i: Int): Genre =
      (i: @unchecked) match {
        case 1 => Drama
        case 2 => Action
        case 3 => Comedy
      }

    def toInt(f: Genre): Int =
      f match {
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

    implicit def featureEncoder: io.circe.Encoder[Feature] =
      Encoder[String].contramap(_.toString)

    implicit def featureDecoder: io.circe.Decoder[Feature] =
      Decoder[String].map(fromString)
  }

  object Tags {
    val tags = List("tag1", "tag2", "tag3")


    def fromInt(i: Int): List[String] = {
      def getTag(m: Int): List[String] =
        if((i&(1 << m)) != 0) List(tags(m)) else Nil
      (0 to 2).flatMap(getTag).toList
    }

    def toInt(tags: List[String]): Int = {
      def getBit(m: Int): Int =
        if(tags.contains(tags(m))) 1 << m else 0
      (0 to 2).foldLeft(0)((acc, m) => acc | getBit(m))
    }
  }

  implicit val localDateOrder: Order[LocalDate] =
    Order.from(_.compareTo(_))

  implicit val localTimeOrder: Order[LocalTime] =
    Order.fromComparable[LocalTime]

  implicit val offsetDateTimeOrder: Order[OffsetDateTime] =
    Order.from(_.compareTo(_))

  implicit val durationOrder: Order[Duration] =
    Order.fromComparable[Duration]
}
