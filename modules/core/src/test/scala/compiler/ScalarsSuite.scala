// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
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

package compiler

import java.time.{Duration, LocalDate, LocalTime, ZonedDateTime}
import java.util.UUID
import scala.util.Try

import cats.{Eq, Order}
import cats.effect.IO
import cats.implicits._
import io.circe.literal._
import munit.CatsEffectSuite

import grackle._
import grackle.syntax._
import Query._
import Predicate._, Value._
import QueryCompiler._

import io.circe.Encoder

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
  }

  implicit val localDateOrder: Order[LocalDate] =
    Order.from(_.compareTo(_))

  implicit val localTimeOrder: Order[LocalTime] =
    Order.fromComparable[LocalTime]

  implicit val zonedDateTimeOrder: Order[ZonedDateTime] =
    Order.from(_.compareTo(_))

  implicit val durationOrder: Order[Duration] =
    Order.fromComparable[Duration]

  import Genre.{Action, Comedy, Drama}

  case class Movie(
    id: UUID,
    title: String,
    genre: Genre,
    releaseDate: LocalDate,
    showTime: LocalTime,
    nextShowing: ZonedDateTime,
    duration: Duration
  )

  val movies =
    List(
      Movie(UUID.fromString("6a7837fc-b463-4d32-b628-0f4b3065cb21"), "Celine et Julie Vont en Bateau", Drama, LocalDate.parse("1974-10-07"), LocalTime.parse("19:35:00"), ZonedDateTime.parse("2020-05-22T19:35:00Z"), Duration.ofMillis(12300000)),
      Movie(UUID.fromString("11daf8c0-11c3-4453-bfe1-cb6e6e2f9115"), "Duelle", Drama, LocalDate.parse("1975-09-15"), LocalTime.parse("19:20:00"), ZonedDateTime.parse("2020-05-27T19:20:00Z"), Duration.ofMillis(7260000)),
      Movie(UUID.fromString("aea9756f-621b-42d5-b130-71f3916c4ba3"), "L'Amour fou", Drama, LocalDate.parse("1969-01-15"), LocalTime.parse("21:00:00"), ZonedDateTime.parse("2020-05-27T21:00:00Z"), Duration.ofMillis(15120000)),
      Movie(UUID.fromString("2ddb041f-86c2-4bd3-848c-990a3862634e"), "Last Year at Marienbad", Drama, LocalDate.parse("1961-06-25"), LocalTime.parse("20:30:00"), ZonedDateTime.parse("2020-05-26T20:30:00Z"), Duration.ofMillis(5640000)),
      Movie(UUID.fromString("8ae5b13b-044c-4ff0-8b71-ccdb7d77cd88"), "Zazie dans le Métro", Comedy, LocalDate.parse("1960-10-28"), LocalTime.parse("20:15:00"), ZonedDateTime.parse("2020-05-25T20:15:00Z"), Duration.ofMillis(5340000)),
      Movie(UUID.fromString("9dce9deb-9188-4cc2-9685-9842b8abdd34"), "Alphaville", Action, LocalDate.parse("1965-05-05"), LocalTime.parse("19:45:00"), ZonedDateTime.parse("2020-05-19T19:45:00Z"), Duration.ofMillis(5940000)),
      Movie(UUID.fromString("1bf00ac6-91ab-4e51-b686-3fd5e2324077"), "Stalker", Drama, LocalDate.parse("1979-05-13"), LocalTime.parse("15:30:00"), ZonedDateTime.parse("2020-05-19T15:30:00Z"), Duration.ofMillis(9660000)),
      Movie(UUID.fromString("6a878e06-6563-4a0c-acd9-d28dcfb2e91a"), "Weekend", Comedy, LocalDate.parse("1967-12-29"), LocalTime.parse("22:30:00"), ZonedDateTime.parse("2020-05-19T22:30:00Z"), Duration.ofMillis(6300000)),
      Movie(UUID.fromString("2a40415c-ea6a-413f-bbef-a80ae280c4ff"), "Daisies", Comedy, LocalDate.parse("1966-12-30"), LocalTime.parse("21:30:00"), ZonedDateTime.parse("2020-05-15T21:30:00Z"), Duration.ofMillis(4560000)),
      Movie(UUID.fromString("2f6dcb0a-4122-4a21-a1c6-534744dd6b85"), "Le Pont du Nord", Drama, LocalDate.parse("1982-01-13"), LocalTime.parse("20:45:00"), ZonedDateTime.parse("2020-05-11T20:45:00Z"), Duration.ofMillis(7620000))
    ).sortBy(_.id.toString)
}

object MovieMapping extends ValueMapping[IO] {
  import MovieData._

  val schema =
    schema"""
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

  val QueryType = schema.ref("Query")
  val MovieType = schema.ref("Movie")
  val UUIDType = schema.ref("UUID")
  val GenreType = schema.ref("Genre")
  val TimeType = schema.ref("Time")
  val DateType = schema.ref("Date")
  val DateTimeType = schema.ref("DateTime")
  val IntervalType = schema.ref("Interval")

  val typeMappings =
    List(
      ValueObjectMapping[Unit](
        tpe = QueryType,
        fieldMappings =
          List(
            ValueField("movieById", _ => movies),
            ValueField("moviesByGenre", _ => movies),
            ValueField("moviesReleasedBetween", _ => movies),
            ValueField("moviesLongerThan", _ => movies),
            ValueField("moviesShownLaterThan", _ => movies),
            ValueField("moviesShownBetween", _ => movies)
          )
      ),
      ValueObjectMapping[Movie](
        tpe = MovieType,
        fieldMappings =
          List(
            ValueField("id", _.id),
            ValueField("title", _.title),
            ValueField("genre", _.genre),
            ValueField("releaseDate", _.releaseDate),
            ValueField("showTime", _.showTime),
            ValueField("nextShowing", _.nextShowing),
            ValueField("duration", _.duration)
          )
      ),
      LeafMapping[UUID](UUIDType),
      LeafMapping[Genre](GenreType),
      LeafMapping[LocalDate](DateType),
      LeafMapping[LocalTime](TimeType),
      LeafMapping[ZonedDateTime](DateTimeType),
      LeafMapping[Duration](IntervalType)
    )

  object UUIDValue {
    def unapply(s: StringValue): Option[UUID] =
      Try(UUID.fromString(s.value)).toOption
  }

  object GenreValue {
    def unapply(e: EnumValue): Option[Genre] =
      Genre.fromString(e.name)
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

  override val selectElaborator = SelectElaborator {
    case (QueryType, "movieById", List(Binding("id", UUIDValue(id)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(MovieType / "id", Const(id)), child)))
    case (QueryType, "moviesByGenre", List(Binding("genre", GenreValue(genre)))) =>
      Elab.transformChild(child => Filter(Eql(MovieType / "genre", Const(genre)), child))
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
  }
}

final class ScalarsSuite extends CatsEffectSuite {
  test("query with UUID argument and custom scalar results") {
    val query = """
      query {
        movieById(id: "6a7837fc-b463-4d32-b628-0f4b3065cb21") {
          id
          title
          genre
          releaseDate
          showTime
          nextShowing
          duration
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "movieById" : {
            "id" : "6a7837fc-b463-4d32-b628-0f4b3065cb21",
            "title" : "Celine et Julie Vont en Bateau",
            "genre" : "DRAMA",
            "releaseDate" : "1974-10-07",
            "showTime" : "19:35:00",
            "nextShowing" : "2020-05-22T19:35:00Z",
            "duration" : "PT3H25M"
          }
        }
      }
    """

    val res = MovieMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("query with mapped enum argument") {
    val query = """
      query {
        moviesByGenre(genre: COMEDY) {
          title
          genre
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "moviesByGenre" : [
            {
              "title" : "Daisies",
              "genre" : "COMEDY"
            },
            {
              "title" : "Weekend",
              "genre" : "COMEDY"
            },
            {
              "title" : "Zazie dans le Métro",
              "genre" : "COMEDY"
            }
          ]
        }
      }
    """

    val res = MovieMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("query with LocalDate argument") {
    val query = """
      query {
        moviesReleasedBetween(from: "1970-01-01", to: "1980-01-01") {
          title
          releaseDate
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "moviesReleasedBetween" : [
            {
              "title" : "Duelle",
              "releaseDate" : "1975-09-15"
            },
            {
              "title" : "Stalker",
              "releaseDate" : "1979-05-13"
            },
            {
              "title" : "Celine et Julie Vont en Bateau",
              "releaseDate" : "1974-10-07"
            }
          ]
        }
      }
    """

    val res = MovieMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("query with Duration argument") {
    val query = """
      query {
        moviesLongerThan(duration: "PT3H") {
          title
          duration
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "moviesLongerThan" : [
            {
              "title" : "Celine et Julie Vont en Bateau",
              "duration" : "PT3H25M"
            },
            {
              "title" : "L'Amour fou",
              "duration" : "PT4H12M"
            }
          ]
        }
      }
    """

    val res = MovieMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("query with scalar argument without apostrophes") {
    val query = """
      query {
        moviesLongerThan(duration: PT3H) {
          title
          duration
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "moviesLongerThan" : [
            {
              "title" : "Celine et Julie Vont en Bateau",
              "duration" : "PT3H25M"
            },
            {
              "title" : "L'Amour fou",
              "duration" : "PT4H12M"
            }
          ]
        }
      }
    """

    val res = MovieMapping.compileAndRun(query)

    assertIO(res, expected)
  }


  test("query with LocalTime argument") {
    val query = """
      query {
        moviesShownLaterThan(time: "21:00:00") {
          title
          showTime
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "moviesShownLaterThan" : [
            {
              "title" : "Daisies",
              "showTime" : "21:30:00"
            },
            {
              "title" : "Weekend",
              "showTime" : "22:30:00"
            },
            {
              "title" : "L'Amour fou",
              "showTime" : "21:00:00"
            }
          ]
        }
      }
    """

    val res = MovieMapping.compileAndRun(query)

    assertIO(res, expected)
  }

  test("query with ZonedDateTime argument") {
    val query = """
      query {
        moviesShownBetween(from: "2020-05-01T10:30:00Z", to: "2020-05-19T18:00:00Z") {
          title
          nextShowing
        }
      }
    """

    val expected = json"""
      {
        "data" : {
          "moviesShownBetween" : [
            {
              "title" : "Stalker",
              "nextShowing" : "2020-05-19T15:30:00Z"
            },
            {
              "title" : "Daisies",
              "nextShowing" : "2020-05-15T21:30:00Z"
            },
            {
              "title" : "Le Pont du Nord",
              "nextShowing" : "2020-05-11T20:45:00Z"
            }
          ]
        }
      }
    """

    val res = MovieMapping.compileAndRun(query)

    assertIO(res, expected)
  }
}
