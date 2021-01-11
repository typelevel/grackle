// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package validate

import java.time.Duration
import java.time.LocalDate
import java.time.LocalTime

import cats.effect.IO
import edu.gemini.grackle._
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.skunk.SkunkMonitor
import org.scalatest.funsuite.AnyFunSuite
import cats.implicits._
import _root_.skunk.codec.all._
import edu.gemini.grackle.sql.SqlMappingValidator


final class ValidateSpec extends AnyFunSuite {

  val MovieSchema =
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


  abstract class TestMapping extends SkunkMapping[IO](null, SkunkMonitor.noopMonitor) {
    final val schema: Schema = MovieSchema
  }


  // a commenrs

  test("fails") {

    object Mapping1 extends TestMapping {

      val QueryType    = schema.ref("Query")
      val MovieType    = schema.ref("Movie")
      val UUIDType     = schema.ref("UUID")
      val TimeType     = schema.ref("Time")
      val DateType     = schema.ref("Date")
      val DateTimeType = schema.ref("DateTime")
      val IntervalType = schema.ref("Interval")
      val GenreType    = schema.ref("Genre")
      val FeatureType  = schema.ref("Feature")
      val RatingType   = schema.ref("Rating")

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
                SqlField("id", ColumnRef("movies", "id", uuid), key = true),
                SqlField("title", ColumnRef("movies", "title", text)),
                // SqlField("genre", ColumnRef("movies", "genre", Genre.codec)),
                SqlField("releaseDate", ColumnRef("movies", "releasedate", date)),
                SqlField("showTime", ColumnRef("movies", "showtime", time)),
                SqlField("nextShowing", ColumnRef("movies", "nextshowing", timestamptz)),
                // CursorField("nextEnding", nextEnding, List("nextShowing", "duration")),
                SqlField("duration", ColumnRef("movies", "duration", int8.imap(Duration.ofMillis)(_.toMillis))),
                // SqlField("categories", ColumnRef("movies", "categories")),
                // SqlField("features", ColumnRef("movies", "features")),
                // SqlField("rating", ColumnRef("movies", "rating")),
                // CursorAttribute("isLong", isLong, List("duration"))
              )
          ),
          LeafMapping[String](UUIDType), // this is the wrong type
          LeafMapping[LocalTime](TimeType),
          LeafMapping[LocalDate](DateType),
          // LeafMapping[OffsetDateTime](DateTimeType),
          LeafMapping[Duration](QueryType),
          // LeafMapping[Genre](GenreType, Genre.codec),
          // LeafMapping[Feature](FeatureType),
          // LeafMapping[List[Feature]](ListType(FeatureType))


          // a comment

        )

    }

    val v = new SqlMappingValidator(Mapping1)
    v.validateMapping().toList.sortBy(_.severity).foreach { f =>
      println(s"${f.prefix}$f")
    }

    fail("unimplemented")

  }


  List(
    1,
    2,
    // comment
  )

}
