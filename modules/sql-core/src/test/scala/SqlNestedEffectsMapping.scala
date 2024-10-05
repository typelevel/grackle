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

package grackle.sql.test

import cats.effect.{Ref, Sync}
import cats.implicits._
import io.circe.{Encoder, Json}
import io.circe.syntax._
import io.circe.generic.semiauto.deriveEncoder

import grackle._
import sql.Like
import syntax._
import Query._
import Predicate._, Value._
import QueryCompiler._

class CurrencyService[F[_] : Sync](dataRef: Ref[F, CurrencyData], countRef: Ref[F, Int]) {
  implicit val currencyEncoder: Encoder[Currency] = deriveEncoder

  def get(countryCodes: List[String]): F[Json] =
    for {
      _    <- countRef.update(_+1)
      data <- dataRef.get
    } yield {
      val currencies = data.currencies.values.filter(cur => countryCodes.contains(cur.countryCode))
      Json.fromValues(currencies.map(_.asJson))
    }

  def update(code: String, exchangeRate: Double): F[Unit] =
    dataRef.update(data => data.update(code, exchangeRate).getOrElse(data))

  def count: F[Int] = countRef.get
}

object CurrencyService {
  def apply[F[_] : Sync]: F[CurrencyService[F]] = {
    val BRL = Currency("BRL", 0.25, "BR")
    val EUR = Currency("EUR", 1.12, "NL")
    val GBP = Currency("GBP", 1.25, "GB")

    val data = CurrencyData(List(BRL, EUR, GBP).map(c => (c.code, c)).toMap)

    for {
      dataRef  <- Ref[F].of(data)
      countRef <- Ref[F].of(0)
    } yield new CurrencyService[F](dataRef, countRef)
  }
}

trait SqlNestedEffectsMapping[F[_]] extends SqlTestMapping[F] {
  def currencyService: CurrencyService[F]

  object country extends TableDef("country") {
    val code           = col("code", bpchar(3))
    val name           = col("name", text)
    val continent      = col("continent", text)
    val region         = col("region", text)
    val surfacearea    = col("surfacearea", float4)
    val indepyear      = col("indepyear", nullable(int2))
    val population     = col("population", int4)
    val lifeexpectancy = col("lifeexpectancy", nullable(float4))
    val gnp            = col("gnp", nullable(numeric(10, 2)))
    val gnpold         = col("gnpold", nullable(numeric(10, 2)))
    val localname      = col("localname", text)
    val governmentform = col("governmentform", text)
    val headofstate    = col("headofstate", nullable(text))
    val capitalId      = col("capital", nullable(int4))
    val numCities      = col("num_cities", int8)
    val code2          = col("code2", bpchar(2))
  }

  object city extends TableDef("city") {
    val id          = col("id", int4)
    val countrycode = col("countrycode", bpchar(3))
    val name        = col("name", text)
    val district    = col("district", text)
    val population  = col("population", int4)
  }

  object countrylanguage extends TableDef("countrylanguage") {
    val countrycode = col("countrycode", bpchar(3))
    val language = col("language", text)
    val isOfficial = col("isOfficial", bool)
    val percentage = col("percentage", float4)
  }

  val schema =
    schema"""
      type Query {
        cities(namePattern: String = "%"): [City!]
        country(code: String): Country
      }
      type City {
        name: String!
        country: Country!
        district: String!
        population: Int!
      }
      type Language {
        language: String!
        isOfficial: Boolean!
        percentage: Float!
        countries: [Country!]!
      }
      type Country {
        name: String!
        continent: String!
        region: String!
        surfacearea: Float!
        indepyear: Int
        population: Int!
        lifeexpectancy: Float
        gnp: Float
        gnpold: Float
        localname: String!
        governmentform: String!
        headofstate: String
        capitalId: Int
        code2: String!
        cities: [City!]!
        languages: [Language!]!
        currencies: [Currency!]!
      }
      type Currency {
        code: String!
        exchangeRate: Float!
        countryCode: String!
        country: Country!
      }
    """

  val QueryType    = schema.ref("Query")
  val CountryType  = schema.ref("Country")
  val CityType     = schema.ref("City")
  val LanguageType = schema.ref("Language")
  val CurrencyType = schema.ref("Currency")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings = List(
          SqlObject("cities"),
          SqlObject("country")
        )
      ),
      ObjectMapping(
        tpe = CountryType,
        fieldMappings = List(
          SqlField("code",            country.code, key = true, hidden = true),
          SqlField("name",            country.name),
          SqlField("continent",       country.continent),
          SqlField("region",          country.region),
          SqlField("surfacearea",     country.surfacearea),
          SqlField("indepyear",       country.indepyear),
          SqlField("population",      country.population),
          SqlField("lifeexpectancy",  country.lifeexpectancy),
          SqlField("gnp",             country.gnp),
          SqlField("gnpold",          country.gnpold),
          SqlField("localname",       country.localname),
          SqlField("governmentform",  country.governmentform),
          SqlField("headofstate",     country.headofstate),
          SqlField("capitalId",       country.capitalId),
          SqlField("code2",           country.code2),
          SqlObject("cities",         Join(country.code, city.countrycode)),
          SqlObject("languages",      Join(country.code, countrylanguage.countrycode)),
          EffectField("currencies",   CurrencyQueryHandler, List("code2"))
        ),
      ),
      ObjectMapping(
        tpe = CityType,
        fieldMappings = List(
          SqlField("id",              city.id, key = true, hidden = true),
          SqlField("countrycode",     city.countrycode, hidden = true),
          SqlField("name",            city.name),
          SqlField("district",        city.district),
          SqlField("population",      city.population),
          SqlObject("country",        Join(city.countrycode, country.code)),
        )
      ),
      ObjectMapping(
        tpe = LanguageType,
        fieldMappings = List(
          SqlField("language",        countrylanguage.language, key = true, associative = true),
          SqlField("isOfficial",      countrylanguage.isOfficial),
          SqlField("percentage",      countrylanguage.percentage),
          SqlField("countrycode",     countrylanguage.countrycode, hidden = true),
          SqlObject("countries",      Join(countrylanguage.countrycode, country.code))
        )
      ),
      ObjectMapping(
        tpe = CurrencyType,
        fieldMappings = List(
          EffectField("country",      CountryQueryHandler)
        )
      )
    )

  object CurrencyQueryHandler extends EffectHandler[F] {
    def runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]] = {
      val countryCodes = queries.map(_._2.fieldAs[String]("code2").toOption)
      val distinctCodes = queries.flatMap(_._2.fieldAs[String]("code2").toList).distinct

      val children0 = queries.traverse {
        case (query, parentCursor) =>
          Query.childContext(parentCursor.context, query).map(ctx => (ctx, parentCursor))
      }

      def unpackResults(res: Json): List[Json] =
        (for {
          arr <- res.asArray
        } yield
          countryCodes.map {
            case Some(countryCode) =>
              Json.fromValues(arr.find { elem =>
                (for {
                  obj  <- elem.asObject
                  fld  <- obj("countryCode")
                  code <- fld.asString
                } yield countryCode == code).getOrElse(false)
              })
            case _ => Json.Null
        }).getOrElse(Nil)

      (for {
        children <- ResultT(children0.pure[F])
        res      <- ResultT(currencyService.get(distinctCodes).map(_.success))
      } yield {
        unpackResults(res).zip(children).map {
          case (res, (childContext, parentCursor)) =>
            val cursor = CirceCursor(childContext, res, Some(parentCursor), parentCursor.env)
            cursor
        }
      }).value.widen
    }
  }

  object CountryQueryHandler extends EffectHandler[F] {
    val toCode = Map("BR" -> "BRA", "GB" -> "GBR", "NL" -> "NLD")
    def runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]] = {

      def mkListCursor(cursor: Cursor, fieldName: String, resultName: Option[String]): Result[Cursor] =
        for {
          c  <- cursor.field(fieldName, resultName)
          lc <- c.preunique
        } yield lc

      def extractCode(cursor: Cursor): Result[String] =
        cursor.fieldAs[String]("code")

      def partitionCursor(codes: List[String], cursor: Cursor): Result[List[Cursor]] = {
        for {
          cursors <- cursor.asList
          tagged  <- cursors.traverse(c => (extractCode(c).map { code => (code, c) }))
        } yield {
          val m = tagged.toMap
          codes.map(code => m(code))
        }
      }

      runGrouped(queries) {
        case (Select(_, _, child), cursors, indices) =>
          val codes = cursors.flatMap(_.fieldAs[Json]("countryCode").toOption.flatMap(_.asString).toList).map(toCode)
          val combinedQuery = Select("country", None, Filter(In(CountryType / "code", codes), child))

          (for {
            cursor  <- ResultT(sqlCursor(combinedQuery, Env.empty))
            cursor0 <- ResultT(mkListCursor(cursor, "country", None).pure[F])
            pcs     <- ResultT(partitionCursor(codes, cursor0).pure[F])
          } yield {
            pcs.zip(indices)
          }).value.widen

        case _ => Result.internalError("Continuation query has the wrong shape").pure[F].widen
      }
    }

    def runGrouped(ts: List[(Query, Cursor)])(op: (Query, List[Cursor], List[Int]) => F[Result[List[(Cursor, Int)]]]): F[Result[List[Cursor]]] = {
      val groupedAndIndexed = ts.zipWithIndex.groupMap(_._1._1)(ti => (ti._1._2, ti._2)).toList
      val groupedResults =
        groupedAndIndexed.map { case (q, cis) =>
          val (cursors, indices) = cis.unzip
          op(q, cursors, indices)
        }

      groupedResults.sequence.map(_.sequence.map(_.flatten.sortBy(_._2).map(_._1)))
    }
  }

  override val selectElaborator = SelectElaborator {
    case (QueryType, "cities", List(Binding("namePattern", StringValue(namePattern)))) =>
      if (namePattern == "%")
        Elab.unit
      else
        Elab.transformChild(child => Filter(Like(CityType / "name", namePattern, true), child))

    case (QueryType, "country", List(Binding("code", StringValue(code)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(CountryType / "code", Const(code)), child)))
  }
}
