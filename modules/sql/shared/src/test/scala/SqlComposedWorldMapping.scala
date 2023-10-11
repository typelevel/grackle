// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle.sql.test

import cats.effect.{Ref, Sync}
import cats.implicits._
import edu.gemini.grackle._
import edu.gemini.grackle.sql.Like
import edu.gemini.grackle.syntax._
import io.circe.Json

import Query._
import Predicate._
import Value._
import QueryCompiler._
import QueryInterpreter.ProtoJson

/* Currency component */

case class Currency(
  code: String,
  exchangeRate: Double,
  countryCode: String
)

case class CurrencyData(currencies: Map[String, Currency]) {
  def exchangeRate(code: String): Option[Double] = currencies.get(code).map(_.exchangeRate)
  def update(code: String, exchangeRate: Double): Option[CurrencyData] =
    currencies.get(code).map(currency => CurrencyData(currencies.updated(code, currency.copy(exchangeRate = exchangeRate))))
  def currencies(countryCodes: List[String]): List[Currency] =
    countryCodes.flatMap(cc => currencies.values.find(_.countryCode == cc).toList)
}

class CurrencyMapping[F[_] : Sync](dataRef: Ref[F, CurrencyData], countRef: Ref[F, Int]) extends ValueMapping[F] {
  def update(code: String, exchangeRate: Double): F[Unit] =
    dataRef.update(data => data.update(code, exchangeRate).getOrElse(data))

  def count: F[Int] = countRef.get

  val schema =
    schema"""
      type Query {
        exchangeRate(code: String!): Currency
        currencies(countryCodes: [String!]!): [Currency!]!
      }
      type Currency {
        code: String!
        exchangeRate: Float!
        countryCode: String!
      }
    """

  val QueryType = schema.ref("Query")
  val CurrencyType = schema.ref("Currency")

  override val selectElaborator = SelectElaborator {
    case (QueryType, "exchangeRate", List(Binding("code", StringValue(code)))) =>
      Elab.env("code", code)
    case (QueryType, "currencies", List(Binding("countryCodes", StringListValue(countryCodes)))) =>
      Elab.env("countryCodes", countryCodes)
  }

  val typeMappings =
    List(
      ValueObjectMapping[Unit](
        tpe = QueryType,
        fieldMappings =
          List(
            RootEffect.computeCursor("exchangeRate")((path, env) =>
              env.getR[String]("code").traverse(code =>
                countRef.update(_+1) *>
                dataRef.get.map(data => valueCursor(path, env, data.exchangeRate(code)))
              )
            ),
            RootEffect.computeCursor("currencies")((path, env) =>
              env.getR[List[String]]("countryCodes").traverse(countryCodes =>
                countRef.update(_+1) *>
                dataRef.get.map(data => valueCursor(path, env, data.currencies(countryCodes)))
              )
            )
          )
      ),
      ValueObjectMapping[Currency](
        tpe = CurrencyType,
        fieldMappings =
          List(
            ValueField("code", _.code),
            ValueField("exchangeRate", _.exchangeRate),
            ValueField("countryCode", _.countryCode)
          )
      )
  )

  override def combineAndRun(queries: List[(Query, Cursor)]): F[Result[List[ProtoJson]]] = {
    import SimpleCurrencyQuery.unpackResults

    val expandedQueries =
      queries.map {
        case (Select("currencies", _, child), c@Code(code)) =>
          (SimpleCurrencyQuery(List(code), child), c)
        case other => other
      }

    if(expandedQueries.sizeCompare(1) <= 0) super.combineAndRun(expandedQueries)
    else {
      val indexedQueries = expandedQueries.zipWithIndex
      val (groupable, ungroupable) = indexedQueries.partition {
        case ((SimpleCurrencyQuery(_, _), _), _) => true
        case _ => false
      }

      def mkKey(q: ((Query, Cursor), String, Int)): (Query, Env, Context) =
        (q._1._1, q._1._2.fullEnv, q._1._2.context)

      val grouped: List[((Query, Cursor), List[Int])] =
        (groupable.collect {
          case ((SimpleCurrencyQuery(code, child), cursor), i) =>
            ((child, cursor), code, i)
        }).groupBy(mkKey).toList.map {
          case ((child, _, _), xs) =>
            val (codes, is) = xs.foldLeft((List.empty[String], List.empty[Int])) {
              case ((codes, is), (_, code, i)) => (code :: codes, i :: is)
            }
            val cursor = xs.head._1._2
            ((SimpleCurrencyQuery(codes, child), cursor), is)
        }

      val (grouped0, groupedIndices) = grouped.unzip
      val (ungroupable0, ungroupedIndices) = ungroupable.unzip

      (for {
        groupedResults   <- ResultT(super.combineAndRun(grouped0))
        ungroupedResults <- ResultT(super.combineAndRun(ungroupable0))
      } yield {
        val allResults: List[(ProtoJson, Int)] = groupedResults.zip(groupedIndices).flatMap((unpackResults _).tupled) ++ ungroupedResults.zip(ungroupedIndices)
        val repackedResults = indexedQueries.map {
          case (_, i) => allResults.find(_._2 == i).map(_._1).getOrElse(ProtoJson.fromJson(Json.Null))
        }

        repackedResults
      }).value
    }
  }

  object Code {
    def unapply(c: Cursor): Option[String] = c.fieldAs[String]("code").toOption
  }

  object SimpleCurrencyQuery {
    def apply(codes: List[String], child: Query): Query =
      Environment(Env("countryCodes" -> codes), Select("currencies", child))

    def unapply(sel: Query): Option[(String, Query)] =
      sel match {
        case Environment(env, Select("currencies", None, child)) => env.get[List[String]]("countryCodes").flatMap(_.headOption).map((_, child))
        case _ => None
      }

    def unpackResults(res: ProtoJson, indices: List[Int]): List[(ProtoJson, Int)] =
      res match {
        case j: Json =>
          (for {
            obj <- j.asObject
            fld <- obj("currencies")
            arr <- fld.asArray
          } yield {
            val ress = arr.toList.map { elem => ProtoJson.fromJson(Json.obj("currencies" -> Json.arr(elem))) }
            ress.zip(indices)
          }).getOrElse(Nil)
        case _ => Nil
      }
  }
}

object CurrencyMapping {
  def apply[F[_] : Sync]: F[CurrencyMapping[F]] = {
    val BRL = Currency("BRL", 0.25, "BRA")
    val EUR = Currency("EUR", 1.12, "NLD")
    val GBP = Currency("GBP", 1.25, "GBR")

    val data = CurrencyData(List(BRL, EUR, GBP).map(c => (c.code, c)).toMap)

    for {
      dataRef  <- Ref[F].of(data)
      countRef <- Ref[F].of(0)
    } yield new CurrencyMapping[F](dataRef, countRef)
  }
}

/* Composition */

class SqlComposedMapping[F[_] : Sync]
  (world: Mapping[F], currency: Mapping[F]) extends ComposedMapping[F] {
  val schema =
    schema"""
      type Query {
        cities(namePattern: String = "%"): [City!]
        country(code: String): Country
        countries: [Country!]
        currencies: [Currency!]!
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
      type Currency {
        code: String!
        exchangeRate: Float!
        countryCode: String!
      }
      type Country {
        name: String!
        continent: String!
        region: String!
        surfacearea: Float!
        indepyear: Int
        population: Int!
        lifeexpectancy: Float
        gnp: String
        gnpold: String
        localname: String!
        governmentform: String!
        headofstate: String
        capitalId: Int
        code2: String!
        cities: [City!]!
        languages: [Language!]!
        currencies: [Currency!]!
      }
    """

  val QueryType = schema.ref("Query")
  val CountryType = schema.ref("Country")
  val CurrencyType = schema.ref("Currency")
  val CityType = schema.ref("City")

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            Delegate("country", world),
            Delegate("countries", world),
            Delegate("cities", world)
          )
      ),
      ObjectMapping(
        tpe = CountryType,
        fieldMappings =
          List(
            Delegate("currencies", currency)
          )
      )
    )

  override val selectElaborator =  SelectElaborator {
    case (QueryType, "country", List(Binding("code", StringValue(code)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(CountryType / "code", Const(code)), child)))
    case (QueryType, "cities", List(Binding("namePattern", StringValue(namePattern)))) =>
      Elab.transformChild(child => Filter(Like(CityType / "name", namePattern, true), child))
  }
}
