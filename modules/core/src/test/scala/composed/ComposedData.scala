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

package composed

import cats.effect.IO
import cats.implicits._

import grackle._
import grackle.syntax._
import Query._
import Predicate._, Value._
import QueryCompiler._

/* Currency component */

object CurrencyData {
  case class Currency(
    code: String,
    exchangeRate: Double
  )

  val EUR = Currency("EUR", 1.12)
  val GBP = Currency("GBP", 1.25)

  val currencies = List(EUR, GBP)
}

object CurrencyMapping extends ValueMapping[IO] {
  import CurrencyData._

  val schema =
    schema"""
      type Query {
        fx(code: String): Currency
      }
      type Currency {
        code: String!
        exchangeRate: Float!
      }
    """

  val QueryType = schema.ref("Query")
  val CurrencyType = schema.ref("Currency")

  val typeMappings =
    List(
      ValueObjectMapping[Unit](
        tpe = QueryType,
        fieldMappings =
          List(
            ValueField("fx", _ => currencies)
          )
      ),
      ValueObjectMapping[Currency](
        tpe = CurrencyType,
        fieldMappings =
          List(
            ValueField("code", _.code),
            ValueField("exchangeRate", _.exchangeRate)
          )
      )
    )

  override val selectElaborator = SelectElaborator {
    case (QueryType, "fx", List(Binding("code", StringValue(code)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(CurrencyType / "code", Const(code)), child)))
  }
}

/* Country component */

object CountryData {
  case class Country(
    code: String,
    name: String,
    currencyCode: String
  )

  val DEU = Country("DEU", "Germany", "EUR")
  val FRA = Country("FRA", "France", "EUR")
  val GBR = Country("GBR", "United Kingdom", "GBP")

  val countries = List(DEU, FRA, GBR)
}

object CountryMapping extends ValueMapping[IO] {
  import CountryData._

  val schema =
    schema"""
      type Query {
        country(code: String): Country
        countries: [Country!]!
      }
      type Country {
        code: String!
        name: String!
      }
    """

  val QueryType = schema.ref("Query")
  val CountryType = schema.ref("Country")

  val typeMappings =
    List(
      ValueObjectMapping[Unit](
        tpe = QueryType,
        fieldMappings =
          List(
            ValueField("country", _ => countries),
            ValueField("countries", _ => countries)
          )
      ),
      ValueObjectMapping[Country](
        tpe = CountryType,
        fieldMappings =
          List(
            ValueField("code", _.code),
            ValueField("name", _.name)
          )
      )
    )

  override val selectElaborator = SelectElaborator {
    case (QueryType, "country", List(Binding("code", StringValue(code)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(CurrencyMapping.CurrencyType / "code", Const(code)), child)))
  }
}

/* Composition */

object ComposedMapping extends ComposedMapping[IO] {
  val schema =
    schema"""
      type Query {
        country(code: String): Country
        fx(code: String): Currency
        countries: [Country!]!
      }
      type Currency {
        code: String!
        exchangeRate: Float!
      }
      type Country {
        code: String!
        name: String!
        currency: Currency!
      }
    """

  val QueryType = schema.ref("Query")
  val CountryType = schema.ref("Country")
  val CurrencyType = schema.ref("Currency")

  override val selectElaborator = SelectElaborator {
    case (QueryType, "fx", List(Binding("code", StringValue(code)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(CurrencyType / "code", Const(code)), child)))
    case (QueryType, "country", List(Binding("code", StringValue(code)))) =>
      Elab.transformChild(child => Unique(Filter(Eql(CountryType / "code", Const(code)), child)))
  }

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings =
          List(
            Delegate("country", CountryMapping),
            Delegate("countries", CountryMapping),
            Delegate("fx", CurrencyMapping)
          )
        ),
      ObjectMapping(
        tpe = CountryType,
        fieldMappings =
          List(
            Delegate("currency", CurrencyMapping, countryCurrencyJoin)
          )
      )
    )

  def countryCurrencyJoin(q: Query, c: Cursor): Result[Query] =
    (c.focus, q) match {
      case (c: CountryData.Country, Select("currency", _, child)) =>
        Select("fx", Unique(Filter(Eql(CurrencyType / "code", Const(c.currencyCode)), child))).success
      case _ =>
        Result.internalError(s"Unexpected cursor focus type in countryCurrencyJoin")
    }

}
