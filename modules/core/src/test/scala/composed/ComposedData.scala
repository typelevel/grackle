// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import cats.Id
import cats.implicits._
import io.circe.Json

import edu.gemini.grackle._
import Query._, Binding._
import QueryInterpreter.mkError

object ComposedMapping extends Mapping[Id] {
  import ComposedSchema._
  import CountryData._

  val currencyMapping =
    ObjectMapping(
      tpe = CurrencyType,
      interpreter = CurrencyQueryInterpreter,
      fieldMappings = Nil
    )

  val countryMapping =
    ObjectMapping(
      tpe = CountryType,
      interpreter = CountryQueryInterpreter,
      fieldMappings = 
        List(
          "currency" -> Subobject(currencyMapping, countryCurrencyJoin)
        )
    )

  val queryMapping =
    ObjectMapping(
      tpe = QueryType,
      interpreter = ComposedQueryInterpreter,
      fieldMappings =
        List(
          "country" -> Subobject(countryMapping),
          "currency" -> Subobject(currencyMapping)
        )
    )

  def countryCurrencyJoin(c: Country, q: Query): Query =
    Select("currency", List(StringBinding("code", c.currencyCode)), q)

  val objectMappings = List(queryMapping, countryMapping, currencyMapping)
}

object ComposedQueryInterpreter extends QueryInterpreter[Id] {
  implicit val F = cats.catsInstancesForId

  val schema = ComposedSchema

  def runRoot(query: Query): Result[Json] = {
    runObject(query, null, null, null)
  }
}

case class ComposedCursor() extends Cursor {
  def isLeaf: Boolean = ???
  def asLeaf: Result[Json] = ???
  def isList: Boolean = ???
  def asList: Result[List[Cursor]] = ???
  def isNullable: Boolean = ???
  def asNullable: Result[Option[Cursor]] = ???
  def hasField(field: String): Boolean = ???
  def field(field: String, args: Map[String, Any]): Result[Cursor] = ???
}

object CurrencyData {
  case class Currency(
    code: String,
    exchangeRate: Double
  )
  
  val GBP = Currency("GBP", 1.25)

  val currencies = List(GBP)
}

object CurrencyQueryInterpreter extends QueryInterpreter[Id] {
  implicit val F = cats.catsInstancesForId

  val schema = ComposedSchema

  import schema._
  import CurrencyData._

  def runRoot(query: Query): Result[Json] = {
    query match {
      case Select("currency", List(StringBinding("code", code)), child) =>
        runObject(child, "currency", NullableType(CurrencyType), CurrencyCursor(currencies.find(_.code == code)))
      case _ => List(mkError("Bad query")).leftIor
    }
  }
}

case class CurrencyCursor(focus: Any) extends DataTypeCursor {
  import CurrencyData._

  def mkCursor(focus: Any): Cursor = CurrencyCursor(focus)

  def hasField(field: String): Boolean = (focus, field) match {
    case (_: Currency, "code" | "exchangeRate") => true
    case _ => false
  }

  def field(field: String, args: Map[String, Any]): Result[Cursor] = {
    focus match {
      case _: Option[_] => assert(false, s"Unexpected option")
      case _: List[_]   => assert(false, s"Unexpected list")
      case _ =>
    }

    (focus, field) match {
      case (c: Currency, "code") => mkCursor(c.code).rightIor
      case (c: Currency, "exchangeRate") => mkCursor(c.exchangeRate).rightIor
      case _ => List(mkError(s"No field '$field'")).leftIor
    }
  }
}

object CountryData {
  case class Country(
    code: String,
    name: String,
    currencyCode: String
  )
  
  val GBR = Country("GBR", "United Kingdom", "GBP")

  val countries = List(GBR)
}

object CountryQueryInterpreter extends QueryInterpreter[Id] {
  implicit val F = cats.catsInstancesForId

  val schema = ComposedSchema

  import schema._
  import CountryData._

  def runRoot(query: Query): Result[Json] = {
    query match {
      case Select("country", List(StringBinding("code", code)), child) =>
        runObject(child, "country", NullableType(CountryType), CountryCursor(countries.find(_.code == code)))
      case _ => List(mkError("Bad query")).leftIor
    }
  }
}

case class CountryCursor(focus: Any) extends DataTypeCursor {
  import CountryData._

  def mkCursor(focus: Any): Cursor = CountryCursor(focus)

  def hasField(field: String): Boolean = (focus, field) match {
    case (_: Country, "code" | "name" | "currency") => true
    case _ => false
  }

  def field(field: String, args: Map[String, Any]): Result[Cursor] = {
    focus match {
      case _: Option[_] => assert(false, s"Unexpected option")
      case _: List[_]   => assert(false, s"Unexpected list")
      case _ =>
    }

    (focus, field) match {
      case (c: Country, "code") => mkCursor(c.code).rightIor
      case (c: Country, "name") => mkCursor(c.name).rightIor
      case (_: Country, "currency") => ???
      case _ => List(mkError(s"No field '$field'")).leftIor
    }
  }
}
