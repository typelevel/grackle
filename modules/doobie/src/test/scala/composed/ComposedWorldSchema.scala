// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import edu.gemini.grackle._

object ComposedWorldSchema extends Schema {
  import ScalarType._

  val NamePatternArg = InputValue("namePattern", None, NullableType(StringType), Some("%"))
  val CodeArg = InputValue("code", None, NullableType(StringType), None)

  val QueryType: ObjectType =
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("cities", None, List(NamePatternArg), NullableType(ListType(TypeRef("City"))), false, None),
        Field("country", None, List(CodeArg), NullableType(TypeRef("Country")), false, None),
        Field("countries", None, Nil, NullableType(ListType(TypeRef("Country"))), false, None)
      ),
      interfaces = Nil
    )

  val CityType: ObjectType =
    ObjectType(
      name = "City",
      description = None,
      fields = List(
        Field("name", None, Nil, StringType, false, None),
        Field("country", None, Nil, TypeRef("Country"), false, None),
        Field("district", None, Nil, StringType, false, None),
        Field("population", None, Nil, IntType, false, None)
      ),
      interfaces = Nil
    )

  val LanguageType: ObjectType =
    ObjectType(
      name = "Language",
      description = None,
      fields = List(
        Field("language", None, Nil, StringType, false, None),
        Field("isOfficial", None, Nil, BooleanType, false, None),
        Field("percentage", None, Nil, FloatType, false, None),
        Field("countries", None, Nil, ListType(TypeRef("Country")), false, None)
      ),
      interfaces = Nil
    )

  val CurrencyType: ObjectType =
    ObjectType(
      name = "Currency",
      description = None,
      fields = List(
        Field("code", None, Nil, StringType, false, None),
        Field("exchangeRate", None, Nil, FloatType, false, None),
        Field("countryCode", None, Nil, StringType, false, None)
      ),
      interfaces = Nil
    )

  val CountryType: ObjectType =
    ObjectType(
      name = "Country",
      description = None,
      fields = List(
        Field("name", None, Nil, StringType, false, None),
        Field("continent", None, Nil, StringType, false, None),
        Field("region", None, Nil, StringType, false, None),
        Field("surfacearea", None, Nil, FloatType, false, None),
        Field("indepyear", None, Nil, NullableType(IntType), false, None),
        Field("population", None, Nil, IntType, false, None),
        Field("lifeexpectancy", None, Nil, NullableType(FloatType), false, None),
        Field("gnp", None, Nil, NullableType(StringType), false, None),
        Field("gnpold", None, Nil, NullableType(StringType), false, None),
        Field("localname", None, Nil, StringType, false, None),
        Field("governmentform", None, Nil, StringType, false, None),
        Field("headofstate", None, Nil, NullableType(StringType), false, None),
        Field("capitalId", None, Nil, NullableType(IntType), false, None),
        Field("code2", None, Nil, StringType, false, None),
        Field("cities", None, Nil, ListType(TypeRef("City")), false, None),
        Field("languages", None, Nil, ListType(TypeRef("Language")), false, None),
        Field("currencies", None, Nil, ListType(TypeRef("Currency")), false, None)
      ),
      interfaces = Nil
    )

  val types = List(QueryType, CityType, LanguageType, CurrencyType, CountryType)
  val queryType = TypeRef("Query")
  val mutationType = None
  val subscriptionType = None
  val directives = Nil
}

object WorldSchema extends Schema {
  import ScalarType._

  val NamePatternArg = InputValue("namePattern", None, NullableType(StringType), Some("%"))
  val CodeArg = InputValue("code", None, NullableType(StringType), None)

  val QueryType: ObjectType =
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("cities", None, List(NamePatternArg), NullableType(ListType(TypeRef("City"))), false, None),
        Field("country", None, List(CodeArg), NullableType(TypeRef("Country")), false, None),
        Field("countries", None, Nil, NullableType(ListType(TypeRef("Country"))), false, None)
      ),
      interfaces = Nil
    )

  val CityType: ObjectType =
    ObjectType(
      name = "City",
      description = None,
      fields = List(
        Field("name", None, Nil, StringType, false, None),
        Field("country", None, Nil, TypeRef("Country"), false, None),
        Field("district", None, Nil, StringType, false, None),
        Field("population", None, Nil, IntType, false, None)
      ),
      interfaces = Nil
    )

  val LanguageType: ObjectType =
    ObjectType(
      name = "Language",
      description = None,
      fields = List(
        Field("language", None, Nil, StringType, false, None),
        Field("isOfficial", None, Nil, BooleanType, false, None),
        Field("percentage", None, Nil, FloatType, false, None),
        Field("countries", None, Nil, ListType(TypeRef("Country")), false, None)
      ),
      interfaces = Nil
    )

  val CountryType: ObjectType =
    ObjectType(
      name = "Country",
      description = None,
      fields = List(
        Field("name", None, Nil, StringType, false, None),
        Field("continent", None, Nil, StringType, false, None),
        Field("region", None, Nil, StringType, false, None),
        Field("surfacearea", None, Nil, FloatType, false, None),
        Field("indepyear", None, Nil, NullableType(IntType), false, None),
        Field("population", None, Nil, IntType, false, None),
        Field("lifeexpectancy", None, Nil, NullableType(FloatType), false, None),
        Field("gnp", None, Nil, NullableType(StringType), false, None),
        Field("gnpold", None, Nil, NullableType(StringType), false, None),
        Field("localname", None, Nil, StringType, false, None),
        Field("governmentform", None, Nil, StringType, false, None),
        Field("headofstate", None, Nil, NullableType(StringType), false, None),
        Field("capitalId", None, Nil, NullableType(IntType), false, None),
        Field("code2", None, Nil, StringType, false, None),
        Field("cities", None, Nil, ListType(TypeRef("City")), false, None),
        Field("languages", None, Nil, ListType(TypeRef("Language")), false, None)
      ),
      interfaces = Nil
    )

  val types = List(QueryType, CityType, LanguageType, CountryType)
  val queryType = TypeRef("Query")
  val mutationType = None
  val subscriptionType = None
  val directives = Nil
}

object CurrencySchema extends SchemaComponent {
  import ScalarType._

  val CurrencyType: ObjectType =
    ObjectType(
      name = "Currency",
      description = None,
      fields = List(
        Field("code", None, Nil, StringType, false, None),
        Field("exchangeRate", None, Nil, FloatType, false, None),
        Field("countryCode", None, Nil, StringType, false, None)
      ),
      interfaces = Nil
    )

  val types = List(CurrencyType)
}
