// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini
package grackle
package world

object WorldSchema {
  import Schema._, ScalarType._

  val NamePatternArg = InputValue("namePattern", None, StringType, Some("%"))
  val CodeArg = InputValue("code", None, StringType, None)

  val QueryType: ObjectType =
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("cities", None, List(NamePatternArg), ListType(TypeRef("City")), false, None),
        Field("country", None, List(CodeArg), TypeRef("Country"), false, None),
        Field("countries", None, Nil, ListType(TypeRef("Country")), false, None)
      ),
      interfaces = Nil
    )

  val CityType: ObjectType =
    ObjectType(
      name = "City",
      description = None,
      fields = List(
        Field("name", None, Nil, NonNullType(StringType), false, None),
        Field("country", None, Nil, NonNullType(TypeRef("Country")), false, None),
        Field("district", None, Nil, NonNullType(StringType), false, None),
        Field("population", None, Nil, NonNullType(IntType), false, None)
      ),
      interfaces = Nil
    )

  val LanguageType: ObjectType =
    ObjectType(
      name = "Language",
      description = None,
      fields = List(
        Field("language", None, Nil, NonNullType(StringType), false, None),
        Field("isOfficial", None, Nil, NonNullType(BooleanType), false, None),
        Field("percentage", None, Nil, NonNullType(FloatType), false, None)
      ),
      interfaces = Nil
    )

  val CountryType: ObjectType =
    ObjectType(
      name = "Country",
      description = None,
      fields = List(
        Field("name", None, Nil, NonNullType(StringType), false, None),
        Field("continent", None, Nil, NonNullType(StringType), false, None),
        Field("region", None, Nil, NonNullType(StringType), false, None),
        Field("surfacearea", None, Nil, NonNullType(FloatType), false, None),
        Field("indepyear", None, Nil, IntType, false, None),
        Field("population", None, Nil, NonNullType(IntType), false, None),
        Field("lifeexpectancy", None, Nil, FloatType, false, None),
        Field("gnp", None, Nil, StringType, false, None),
        Field("gnpold", None, Nil, StringType, false, None),
        Field("localname", None, Nil, NonNullType(StringType), false, None),
        Field("governmentform", None, Nil, NonNullType(StringType), false, None),
        Field("headofstate", None, Nil, StringType, false, None),
        Field("capitalId", None, Nil, IntType, false, None),
        Field("code2", None, Nil, NonNullType(StringType), false, None),
        Field("cities", None, Nil, NonNullType(ListType(CityType)), false, None),
        Field("languages", None, Nil, NonNullType(ListType(TypeRef("Language"))), false, None)
      ),
      interfaces = Nil
    )

  val schema =
    Schema(
      types = List(QueryType, CityType, LanguageType, CountryType),
      queryType = TypeRef("Query"),
      mutationType = None,
      subscriptionType = None,
      directives = Nil
    )
}
