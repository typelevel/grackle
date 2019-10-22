// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import edu.gemini.grackle._

object ComposedSchema extends Schema {
  import ScalarType._

  val CodeArg = InputValue("code", None, NullableType(StringType), None)

  val QueryType: ObjectType =
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("country", None, List(CodeArg), NullableType(TypeRef("Country")), false, None),
        Field("currency", None, List(CodeArg), NullableType(TypeRef("Currency")), false, None),
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
      ),
      interfaces = Nil
    )

  val CountryType: ObjectType =
    ObjectType(
      name = "Country",
      description = None,
      fields = List(
        Field("code", None, Nil, StringType, false, None),
        Field("name", None, Nil, StringType, false, None),
        Field("currency", None, Nil, ListType(TypeRef("Currency")), false, None)
      ),
      interfaces = Nil
    )

  val types = List(QueryType, CurrencyType, CountryType)
  val queryType = TypeRef("Query")
  val mutationType = None
  val subscriptionType = None
  val directives = Nil
}
