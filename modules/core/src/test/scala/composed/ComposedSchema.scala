// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package composed

import edu.gemini.grackle._

object ComposedSchema extends Schema {
  import ScalarType._

  val CodeArg = InputValue("code", None, NullableType(StringType), None)

  val types = List(
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("country", None, List(CodeArg), NullableType(TypeRef("Country")), false, None),
        Field("fx", None, List(CodeArg), NullableType(TypeRef("Currency")), false, None),
        Field("countries", None, Nil, ListType(TypeRef("Country")), false, None)
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "Currency",
      description = None,
      fields = List(
        Field("code", None, Nil, StringType, false, None),
        Field("exchangeRate", None, Nil, FloatType, false, None),
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "Country",
      description = None,
      fields = List(
        Field("code", None, Nil, StringType, false, None),
        Field("name", None, Nil, StringType, false, None),
        Field("currency", None, Nil, TypeRef("Currency"), false, None)
      ),
      interfaces = Nil
    )
  )

  val directives = Nil
}

object CountrySchema extends Schema {
  import ScalarType._

  val CodeArg = InputValue("code", None, NullableType(StringType), None)

  val types = List(
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("country", None, List(CodeArg), NullableType(TypeRef("Country")), false, None),
        Field("countries", None, Nil, ListType(TypeRef("Country")), false, None)
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "Country",
      description = None,
      fields = List(
        Field("code", None, Nil, StringType, false, None),
        Field("name", None, Nil, StringType, false, None)
      ),
      interfaces = Nil
    )
  )

  val directives = Nil
}

object CurrencySchema extends Schema {
  import ScalarType._

  val CodeArg = InputValue("code", None, NullableType(StringType), None)

  val types = List(
    ObjectType(
      name = "Query",
      description = None,
      fields = List(
        Field("fx", None, List(CodeArg), NullableType(TypeRef("Currency")), false, None)
      ),
      interfaces = Nil
    ),
    ObjectType(
      name = "Currency",
      description = None,
      fields = List(
        Field("code", None, Nil, StringType, false, None),
        Field("exchangeRate", None, Nil, FloatType, false, None),
      ),
      interfaces = Nil
    )
  )

  val directives = Nil
}
