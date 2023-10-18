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

package minimizer

import munit.CatsEffectSuite

import grackle.{ GraphQLParser, QueryMinimizer }

final class MinimizerSuite extends CatsEffectSuite {
  def run(query: String, expected: String, echo: Boolean = false): Unit = {
    val Right(minimized) = QueryMinimizer.minimizeText(query) : @unchecked
    if (echo)
      println(minimized)

    assert(minimized == expected)

    val Some(parsed0) = GraphQLParser.Document.parseAll(query).toOption : @unchecked
    val Some(parsed1) = GraphQLParser.Document.parseAll(minimized).toOption : @unchecked

    assertEquals(parsed0, parsed1)
  }

  test("minimize simple query") {
    val query = """
      query {
        character(id: 1000) {
          name
        }
      }
    """

    val expected = """query{character(id:1000){name}}"""

    run(query, expected)
  }

  test("minimize multiple parameters (commas)") {
    val query = """
      query {
        wibble(foo: "a", bar: "b", baz: 3) {
          quux
        }
      }
    """

    val expected = """query{wibble(foo:"a",bar:"b",baz:3){quux}}"""

    run(query, expected)
  }

  test("minimize multiple parameters (no commas)") {
    val query = """
      query {
        wibble(foo: "a" bar: "b" baz: 3) {
          quux
        }
      }
    """

    val expected = """query{wibble(foo:"a",bar:"b",baz:3){quux}}"""

    run(query, expected)
  }

  test("minimize introspection query") {
    val query = """
      query IntrospectionQuery {
        __schema {
          queryType {
            name
          }
          mutationType {
            name
          }
          subscriptionType {
            name
          }
        }
      }
    """

    val expected = """query IntrospectionQuery{__schema{queryType{name},mutationType{name},subscriptionType{name}}}"""

    run(query, expected)
  }

  test("minimize shorthand query") {
    val query = """
      {
        hero(episode: NEWHOPE) {
          name
          friends {
            name
            friends {
              name
            }
          }
        }
      }
    """

    val expected = """{hero(episode:NEWHOPE){name,friends{name,friends{name}}}}"""

    run(query, expected)
  }

  test("minimize field alias") {
    val query = """
      {
        user(id: 4) {
          id
          name
          smallPic: profilePic(size: 64)
          bigPic: profilePic(size: 1024)
        }
      }
    """

    val expected = """{user(id:4){id,name,smallPic:profilePic(size:64),bigPic:profilePic(size:1024)}}"""

    run(query, expected)
  }

  test("minimize multiple root fields") {
    val query = """
      {
        luke: character(id: "1000") {
          name
        }
        darth: character(id: "1001") {
          name
        }
      }
    """

    val expected = """{luke:character(id:"1000"){name},darth:character(id:"1001"){name}}"""

    run(query, expected)
  }

  test("minimize variables") {
    val query = """
      query getZuckProfile($devicePicSize: Int) {
        user(id: 4) {
          id
          name
          profilePic(size: $devicePicSize)
        }
      }
    """

    val expected = """query getZuckProfile($devicePicSize:Int){user(id:4){id,name,profilePic(size:$devicePicSize)}}"""

    run(query, expected)
  }

  test("minimize comments") {
    val query = """
      #comment at start of document
      query IntrospectionQuery { #comment at end of line
        __schema {
          queryType {
            name#comment eol no space
          }
          mutationType {
            name
            #several comments
            #one after another
          }
          subscriptionType {
            name
          }
        }
      }
      #comment at end of document
    """

    val expected = """query IntrospectionQuery{__schema{queryType{name},mutationType{name},subscriptionType{name}}}"""

    run(query, expected)
  }


  test("minimize simple fragment query") {
    val query = """
      query withFragments {
        user(id: 1) {
          friends {
            ...friendFields
          }
          mutualFriends {
            ...friendFields
          }
        }
      }

      fragment friendFields on User {
        id
        name
        profilePic
      }
    """

    val expected = """query withFragments{user(id:1){friends{...friendFields},mutualFriends{...friendFields}}},fragment friendFields on User{id,name,profilePic}"""

    run(query, expected)
  }

  test("minimize nested fragment query") {
    val query = """
      query withNestedFragments {
        user(id: 1) {
          friends {
            ...friendFields
          }
          mutualFriends {
            ...friendFields
          }
        }
      }

      fragment friendFields on User {
        id
        name
        ...standardProfilePic
      }

      fragment standardProfilePic on User {
        profilePic
      }
    """

    val expected = """query withNestedFragments{user(id:1){friends{...friendFields},mutualFriends{...friendFields}}},fragment friendFields on User{id,name,...standardProfilePic},fragment standardProfilePic on User{profilePic}"""

    run(query, expected)
  }

  test("minimize typed fragment query") {
    val query = """
      query FragmentTyping {
        profiles {
          id
          __typename
          ...userFragment
          ...pageFragment
        }
      }

      fragment userFragment on User {
        name
      }

      fragment pageFragment on Page {
        title
      }
    """

    val expected = """query FragmentTyping{profiles{id,__typename,...userFragment,...pageFragment}},fragment userFragment on User{name},fragment pageFragment on Page{title}"""

    run(query, expected)
  }

  test("minimize inline fragment query") {
    val query = """
      query inlineFragmentTyping {
        profiles {
          id
          ... on User {
            name
          }
          ... on Page {
            title
          }
        }
      }
    """

    val expected = """query inlineFragmentTyping{profiles{id,...on User{name},...on Page{title}}}"""

    run(query, expected)
  }

  test("minimize typed union fragment query") {
    val query = """
      query FragmentUnionTyping {
        user: user(id: "1") {
          favourite {
            __typename
            ...userFragment
            ...pageFragment
          }
        }
        page: user(id: "2") {
          favourite {
            __typename
            ...userFragment
            ...pageFragment
          }
        }
      }

      fragment userFragment on User {
        id
        name
      }

      fragment pageFragment on Page {
        id
        title
      }
    """

    val expected = """query FragmentUnionTyping{user:user(id:"1"){favourite{__typename,...userFragment,...pageFragment}},page:user(id:"2"){favourite{__typename,...userFragment,...pageFragment}}},fragment userFragment on User{id,name},fragment pageFragment on Page{id,title}"""

    run(query, expected)
  }

  test("minimize skip/include field") {
    val query = """
      query ($yup: Boolean, $nope: Boolean) {
        a: field @skip(if: $yup) {
          subfieldA
        }
        b: field @skip(if: $nope) {
          subfieldB
        }
        c: field @include(if: $yup) {
          subfieldA
        }
        d: field @include(if: $nope) {
          subfieldB
        }
      }
    """

    val expected = """query($yup:Boolean,$nope:Boolean){a:field@skip(if:$yup){subfieldA},b:field@skip(if:$nope){subfieldB},c:field@include(if:$yup){subfieldA},d:field@include(if:$nope){subfieldB}}"""

    run(query, expected)
  }

  test("minimize skip/include fragment spread") {
    val query = """
      query ($yup: Boolean, $nope: Boolean) {
        a: field {
          ...frag @skip(if: $yup)
        }
        b: field {
          ...frag @skip(if: $nope)
        }
        c: field {
          ...frag @include(if: $yup)
        }
        d: field {
          ...frag @include(if: $nope)
        }
      }

      fragment frag on Value {
        subfieldA
        subfieldB
      }
    """

    val expected = """query($yup:Boolean,$nope:Boolean){a:field{...frag@skip(if:$yup)},b:field{...frag@skip(if:$nope)},c:field{...frag@include(if:$yup)},d:field{...frag@include(if:$nope)}},fragment frag on Value{subfieldA,subfieldB}"""

    run(query, expected)
  }

  test("minimize fragment spread with nested skip/include") {
    val query = """
      query ($yup: Boolean, $nope: Boolean) {
        field {
          ...frag
        }
      }

      fragment frag on Value {
        a: subfieldA @skip(if: $yup)
        b: subfieldB @skip(if: $nope)
        c: subfieldA @include(if: $yup)
        d: subfieldB @include(if: $nope)
      }
    """

    val expected = """query($yup:Boolean,$nope:Boolean){field{...frag}},fragment frag on Value{a:subfieldA@skip(if:$yup),b:subfieldB@skip(if:$nope),c:subfieldA@include(if:$yup),d:subfieldB@include(if:$nope)}"""

    run(query, expected)
  }

  test("minimize skip/include inline fragment") {
    val query = """
      query ($yup: Boolean, $nope: Boolean) {
        a: field {
          ... on Value @skip(if: $yup) {
            subfieldA
            subfieldB
          }
        }
        b: field {
          ... on Value @skip(if: $nope) {
            subfieldA
            subfieldB
          }
        }
        c: field {
          ... on Value @include(if: $yup) {
            subfieldA
            subfieldB
          }
        }
        d: field {
          ... on Value @include(if: $nope) {
            subfieldA
            subfieldB
          }
        }
      }
    """

    val expected = """query($yup:Boolean,$nope:Boolean){a:field{...on Value@skip(if:$yup){subfieldA,subfieldB}},b:field{...on Value@skip(if:$nope){subfieldA,subfieldB}},c:field{...on Value@include(if:$yup){subfieldA,subfieldB}},d:field{...on Value@include(if:$nope){subfieldA,subfieldB}}}"""

    run(query, expected)
  }

  test("minimize inline fragment with nested skip/include") {
    val query = """
      query ($yup: Boolean, $nope: Boolean) {
        field {
          ... on Value {
            a: subfieldA @skip(if: $yup)
            b: subfieldB @skip(if: $nope)
            c: subfieldA @include(if: $yup)
            d: subfieldB @include(if: $nope)
          }
        }
      }
    """

    val expected = """query($yup:Boolean,$nope:Boolean){field{...on Value{a:subfieldA@skip(if:$yup),b:subfieldB@skip(if:$nope),c:subfieldA@include(if:$yup),d:subfieldB@include(if:$nope)}}}"""

    run(query, expected)
  }

  test("minimize null value") {
    val query = """
      query {
        field {
          subfield
        }
        field(arg: null) {
          subfield
        }
        field(arg: 23) {
          subfield
        }
      }
    """

    val expected = """query{field{subfield},field(arg:null){subfield},field(arg:23){subfield}}"""

    run(query, expected)
  }

  test("minimize list value") {
    val query = """
      query {
        listField(arg: []) {
          subfield
        }
        listField(arg: ["foo", "bar"]) {
          subfield
        }
      }
    """

    val expected = """query{listField(arg:[]){subfield},listField(arg:["foo","bar"]){subfield}}"""

    run(query, expected)
  }

  test("minimize input object value") {
    val query = """
      query {
        objectField(arg: { foo: 23, bar: true, baz: "quux" }) {
          subfield
        }
      }
    """

    val expected = """query{objectField(arg:{foo:23,bar:true,baz:"quux"}){subfield}}"""

    run(query, expected)
  }

  test("minimize query with UUID argument and custom scalar results") {
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

    val expected = """query{movieById(id:"6a7837fc-b463-4d32-b628-0f4b3065cb21"){id,title,genre,releaseDate,showTime,nextShowing,duration}}"""

    run(query, expected)
  }

  test("minimize query with mapped enum argument") {
    val query = """
      query {
        moviesByGenre(genre: COMEDY) {
          title
          genre
        }
      }
    """

    val expected = """query{moviesByGenre(genre:COMEDY){title,genre}}"""

    run(query, expected)
  }

  test("standard introspection query") {
    val query = """
      |query IntrospectionQuery {
      |  __schema {
      |    queryType { name }
      |    mutationType { name }
      |    subscriptionType { name }
      |    types {
      |      ...FullType
      |    }
      |    directives {
      |      name
      |      description
      |      locations
      |      args {
      |        ...InputValue
      |      }
      |    }
      |  }
      |}
      |
      |fragment FullType on __Type {
      |  kind
      |  name
      |  description
      |  fields(includeDeprecated: true) {
      |    name
      |    description
      |    args {
      |      ...InputValue
      |    }
      |    type {
      |      ...TypeRef
      |    }
      |    isDeprecated
      |    deprecationReason
      |  }
      |  inputFields {
      |    ...InputValue
      |  }
      |  interfaces {
      |    ...TypeRef
      |  }
      |  enumValues(includeDeprecated: true) {
      |    name
      |    description
      |    isDeprecated
      |    deprecationReason
      |  }
      |  possibleTypes {
      |    ...TypeRef
      |  }
      |}
      |
      |fragment InputValue on __InputValue {
      |  name
      |  description
      |  type { ...TypeRef }
      |  defaultValue
      |}
      |
      |fragment TypeRef on __Type {
      |  kind
      |  name
      |  ofType {
      |    kind
      |    name
      |    ofType {
      |      kind
      |      name
      |      ofType {
      |        kind
      |        name
      |        ofType {
      |          kind
      |          name
      |          ofType {
      |            kind
      |            name
      |            ofType {
      |              kind
      |              name
      |              ofType {
      |                kind
      |                name
      |              }
      |            }
      |          }
      |        }
      |      }
      |    }
      |  }
      |}
    """.stripMargin.trim

    val expected = """query IntrospectionQuery{__schema{queryType{name},mutationType{name},subscriptionType{name},types{...FullType},directives{name,description,locations,args{...InputValue}}}},fragment FullType on __Type{kind,name,description,fields(includeDeprecated:true){name,description,args{...InputValue},type{...TypeRef},isDeprecated,deprecationReason},inputFields{...InputValue},interfaces{...TypeRef},enumValues(includeDeprecated:true){name,description,isDeprecated,deprecationReason},possibleTypes{...TypeRef}},fragment InputValue on __InputValue{name,description,type{...TypeRef},defaultValue},fragment TypeRef on __Type{kind,name,ofType{kind,name,ofType{kind,name,ofType{kind,name,ofType{kind,name,ofType{kind,name,ofType{kind,name,ofType{kind,name}}}}}}}}"""

    run(query, expected)
  }
}
