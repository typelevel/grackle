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

package conformance

/**
 * Conformance test cases for section 5.5, Fragments.
 *
 * Each test case adds a driver operation, as [[ValidationFieldsSuite]] describes. A
 * counter-example block which holds more than one fragment becomes one test case per fragment,
 * for the reason which [[ValidationFieldsSuite]] gives.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Validation.Fragments
 */
final class ValidationFragmentsSuite extends ValidationSuite {

  // 5.5.1.1 Fragment Name Uniqueness
  // https://spec.graphql.org/September2025/#sec-Fragment-Name-Uniqueness

  validQuery("two fragments can have different names")("""
    {
      dog {
        ...fragmentOne
        ...fragmentTwo
      }
    }

    fragment fragmentOne on Dog {
      name
    }

    fragment fragmentTwo on Dog {
      owner {
        name
      }
    }
  """)

  invalidQuery("two fragments must not share a name")("""
    {
      dog {
        ...fragmentOne
      }
    }

    fragment fragmentOne on Dog {
      name
    }

    fragment fragmentOne on Dog {
      owner {
        name
      }
    }
  """)

  // 5.5.1.2 Fragment Spread Type Existence
  // https://spec.graphql.org/September2025/#sec-Fragment-Spread-Type-Existence

  validQuery("a fragment and an inline fragment can name a type of the schema")("""
    query driver {
      a: dog { ...correctType }
      b: dog { ...inlineFragment }
      c: dog { ...inlineFragment2 }
    }

    fragment correctType on Dog {
      name
    }

    fragment inlineFragment on Dog {
      ... on Dog {
        name
      }
    }

    fragment inlineFragment2 on Dog {
      ... @include(if: true) {
        name
      }
    }
  """)

  invalidQuery("a fragment must not name a type which the schema does not define")("""
    query driver {
      dog { ...notOnExistingType }
    }

    fragment notOnExistingType on NotInSchema {
      name
    }
  """)

  invalidQuery("an inline fragment must not name a type which the schema does not define")("""
    query driver {
      dog { ...inlineNotExistingType }
    }

    fragment inlineNotExistingType on Dog {
      ... on NotInSchema {
        name
      }
    }
  """)

  // 5.5.1.3 Fragments on Object, Interface or Union Types
  // https://spec.graphql.org/September2025/#sec-Fragments-on-Object-Interface-or-Union-Types

  validQuery("a fragment can be declared on an object, an interface or a union type")("""
    query driver {
      a: dog { ...fragOnObject }
      b: pet { ...fragOnInterface }
      c: catOrDog { ...fragOnUnion }
    }

    fragment fragOnObject on Dog {
      name
    }

    fragment fragOnInterface on Pet {
      name
    }

    fragment fragOnUnion on CatOrDog {
      ... on Dog {
        name
      }
    }
  """)

  invalidQuery("a fragment must not be declared on a scalar type")("""
    query driver {
      dog { ...fragOnScalar }
    }

    fragment fragOnScalar on Int {
      something
    }
  """)

  invalidQuery("an inline fragment must not be declared on a scalar type")("""
    query driver {
      dog { ...inlineFragOnScalar }
    }

    fragment inlineFragOnScalar on Dog {
      ... on Boolean {
        somethingElse
      }
    }
  """)

  // 5.5.1.4 Fragments Must Be Used
  // https://spec.graphql.org/September2025/#sec-Fragments-Must-Be-Used

  invalidQuery("every fragment of a document must be spread at least once")("""
    fragment nameFragment on Dog { # unused
      name
    }

    {
      dog {
        name
      }
    }
  """)

  // 5.5.2.1 Fragment Spread Target Defined
  // https://spec.graphql.org/September2025/#sec-Fragment-Spread-Target-Defined

  invalidQuery("a fragment spread must name a fragment of the document")("""
    {
      dog {
        ...undefinedFragment
      }
    }
  """)

  // 5.5.2.2 Fragment Spreads Must Not Form Cycles
  // https://spec.graphql.org/September2025/#sec-Fragment-Spreads-Must-Not-Form-Cycles

  invalidQuery("two fragments must not spread each other")("""
    {
      dog {
        ...nameFragment
      }
    }

    fragment nameFragment on Dog {
      name
      ...barkVolumeFragment
    }

    fragment barkVolumeFragment on Dog {
      barkVolume
      ...nameFragment
    }
  """)

  invalidQuery("a cycle through a nested field is also a cycle")("""
    {
      dog {
        ...dogFragment
      }
    }

    fragment dogFragment on Dog {
      name
      owner {
        ...ownerFragment
      }
    }

    fragment ownerFragment on Human {
      name
      pets {
        ...dogFragment
      }
    }
  """)

  // 5.5.2.3.1 Object Spreads in Object Scope
  // https://spec.graphql.org/September2025/#sec-Object-Spreads-in-Object-Scope

  validQuery("an object fragment can spread into the same object type")("""
    query driver {
      dog { ...dogFragment }
    }

    fragment dogFragment on Dog {
      ... on Dog {
        barkVolume
      }
    }
  """)

  invalidQuery("an object fragment must not spread into a different object type")("""
    query driver {
      dog { ...catInDogFragmentInvalid }
    }

    fragment catInDogFragmentInvalid on Dog {
      ... on Cat {
        meowVolume
      }
    }
  """)

  // 5.5.2.3.2 Abstract Spreads in Object Scope
  // https://spec.graphql.org/September2025/#sec-Abstract-Spreads-in-Object-Scope

  validQuery("an interface fragment can spread into an object type which implements it")("""
    query driver {
      dog { ...interfaceWithinObjectFragment }
    }

    fragment petNameFragment on Pet {
      name
    }

    fragment interfaceWithinObjectFragment on Dog {
      ...petNameFragment
    }
  """)

  validQuery("a union fragment can spread into an object type which the union holds")("""
    query driver {
      dog { ...unionWithObjectFragment }
    }

    fragment catOrDogNameFragment on CatOrDog {
      ... on Cat {
        meowVolume
      }
    }

    fragment unionWithObjectFragment on Dog {
      ...catOrDogNameFragment
    }
  """)

  // 5.5.2.3.3 Object Spreads in Abstract Scope
  // https://spec.graphql.org/September2025/#sec-Object-Spreads-in-Abstract-Scope

  validQuery("an object fragment can spread into an abstract type which the object belongs to")(
    """
    query driver {
      a: pet { ...petFragment }
      b: catOrDog { ...catOrDogFragment }
    }

    fragment petFragment on Pet {
      name
      ... on Dog {
        barkVolume
      }
    }

    fragment catOrDogFragment on CatOrDog {
      ... on Cat {
        meowVolume
      }
    }
  """)

  invalidQuery("an object fragment must not spread into an interface which excludes it")("""
    query driver {
      sentient { ...sentientFragment }
    }

    fragment sentientFragment on Sentient {
      ... on Dog {
        barkVolume
      }
    }
  """)

  invalidQuery("an object fragment must not spread into a union which excludes it")("""
    query driver {
      humanOrAlien { ...humanOrAlienFragment }
    }

    fragment humanOrAlienFragment on HumanOrAlien {
      ... on Cat {
        meowVolume
      }
    }
  """)

  // 5.5.2.3.4 Abstract Spreads in Abstract Scope
  // https://spec.graphql.org/September2025/#sec-Abstract-Spreads-in-Abstract-Scope

  validQuery("a union fragment can spread into an interface which they share a type with")("""
    query driver {
      pet { ...unionWithInterface }
    }

    fragment unionWithInterface on Pet {
      ...dogOrHumanFragment
    }

    fragment dogOrHumanFragment on DogOrHuman {
      ... on Dog {
        barkVolume
      }
    }
  """)

  invalidQuery("two abstract types with no type in common must not spread into each other")("""
    query driver {
      pet { ...nonIntersectingInterfaces }
    }

    fragment nonIntersectingInterfaces on Pet {
      ...sentientFragment
    }

    fragment sentientFragment on Sentient {
      name
    }
  """)

  // The specification writes the two interface definitions in the same document as the
  // fragments. `ValidationSchema` holds them instead, because a request accepts executable
  // definitions only.
  validQuery("an interface fragment can spread into an interface which implements it")("""
    query driver {
      node { ...interfaceWithInterface }
    }

    fragment interfaceWithInterface on Node {
      ...resourceFragment
    }

    fragment resourceFragment on Resource {
      url
    }
  """)
}
