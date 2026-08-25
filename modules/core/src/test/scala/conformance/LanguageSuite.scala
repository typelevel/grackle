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

import conformance.LanguageMappings.Site
import io.circe.literal._

/**
 * Conformance test cases for section 2, Language.
 *
 * The specification defines no schema for these examples, so most test cases check the grammar
 * only. Where the specification states a response, the test case runs the document against
 * [[LanguageMappings.Site]] and asserts that response.
 *
 * @see
 *   https://spec.graphql.org/September2025/#sec-Language
 */
final class LanguageSuite extends ConformanceSuite {

  // 1 Overview
  // https://spec.graphql.org/September2025/#sec-Overview

  yields("a request selects a field of an object and a field of that object", Site)("""
    {
      user(id: 4) {
        name
      }
    }
  """)(json"""
    {
      "data": {
        "user": {
          "name": "Mark Zuckerberg"
        }
      }
    }
  """)

  // 2.2 Descriptions
  // https://spec.graphql.org/September2025/#sec-Descriptions

  parses("an operation, a variable and a fragment can carry a description".fail)("""
    '''
    Request the current status of a time machine and its operator.
    You can also check the status for a particular year.
    **Warning:** certain years may trigger an anomaly in the space-time continuum.
    '''
    query GetTimeMachineStatus(
      "The unique serial number of the time machine to inspect."
      $machineId: ID!
      "The year to check the status for."
      $year: Int
    ) {
      timeMachine(id: $machineId) {
        ...TimeMachineDetails
        status(year: $year)
      }
    }

    "Details about a time machine and its operator."
    fragment TimeMachineDetails on TimeMachine {
      id
      model
      lastMaintenance
      operator {
        name
        licenseLevel
      }
    }
  """)

  // 2.4 Operations
  // https://spec.graphql.org/September2025/#sec-Language.Operations

  parses("a mutation operation can carry a description".fail)("""
    '''
    Mark story 12345 as "liked"
    and return the updated number of likes on the story
    '''
    mutation {
      likeStory(storyID: 12345) {
        story {
          likeCount
        }
      }
    }
  """)

  parses("a query with no name and no variable definitions can use the shorthand form")("""
    {
      field
    }
  """)

  // 2.5 Selection Sets
  // https://spec.graphql.org/September2025/#sec-Selection-Sets

  parses("a selection set requests a set of information")("""
    {
      id
      firstName
      lastName
    }
  """)

  // 2.6 Fields
  // https://spec.graphql.org/September2025/#sec-Language.Fields

  parses("a field can select a nested selection set")("""
    {
      me {
        id
        firstName
        lastName
        birthday {
          month
          day
        }
        friends {
          name
        }
      }
    }
  """)

  parses("a comment runs to the end of the line")("""
    # `me` could represent the currently logged in viewer.
    {
      me {
        name
      }
    }
  """)

  parses("a field can take an argument")("""
    # `user` represents one of many users in a graph of data, referred to by a
    # unique identifier.
    {
      user(id: 4) {
        name
      }
    }
  """)

  // 2.7 Arguments
  // https://spec.graphql.org/September2025/#sec-Language.Arguments

  parses("an argument names a value")("""
    {
      user(id: 4) {
        id
        name
        profilePic(size: 100)
      }
    }
  """)

  parses("a field can take more than one argument")("""
    {
      user(id: 4) {
        id
        name
        profilePic(width: 100, height: 50)
      }
    }
  """)

  parses("arguments in one order")("""
    {
      picture(width: 200, height: 100)
    }
  """)

  parses("the same arguments in the reverse order, which is equivalent")("""
    {
      picture(height: 100, width: 200)
    }
  """)

  // 2.8 Field Alias
  // https://spec.graphql.org/September2025/#sec-Field-Alias

  yields("an alias renames the response key of a field", Site)("""
    {
      user(id: 4) {
        id
        name
        smallPic: profilePic(size: 64)
        bigPic: profilePic(size: 1024)
      }
    }
  """)(json"""
    {
      "data": {
        "user": {
          "id": 4,
          "name": "Mark Zuckerberg",
          "smallPic": "https://cdn.site.io/pic-4-64.jpg",
          "bigPic": "https://cdn.site.io/pic-4-1024.jpg"
        }
      }
    }
  """)

  yields("an alias applies to a top level field", Site)("""
    {
      zuck: user(id: 4) {
        id
        name
      }
    }
  """)(json"""
    {
      "data": {
        "zuck": {
          "id": 4,
          "name": "Mark Zuckerberg"
        }
      }
    }
  """)

  // 2.9 Fragments
  // https://spec.graphql.org/September2025/#sec-Language.Fragments

  parses("a query which repeats a selection set")("""
    query noFragments {
      user(id: 4) {
        friends(first: 10) {
          id
          name
          profilePic(size: 50)
        }
        mutualFriends(first: 10) {
          id
          name
          profilePic(size: 50)
        }
      }
    }
  """)

  parses("a fragment factors out a repeated selection set".fail)("""
    query withFragments {
      user(id: 4) {
        friends(first: 10) {
          ...friendFields
        }
        mutualFriends(first: 10) {
          ...friendFields
        }
      }
    }

    "Common fields for a user's friends."
    fragment friendFields on User {
      id
      name
      profilePic(size: 50)
    }
  """)

  parses("a fragment can spread another fragment")("""
    query withNestedFragments {
      user(id: 4) {
        friends(first: 10) {
          ...friendFields
        }
        mutualFriends(first: 10) {
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
      profilePic(size: 50)
    }
  """)

  // 2.9.1 Type Conditions
  // https://spec.graphql.org/September2025/#sec-Type-Conditions

  yields("a fragment declares the type it applies to", Site)("""
    query FragmentTyping {
      profiles(handles: ["zuck", "coca-cola"]) {
        handle
        ...userFragment
        ...pageFragment
      }
    }

    fragment userFragment on User {
      friends {
        count
      }
    }

    fragment pageFragment on Page {
      likers {
        count
      }
    }
  """)(json"""
    {
      "data": {
        "profiles": [
          {
            "handle": "zuck",
            "friends": { "count": 1234 }
          },
          {
            "handle": "coca-cola",
            "likers": { "count": 90234512 }
          }
        ]
      }
    }
  """)

  // 2.9.2 Inline Fragments
  // https://spec.graphql.org/September2025/#sec-Inline-Fragments

  parses("an inline fragment applies a type condition without a fragment definition")("""
    query inlineFragmentTyping {
      profiles(handles: ["zuck", "coca-cola"]) {
        handle
        ... on User {
          friends {
            count
          }
        }
        ... on Page {
          likers {
            count
          }
        }
      }
    }
  """)

  parses("an inline fragment can omit the type condition and carry a directive")("""
    query inlineFragmentNoType($expandedInfo: Boolean) {
      user(handle: "zuck") {
        id
        name
        ... @include(if: $expandedInfo) {
          firstName
          lastName
          birthday
        }
      }
    }
  """)

  // 2.10.4 String Value
  // https://spec.graphql.org/September2025/#sec-String-Value

  parses("a block string spans lines and strips the common indentation")("""
    mutation {
      sendEmail(message: '''
        Hello,
          World!

        Yours,
          GraphQL.
      ''')
    }
  """)

  parses("the same value written as a single line string")("""
    mutation {
      sendEmail(message: "Hello,\n  World!\n\nYours,\n  GraphQL.")
    }
  """)

  // 2.10.5 Null Value
  // https://spec.graphql.org/September2025/#sec-Null-Value

  parses("an explicit null argument differs from an absent argument")("""
    {
      field(arg: null)
      field
    }
  """)

  // 2.10.8 Input Object Values
  // https://spec.graphql.org/September2025/#sec-Input-Object-Values

  parses("input object fields in one order")("""
    {
      nearestThing(location: { lon: 12.43, lat: -53.211 })
    }
  """)

  parses("the same input object fields in the reverse order, which is equivalent")("""
    {
      nearestThing(location: { lat: -53.211, lon: 12.43 })
    }
  """)

  // 2.11 Variables
  // https://spec.graphql.org/September2025/#sec-Language.Variables

  parses("a variable definition can carry a description".fail)("""
    query getZuckProfile(
      "The size of the profile picture to fetch."
      $devicePicSize: Int
    ) {
      user(id: 4) {
        id
        name
        profilePic(size: $devicePicSize)
      }
    }
  """)

  // The specification states the variable values `{"devicePicSize": 60}` for the example above.
  // This test case supplies those values. It drops the description of the variable definition,
  // because the parser rejects it, which the test case above records.
  yields(
    "a request supplies a value for the variable of an operation",
    Site,
    json"""{"devicePicSize": 60}""")("""
    query getZuckProfile($devicePicSize: Int) {
      user(id: 4) {
        id
        name
        profilePic(size: $devicePicSize)
      }
    }
  """)(json"""
    {
      "data": {
        "user": {
          "id": 4,
          "name": "Mark Zuckerberg",
          "profilePic": "https://cdn.site.io/pic-4-60.jpg"
        }
      }
    }
  """)

  // 2.13 Directives
  // https://spec.graphql.org/September2025/#sec-Language.Directives

  parses("directives on a type definition in one order")("""
    type Person
      @addExternalFields(source: "profiles")
      @excludeField(name: "photo") {
      name: String
    }
  """)

  parses("the same directives in the reverse order, which can mean something else")("""
    type Person
      @excludeField(name: "photo")
      @addExternalFields(source: "profiles") {
      name: String
    }
  """)
}
