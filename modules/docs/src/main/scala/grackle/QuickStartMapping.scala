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

package grackle.docs

import cats.effect.IO

import grackle._
import grackle.Predicate._
import grackle.Query._
import grackle.QueryCompiler._
import grackle.Value._
import grackle.syntax._

// #quickstart
object QuickStartMapping extends ValueMapping[IO] {

  // The data: an ordinary Scala value, with no Grackle dependencies.
  case class Book(id: Int, title: String, author: String)

  val books =
    List(
      Book(1, "The Left Hand of Darkness", "Ursula K. Le Guin"),
      Book(2, "Kindred", "Octavia E. Butler"),
      Book(3, "Hyperion", "Dan Simmons")
    )

  // The schema, validated at compile time by the `schema` interpolator.
  val schema =
    schema"""
      type Query {
        books: [Book!]!
        book(id: Int!): Book
      }
      type Book {
        id: Int!
        title: String!
        author: String!
      }
    """

  val QueryType = schema.ref("Query")
  val BookType  = schema.ref("Book")

  // The mapping: how each GraphQL field is served from the data.
  val typeMappings =
    List(
      ValueObjectMapping[Unit](
        tpe = QueryType,
        fieldMappings =
          List(
            ValueField("books", _ => books),
            ValueField("book", _ => books)
          )
      ),
      ValueObjectMapping[Book](
        tpe = BookType,
        fieldMappings =
          List(
            ValueField("id", _.id),
            ValueField("title", _.title),
            ValueField("author", _.author)
          )
      )
    )

  // The elaborator: turn `book(id: 2)` into a predicate that selects one book.
  override val selectElaborator =
    SelectElaborator {
      case (QueryType, "book", List(Binding("id", IntValue(id)))) =>
        Elab.transformChild(child => Unique(Filter(Eql(BookType / "id", Const(id)), child)))
    }
}
// #quickstart
