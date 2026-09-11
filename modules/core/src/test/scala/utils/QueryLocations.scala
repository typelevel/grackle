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

package utils

import grackle.Ast

/**
 * Helpers for the source locations of field selections.
 */
object QueryLocations {

  /**
   * Helper to create a location for a field selection
   */
  def loc(line: Int, column: Int): Option[(Int, Int)] =
    Some((line, column))

  /**
   * Clears the source location of every field selection of the document `doc`.
   */
  def stripLocations(doc: Ast.Document): Ast.Document =
    doc.map {
      case d: Ast.OperationDefinition.QueryShorthand =>
        d.copy(selectionSet = stripSelectionLocations(d.selectionSet))
      case d: Ast.OperationDefinition.Operation =>
        d.copy(selectionSet = stripSelectionLocations(d.selectionSet))
      case d: Ast.FragmentDefinition =>
        d.copy(selectionSet = stripSelectionLocations(d.selectionSet))
      case other => other
    }

  private def stripSelectionLocations(sels: List[Ast.Selection]): List[Ast.Selection] =
    sels.map {
      case s: Ast.Selection.Field =>
        s.copy(selectionSet = stripSelectionLocations(s.selectionSet), location = None)
      case s: Ast.Selection.InlineFragment =>
        s.copy(selectionSet = stripSelectionLocations(s.selectionSet))
      case other => other
    }

}
