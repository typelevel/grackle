// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package sql

sealed trait NullabilityKnown

case object NoNulls extends NullabilityKnown
case object Nullable extends NullabilityKnown
