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

package grackle

import grackle.Query.{UntypedFragment, VarDefs}

/**
 * The variable-free result of compiling a GraphQL document.
 *
 * A `PreparedDocument` depends on the document text and on the schema of the compiler that
 * produced it. It does not depend on the variable values, on the `Env`, on the operation name,
 * or on the introspection level, so one instance serves every request that sends the same
 * document text.
 *
 * A `PreparedDocument` belongs to one `QueryCompiler`. Do not share one across two compilers
 * with different schemas.
 */
final class PreparedDocument private[grackle] (
    compiler: QueryCompiler,
    val ops: List[PreparedOperation],
    val frags: List[UntypedFragment]) {

  private def untypedOps: List[UntypedOperation] = ops.map(_.op)

  /**
   * The problems from `validateVariablesAndFragments` for the given `reportUnused` flag. Each
   * value of the flag is computed at most once, so one prepared document serves both.
   */
  private[grackle] def varAndFragProblems(reportUnused: Boolean): List[Problem] =
    if (reportUnused) varAndFragProblemsReportingUnused else varAndFragProblemsIgnoringUnused

  private lazy val varAndFragProblemsReportingUnused: List[Problem] =
    compiler.validateVariablesAndFragments(untypedOps, frags, true)

  private lazy val varAndFragProblemsIgnoringUnused: List[Problem] =
    compiler.validateVariablesAndFragments(untypedOps, frags, false)

  /**
   * The problems from `validateFieldMergeability`.
   */
  private[grackle] lazy val mergeProblems: List[Problem] =
    compiler.validateFieldMergeability(untypedOps, frags)
}

/**
 * The variable-free result of compiling one operation of a GraphQL document.
 */
final class PreparedOperation private[grackle] (
    compiler: QueryCompiler,
    schema: Schema,
    val op: UntypedOperation,
    private[grackle] val frags: List[UntypedFragment]) {

  def name: Option[String] = op.name

  private[grackle] lazy val varDefs: Result[VarDefs] = compiler.compileVarDefs(op.variables)

  private[grackle] lazy val rootTpe: Result[NamedType] = op.rootTpe(schema)

  private[grackle] lazy val usages: Result[Unit] =
    for {
      rt <- rootTpe
      vds <- varDefs
      _ <- VariableUsage.validateVariableUsages(schema, rt, op, frags, vds)
    } yield ()

  private[grackle] lazy val fragMap: Map[String, UntypedFragment] =
    frags.map(frag => (frag.name, frag)).toMap
}
