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

package grackle

import scala.annotation.tailrec
import scala.reflect.ClassTag

import cats.data.StateT
import cats.implicits._
import io.circe.Json
import org.tpolecat.typename.{ TypeName, typeName }

import syntax._
import Query._, Predicate._, Value._, UntypedOperation._
import QueryCompiler._
import ScalarType._

/**
 * GraphQL query parser
 */
trait QueryParser {
  /**
   *  Parse a String to query algebra operations and fragments.
   *
   *  GraphQL errors and warnings are accumulated in the result.
   */
  def parseText(text: String): Result[(List[UntypedOperation], List[UntypedFragment])]

  /**
   *  Parse a document AST to query algebra operations and fragments.
   *
   *  GraphQL errors and warnings are accumulated in the result.
   */
  def parseDocument(doc: Ast.Document): Result[(List[UntypedOperation], List[UntypedFragment])]
}

object QueryParser {
  def apply(parser: GraphQLParser): QueryParser =
    new Impl(parser)

  private final class Impl(parser: GraphQLParser) extends QueryParser {
    import Ast.{ Directive => _, Type => _, Value => _, _ }, OperationDefinition._, Selection._

    /**
    *  Parse a String to query algebra operations and fragments.
    *
    *  GraphQL errors and warnings are accumulated in the result.
    */
    def parseText(text: String): Result[(List[UntypedOperation], List[UntypedFragment])] =
      for {
        doc <- parser.parseText(text)
        res <- parseDocument(doc)
        _   <- Result.failure("At least one operation required").whenA(res._1.isEmpty)
      } yield res

    /**
    *  Parse a document AST to query algebra operations and fragments.
    *
    *  GraphQL errors and warnings are accumulated in the result.
    */
    def parseDocument(doc: Document): Result[(List[UntypedOperation], List[UntypedFragment])] = {
      val ops0 = doc.collect { case op: OperationDefinition => op }
      val fragments0 = doc.collect { case frag: FragmentDefinition => frag }

      for {
        ops    <- ops0.traverse {
                    case op: Operation => parseOperation(op)
                    case qs: QueryShorthand => parseQueryShorthand(qs)
                  }
        frags  <- fragments0.traverse { frag =>
                    val tpnme = frag.typeCondition.name
                    for {
                      sels <- parseSelections(frag.selectionSet)
                      dirs <- parseDirectives(frag.directives)
                    } yield UntypedFragment(frag.name.value, tpnme, dirs, sels)
                  }
      } yield (ops, frags)
    }

    /**
    *  Parse an operation AST to a query algebra operation.
    *
    *  GraphQL errors and warnings are accumulated in the result.
    */
    def parseOperation(op: Operation): Result[UntypedOperation] = {
      val Operation(opType, name, vds, dirs0, sels) = op
      for {
        vs   <- parseVariableDefinitions(vds)
        q    <- parseSelections(sels)
        dirs <- parseDirectives(dirs0)
      } yield {
        val name0 = name.map(_.value)
        opType match {
          case OperationType.Query => UntypedQuery(name0, q, vs, dirs)
          case OperationType.Mutation => UntypedMutation(name0, q, vs, dirs)
          case OperationType.Subscription => UntypedSubscription(name0, q, vs, dirs)
        }
      }
    }

    /**
      * Parse variable definition ASTs to query algebra variable definitions.
      *
      * GraphQL errors and warnings are accumulated in the result.
      */
    def parseVariableDefinitions(vds: List[VariableDefinition]): Result[List[UntypedVarDef]] =
      vds.traverse {
        case VariableDefinition(Name(nme), tpe, dv0, dirs0) =>
          for {
            dv   <- dv0.traverse(Value.fromAst)
            dirs <- parseDirectives(dirs0)
          } yield UntypedVarDef(nme, tpe, dv, dirs)
      }

    /**
      * Parse a query shorthand AST to query algebra operation.
      *
      * GraphQL errors and warnings are accumulated in the result.
      */
    def parseQueryShorthand(qs: QueryShorthand): Result[UntypedOperation] =
      parseSelections(qs.selectionSet).map(q => UntypedQuery(None, q, Nil, Nil))

    /**
      * Parse selection ASTs to query algebra terms.
      *
      * GraphQL errors and warnings are accumulated in the result
      */
    def parseSelections(sels: List[Selection]): Result[Query] =
      sels.traverse(parseSelection).map { sels0 =>
        if (sels0.sizeCompare(1) == 0) sels0.head else Group(sels0)
      }

    /**
      * Parse a selection AST to a query algebra term.
      *
      * GraphQL errors and warnings are accumulated in the result.
      */
    def parseSelection(sel: Selection): Result[Query] = sel match {
      case Field(alias, name, args, directives, sels) =>
        for {
          args0 <- parseArgs(args)
          sels0 <- parseSelections(sels)
          dirs  <- parseDirectives(directives)
        } yield {
          val nme = name.value
          val alias0 = alias.map(_.value).flatMap(n => if (n == nme) None else Some(n))
          if (sels.isEmpty) UntypedSelect(nme, alias0, args0, dirs, Empty)
          else UntypedSelect(nme, alias0, args0, dirs, sels0)
        }

      case FragmentSpread(Name(name), directives) =>
        for {
          dirs <- parseDirectives(directives)
        } yield UntypedFragmentSpread(name, dirs)

      case InlineFragment(typeCondition, directives, sels) =>
        for {
          dirs  <- parseDirectives(directives)
          sels0 <- parseSelections(sels)
        } yield UntypedInlineFragment(typeCondition.map(_.name), dirs, sels0)
    }

    /**
      * Parse directive ASTs to query algebra directives.
      *
      * GraphQL errors and warnings are accumulated in the result.
      */
    def parseDirectives(directives: List[Ast.Directive]): Result[List[Directive]] =
      directives.traverse(Directive.fromAst)

    /**
      * Parse argument ASTs to query algebra bindings.
      *
      * GraphQL errors and warnings are accumulated in the result.
      */
    def parseArgs(args: List[(Name, Ast.Value)]): Result[List[Binding]] =
      args.traverse((parseArg _).tupled)

    /**
      * Parse an argument AST to a query algebra binding.
      *
      * GraphQL errors and warnings are accumulated in the result.
      */
    def parseArg(name: Name, value: Ast.Value): Result[Binding] =
      Value.fromAst(value).map(v => Binding(name.value, v))
  }
}

/**
 * GraphQL query compiler.
 *
 * A QueryCompiler parses GraphQL queries to query algebra terms, then
 * applies a collection of transformation phases in sequence, yielding a
 * query algebra term which can be directly interpreted.
 */
class QueryCompiler(parser: QueryParser, schema: Schema, phases: List[Phase]) {
  import IntrospectionLevel._

  /**
   * Compiles the GraphQL query `text` to a query algebra term which
   * can be directly executed.
   *
   * GraphQL errors and warnings are accumulated in the result.
   */
  def compile(text: String, name: Option[String] = None, untypedVars: Option[Json] = None, introspectionLevel: IntrospectionLevel = Full, reportUnused: Boolean = true, env: Env = Env.empty): Result[Operation] =
    parser.parseText(text).flatMap { case (ops, frags) =>
      for {
        _    <- Result.fromProblems(validateVariablesAndFragments(ops, frags, reportUnused))
        _    <- Result.fromProblems(validateFieldMergeability(ops, frags))
        ops0 <- ops.traverse(op => compileOperation(op, untypedVars, frags, introspectionLevel, env).map(op0 => (op.name, op0)))
        res  <- (ops0, name) match {
                  case (List((_, op)), None) =>
                    op.success
                  case (Nil, _) =>
                    Result.failure("At least one operation required")
                  case (_, None) =>
                    Result.failure("Operation name required to select unique operation")
                  case (ops, _) if ops.lengthCompare(1) > 0 && ops.exists(_._1.isEmpty) =>
                    Result.failure("Query shorthand cannot be combined with multiple operations")
                  case (ops, on@Some(name)) =>
                    ops.filter(_._1 == on) match {
                      case List((_, op)) =>
                        op.success
                      case Nil =>
                        Result.failure(s"No operation named '$name'")
                      case _ =>
                        Result.failure(s"Multiple operations named '$name'")
                    }
                }
      } yield res
    }

  /**
    * Compiles the provided operation AST to a query algebra term
    * which can be directly executed.
    *
    * GraphQL errors and warnings are accumulated in the result.
    */
  def compileOperation(op: UntypedOperation, untypedVars: Option[Json], frags: List[UntypedFragment], introspectionLevel: IntrospectionLevel = Full, env: Env = Env.empty): Result[Operation] = {
    val allPhases =
      IntrospectionElaborator(introspectionLevel).toList ++ (VariablesSkipAndFragmentElaborator :: MergeFields :: phases)

    for {
      varDefs <- compileVarDefs(op.variables)
      vars    <- compileVars(varDefs, untypedVars)
      _       <- Directive.validateDirectivesForQuery(schema, op, frags, vars)
      rootTpe <- op.rootTpe(schema)
      res     <- (
                   for {
                     query <- allPhases.foldLeftM(op.query) { (acc, phase) => phase.transformFragments *> phase.transform(acc) }
                   } yield Operation(query, rootTpe, op.directives)
                 ).runA(
                   ElabState(
                     None,
                     schema,
                     Context(rootTpe),
                     vars,
                     frags.map(f => (f.name, f)).toMap,
                     op.query,
                     env,
                     List.empty,
                     Elab.pure
                   )
                 )
    } yield res
  }

  /**
    * Compiles variable definition ASTs to variable definitions for the target schema.
    *
    * GraphQL errors and warnings are accumulated in the result.
    */
  def compileVarDefs(untypedVarDefs: UntypedVarDefs): Result[VarDefs] =
    untypedVarDefs.traverse {
      case UntypedVarDef(name, untypedTpe, default, dirs) =>
        compileType(untypedTpe).map(tpe => InputValue(name, None, tpe, default, dirs))
    }

  /**
    * Compiles raw query variables to variables for the target schema.
    *
    * GraphQL errors and warnings are accumulated in the result.
    */
  def compileVars(varDefs: VarDefs, untypedVars: Option[Json]): Result[Vars] =
    untypedVars match {
      case None => Map.empty.success
      case Some(untypedVars) =>
        untypedVars.asObject match {
          case None =>
            Result.failure(s"Variables must be represented as a Json object")
          case Some(obj) =>
            varDefs.traverse(iv => checkVarValue(iv, obj(iv.name), "variable values").map(v => (iv.name, (iv.tpe, v)))).map(_.toMap)
        }
    }

  /**
    * Compiles a type AST to a type in the target schema.
    *
    * GraphQL errors and warnings are accumulated in the result.
    */
  def compileType(tpe: Ast.Type): Result[Type] = {
    def loop(tpe: Ast.Type, nonNull: Boolean): Result[Type] = tpe match {
      case Ast.Type.NonNull(Left(named)) => loop(named, true)
      case Ast.Type.NonNull(Right(list)) => loop(list, true)
      case Ast.Type.List(elem) => loop(elem, false).map(e => if (nonNull) ListType(e) else NullableType(ListType(e)))
      case Ast.Type.Named(name) => schema.definition(name.value) match {
        case None => Result.failure(s"Undefined type '${name.value}'")
        case Some(tpe) => (if (nonNull) tpe else NullableType(tpe)).success
      }
    }
    loop(tpe, false)
  }

  def validateVariablesAndFragments(ops: List[UntypedOperation], frags: List[UntypedFragment], reportUnused: Boolean): List[Problem] = {
    val (uniqueFrags, duplicateFrags) = frags.map(_.name).foldLeft((Set.empty[String], Set.empty[String])) {
      case ((unique, duplicate), nme) =>
        if (unique.contains(nme)) (unique, duplicate + nme)
        else (unique + nme, duplicate)
    }

    if (duplicateFrags.nonEmpty)
      duplicateFrags.toList.map(nme => Problem(s"Fragment '$nme' is defined more than once"))
    else {
      def collectQueryRefs(query: Query): (Set[String], Set[String]) = {
        @tailrec
        def loop(queries: Iterator[Query], vars: Set[String], frags: Set[String]): (Set[String], Set[String]) =
          if (!queries.hasNext) (vars, frags)
          else
            queries.next() match {
              case UntypedSelect(_, _, args, dirs, child) =>
                val v0 = args.iterator.flatMap(arg => collectValueRefs(arg.value)).toSet
                val v1 = dirs.iterator.flatMap(dir => dir.args.iterator.flatMap(arg => collectValueRefs(arg.value))).toSet
                loop(Iterator.single(child) ++ queries, vars ++ v0 ++ v1, frags)
              case UntypedFragmentSpread(nme, dirs) =>
                val v0 = dirs.iterator.flatMap(dir => dir.args.iterator.flatMap(arg => collectValueRefs(arg.value))).toSet
                loop(queries, vars ++ v0, frags + nme)
              case UntypedInlineFragment(_, dirs, child) =>
                val v0 = dirs.iterator.flatMap(dir => dir.args.iterator.flatMap(arg => collectValueRefs(arg.value))).toSet
                loop(Iterator.single(child) ++ queries, vars ++ v0, frags)
              case Group(children) =>
                loop(children.iterator ++ queries, vars, frags)
              case Select(_, _, child)       => loop(Iterator.single(child) ++ queries, vars, frags)
              case Narrow(_, child)          => loop(Iterator.single(child) ++ queries, vars, frags)
              case Unique(child)             => loop(Iterator.single(child) ++ queries, vars, frags)
              case Filter(_, child)          => loop(Iterator.single(child) ++ queries, vars, frags)
              case Limit(_, child)           => loop(Iterator.single(child) ++ queries, vars, frags)
              case Offset(_, child)          => loop(Iterator.single(child) ++ queries, vars, frags)
              case OrderBy(_, child)         => loop(Iterator.single(child) ++ queries, vars, frags)
              case Introspect(_, child)      => loop(Iterator.single(child) ++ queries, vars, frags)
              case Environment(_, child)     => loop(Iterator.single(child) ++ queries, vars, frags)
              case Component(_, _, child)    => loop(Iterator.single(child) ++ queries, vars, frags)
              case Effect(_, child)          => loop(Iterator.single(child) ++ queries, vars, frags)
              case TransformCursor(_, child) => loop(Iterator.single(child) ++ queries, vars, frags)
              case Count(_)                  => loop(queries, vars, frags)
              case Empty                     => loop(queries, vars, frags)
            }

        loop(Iterator.single(query), Set.empty[String], Set.empty[String])
      }

      def collectValueRefs(value: Value): Set[String] = {
        @tailrec
        def loop(values: Iterator[Value], vars: Set[String]): Set[String] =
          if (!values.hasNext) vars
          else
            values.next() match {
              case VariableRef(nme) =>
                loop(values, Set(nme))
              case ObjectValue(fields) =>
                loop(fields.iterator.map(_._2) ++ values, vars)
              case ListValue(elems) =>
                loop(elems.iterator ++ values, vars)
              case _ => loop(values, vars)
            }

        loop(Iterator.single(value), Set.empty[String])
      }

      val fragRefs: Map[String, (Set[String], Set[String])] =
        frags.map { frag =>
          (frag.name, collectQueryRefs(frag.child))
        }.toMap

      @tailrec
      def checkCycle(pendingFrags: Set[String], seen: Set[String]): Option[Set[String]] = {
        if (pendingFrags.isEmpty) Some(seen)
        else {
          val hd = pendingFrags.head
          if (seen.contains(hd)) None
          else checkCycle(fragRefs.get(hd).map(_._2).getOrElse(Set.empty) ++ pendingFrags.tail, seen + hd)
        }
      }

      def findCycle: Option[String] = {
        @tailrec
        def loop(pendingFrags: Set[String]): Either[Set[String], String] = {
          if(pendingFrags.isEmpty) Left(Set.empty[String])
          else {
            val hd = pendingFrags.head
            checkCycle(Set(hd), Set.empty[String]) match {
              case None => Right(hd)
              case Some(seen) => loop(pendingFrags.tail.diff(seen))
            }
          }
        }

        if (uniqueFrags.isEmpty) None
        else loop(uniqueFrags).toOption
      }

      findCycle match {
        case Some(from) => List(Problem(s"Fragment cycle starting from '$from'"))
        case _ =>
          def validateOp(op: UntypedOperation, pendingFrags: Set[String]): (List[Problem], Set[String]) = {
            val pendingVars = op.variables.map(_.name).toSet
            val (dqv, dqf) = collectQueryRefs(op.query)

            @tailrec
            def closeRefs(pendingFrags: List[String], seenVars: Set[String], seenFrags: Set[String]): (Set[String], Set[String]) =
              pendingFrags match {
                case Nil => (seenVars, seenFrags)
                case hd :: tl =>
                  if (seenFrags.contains(hd)) closeRefs(tl, seenVars, seenFrags)
                  else {
                    fragRefs.get(hd) match {
                      case None =>
                        closeRefs(tl, seenVars, seenFrags + hd)
                      case Some((v, f)) =>
                        closeRefs(f.toList ::: tl, seenVars ++ v, seenFrags + hd)
                    }
                  }
              }

            val (qv, qf) = closeRefs(dqf.toList, dqv, Set.empty)

            val varProblems =
              if (qv == pendingVars) Nil
              else {
                val undefinedProblems =
                  qv.diff(pendingVars).toList.map(nme => Problem(s"Variable '$nme' is undefined"))

                val unusedProblems =
                  if (!reportUnused) Nil
                  else pendingVars.diff(qv).toList.map(nme => Problem(s"Variable '$nme' is unused"))

                undefinedProblems ++ unusedProblems
              }

            val fragProblems =
              if (qf.subsetOf(uniqueFrags)) Nil
              else {
                val undefined = qf.diff(uniqueFrags)
                val undefinedProblems = undefined.toList.map(nme => Problem(s"Fragment '$nme' is undefined"))
                undefinedProblems
              }

            (varProblems ++ fragProblems, pendingFrags.diff(qf))
          }

          val (opProblems, unreferencedFrags) =
            ops.foldLeft((List.empty[Problem], uniqueFrags)) {
              case ((acc, pendingFrags), op) =>
                val (problems, pendingFrags0) = validateOp(op, pendingFrags)
                (acc ++ problems, pendingFrags0)
              }

          val unreferencedFragProblems =
            if (!reportUnused) Nil
            else unreferencedFrags.toList.map(nme => Problem(s"Fragment '$nme' is unused"))

          opProblems ++ unreferencedFragProblems
      }
    }
  }

  /**
    * Validates that field mergeability rules are satisfied for the supplied operations and fragments.
    *
    * Returns a list of problems encountered.
    */
  def validateFieldMergeability(ops: List[UntypedOperation], frags: List[UntypedFragment]): List[Problem] = {
    // Validates field mergeability for a single operation
    def validateOp(op: UntypedOperation): List[Problem] = {
      // Collects all top level selects of the supplied queries, ungrouping and inlining fragments where necessary
      def collectSelects(queries: List[(NamedType, Query)]): List[(NamedType, UntypedSelect)] = {
        queries.flatMap {
          case (tpe, g: Group) => collectSelects(g.queries.map(q => (tpe, q)))
          case (tpe, s: UntypedSelect) => List((tpe, s))
          case (tpe, i: UntypedInlineFragment) =>
            (for {
              ntpe <- i.tpnme.map(nme => schema.definition(nme))
              sels <- Some(collectSelects(List((ntpe.getOrElse(tpe), i.child))))
            } yield sels).getOrElse(Nil) // Undefined types will be reported later
          case (_, f: UntypedFragmentSpread) =>
            (for {
              frag <- frags.find(_.name == f.name)
              ntpe <- schema.definition(frag.tpnme)
              sels <- Some(collectSelects(List((ntpe, frag.child))))
            } yield sels).getOrElse(Nil) // Undefined types will be reported later
          case _ =>
            Nil
        }
      }

      // Checks that the supplied field types are compatible
      def checkShapes(tpes: List[Type], resultName: String): Either[List[Problem], List[NamedType]] = {
        if(tpes.sizeCompare(1) <= 0) Right(tpes.map(_.underlyingNamed))
        else {
          def stripNull(tpes: List[Type]): Either[List[Problem], List[NamedType]] = {
            if (tpes.forall(!_.isNullable)) stripList(tpes)
            else if(tpes.forall(_.isNullable)) stripList(tpes.map(_.nonNull))
              else Left(List(Problem(s"Cannot merge fields named '$resultName' of both nullable and non-nullable types")))
          }

          def stripList(tpes: List[Type]): Either[List[Problem], List[NamedType]] = {
            if (tpes.forall(!_.isList)) Right(tpes.map(_.underlyingNamed))
            else if(tpes.forall(_.isList)) stripNull(tpes.collect { case ListType(elem) => elem })
              else Left(List(Problem(s"Cannot merge fields named '$resultName' of both list and non-list types")))
          }

          stripNull(tpes) match {
            case l@Left(_) => l
            case r@Right(tpes) =>
              if (tpes.forall(!_.isLeaf)) r
              else {
                val first = tpes.head
                if (tpes.tail.forall(_ =:= first)) r
                else {
                  val (leaf0, nonLeaf0) = tpes.partition(_.isLeaf)
                  val leafNames = leaf0.map(_.name).distinct
                  val nonLeafNames = nonLeaf0.map(_.name).distinct
                  val leafErrors =
                    if (leafNames.sizeCompare(1) <= 0) Nil
                    else List(Problem(s"Cannot merge fields named '$resultName' of distinct leaf types ${leafNames.mkString(", ")}"))
                  val nonLeafErrors =
                    if (nonLeafNames.isEmpty) Nil
                    else List(Problem(s"Cannot merge fields named '$resultName' of leaf types ${leafNames.mkString(", ")} and non-leaf types ${nonLeafNames.mkString(", ")}"))
                  Left(leafErrors ::: nonLeafErrors)
                }
              }
          }
        }
      }

      // Checks that the supplied queries are mergeable
      def validateQueries(queries: List[(NamedType, Query)]): List[Problem] = {
        val sels = collectSelects(queries)
        val grouped = sels.groupBy(_._2.resultName)
        val mergeProblems =
          if (sels.sizeCompare(1) <= 0) Nil
          else
            grouped.toList.flatMap {
              case (resultName, sels) =>
                if (sels.sizeCompare(1) <= 0) Nil
                else {
                  val allTypes = sels.map(_._1.dealias).distinct
                  allTypes.flatMap { tpe =>
                    val conflictSet = sels.collect { case (ntpe, sel) if ntpe <:< tpe || tpe <:< ntpe => sel }
                    if (conflictSet.sizeCompare(1) <= 0) Nil
                    else {
                      val first = conflictSet.head
                      val noNameConflicts = conflictSet.forall(_.name == first.name)
                      val noArgConflicts = conflictSet.forall(_.args == first.args)
                      if (noNameConflicts && noArgConflicts) Nil
                      else {
                        val nameProblems =
                          if (noNameConflicts) Nil
                          else List(Problem(s"Cannot merge fields with alias '$resultName' and names ${conflictSet.map(s => s"'${s.name}'").distinct.mkString(", ")}"))
                        val argProblems =
                          if (noArgConflicts) Nil
                          else List(Problem(s"Cannot merge fields named '$resultName' with different arguments"))
                        nameProblems ::: argProblems
                      }
                    }
                  }
                }
            }

        if (mergeProblems.nonEmpty) mergeProblems.toList.distinct
        else
          grouped.flatMap {
            case (resultName, sels) =>
              val children = sels.flatMap {
                case ((tpe, q)) =>
                  q.name match {
                    case "__typename" => Nil
                    case "__type"     => List((Introspection.__TypeType, q.child))
                    case "__schema"   => List((Introspection.__SchemaType, q.child))
                    case _ =>
                      (for {
                        ctpe <- tpe.field(q.name)
                      } yield (ctpe, q.child)).toList
                  } // Undefined fields and bogus subselection sets will be reported later
              }
              val (ctpes, cqs) = children.unzip
              checkShapes(ctpes, resultName) match {
                case Left(ps) => ps
                case Right(Nil) => Nil
                case Right(ctpes) if ctpes.head.isLeaf => Nil
                case Right(ctpes) => validateQueries(ctpes.zip(cqs))
              }
          }.toList
      }

      op.rootTpe(schema) match {
        case Result.Success(tpe)     => validateQueries(List((tpe, op.query)))
        case Result.Warning(ps, tpe) => ps.toList ++ validateQueries(List((tpe, op.query)))
        case Result.Failure(ps)      => ps.toList
        case Result.InternalError(_) => Nil // This will be reported elsewhere
      }
    }

    ops.flatMap(validateOp)
  }
}

object QueryCompiler {
  sealed trait IntrospectionLevel
  object IntrospectionLevel {
    case object Full extends IntrospectionLevel
    case object TypenameOnly extends IntrospectionLevel
    case object Disabled extends IntrospectionLevel
  }

  import IntrospectionLevel._

  /**
    * Elaboration monad.
    *
    * Supports threading of state through the elaboration of a query. Provides,
    * + access to the schema, context, variables and fragments of a query.
    * + ability to transform the children of Selects to supply semantics for field arguments.
    * + ability to add contextual data to the resulting query both to support propagation of
    *   context to the elaboration of children, and to to drive run time behaviour.
    * + ability to add selects for additional attributes to the resulting query.
    * + ability to test existence and properties of neighbour nodes of the node being
    *   elaborated.
    * + ability to report errors and warnings during elaboration.
    */
  type Elab[T] = StateT[Result, ElabState, T]
  object Elab {
    def unit: Elab[Unit] = StateT.pure(())
    def pure[T](t: T): Elab[T] = StateT.pure(t)
    def liftR[T](rt: Result[T]): Elab[T] = StateT.liftF(rt)

    /** The scheam of the query being elaborated */
    def schema: Elab[Schema] = StateT.inspect(_.schema)
    /** The context of the node currently being elaborated */
    def context: Elab[Context] = StateT.inspect(_.context)
    /** The variables of the query being elaborated */
    def vars: Elab[Vars] = StateT.inspect(_.vars)
    /** The fragments of the query being elaborated */
    def fragments: Elab[Map[String, UntypedFragment]] = StateT.inspect(_.fragments)
    /** The fragment with the supplied name, if defined, failing otherwise */
    def fragment(nme: String): Elab[UntypedFragment] =
      StateT.inspectF(_.fragments.get(nme).toResult(s"Fragment '$nme' is not defined"))
    def transformFragments(f: Map[String, UntypedFragment] => Elab[Map[String, UntypedFragment]]): Elab[Unit] =
      for {
        fs  <- fragments
        fs0 <- f(fs)
        _   <- StateT.modify(_.copy(fragments = fs0)): Elab[Unit]
      } yield ()
    /** `true` if the node currently being elaborated has a child with the supplied name */
    def hasField(name: String): Elab[Boolean] = StateT.inspect(_.hasField(name))
    /** The alias, if any, of the child with the supplied name */
    def fieldAlias(name: String): Elab[Option[String]] = StateT.inspect(_.fieldAlias(name))
    /** `true` if the node currently being elaborated has a sibling with the supplied name */
    def hasSibling(name: String): Elab[Boolean] = StateT.inspect(_.hasSibling(name))
    /** The result name of the node currently being elaborated */
    def resultName: Elab[Option[String]] = StateT.inspect(_.resultName)

    /** Binds the supplied value to the supplied name in the elaboration environment */
    def env(nme: String, value: Any): Elab[Unit] = env(List(nme -> value))
    /** Binds the supplied names and values in the elaboration environment */
    def env(kv: (String, Any), kvs: (String, Any)*): Elab[Unit] = env(kv +: kvs.toSeq)
    /** Binds the supplied names and values in the elaboration environment */
    def env(kvs: Seq[(String, Any)]): Elab[Unit] = StateT.modify(_.env(kvs))
    /** Adds all the bindings of the supplied environment to the elaboration environment */
    def env(other: Env): Elab[Unit] = StateT.modify(_.env(other))
    /** The value bound to the supplied name in the elaboration environment, if any */
    def env[T: ClassTag](nme: String): Elab[Option[T]] = StateT.inspect(_.env[T](nme))
    /** The value bound to the supplied name in the elaboration environment, if any, failing otherwise */
    def envE[T: ClassTag: TypeName](nme: String): Elab[T] =
      env(nme).flatMap(v => Elab.liftR(v.toResultOrError(s"Key '$nme' of type ${typeName[T]} was not found in $this")))
    /** The subset of the elaboration environment defined directly at this node */
    def localEnv: Elab[Env] = StateT.inspect(_.localEnv)

    /** Applies the supplied transformation to the child of the node currently being elaborated */
    def transformChild(f: Query => Elab[Query]): Elab[Unit] = StateT.modify(_.addChildTransform(f))
    /** Applies the supplied transformation to the child of the node currently being elaborated */
    def transformChild(f: Query => Query)(implicit dummy: DummyImplicit): Elab[Unit] = transformChild(q => Elab.pure(f(q)))
    /** Applies the supplied transformation to the child of the node currently being elaborated */
    def transformChild(f: Query => Result[Query])(implicit dummy1: DummyImplicit, dummy2: DummyImplicit): Elab[Unit] = transformChild(q => Elab.liftR(f(q)))
    /** The transformation to be applied to the child of the node currently being elaborated */
    def transform: Elab[Query => Elab[Query]] = StateT.inspect(_.childTransform)
    /** Add the supplied attributed and corresponding query, if any, to the query being elaborated */
    def addAttribute(name: String, query: Query = Empty): Elab[Unit] = StateT.modify(_.addAttribute(name, query))
    /** The attributes which have been added to the query being elaborated */
    def attributes: Elab[List[(String, Query)]] = StateT.inspect(_.attributes)

    /** Report the supplied GraphQL warning during elaboration */
    def warning(msg: String): Elab[Unit] = StateT(s => Result.warning[(ElabState, Unit)](msg, (s, ())))
    /** Report the supplied GraphQL warning during elaboration */
    def warning(err: Problem): Elab[Unit] = StateT(s => Result.warning[(ElabState, Unit)](err, (s, ())))
    /** Report the supplied GraphQL error during elaboration */
    def failure[T](msg: String): Elab[T] = StateT(_ => Result.failure[(ElabState, T)](msg))
    /** Report the supplied GraphQL error during elaboration */
    def failure[T](err: Problem): Elab[T] = StateT(_ => Result.failure[(ElabState, T)](err))
    /** Report the supplied internal error during elaboration */
    def internalError[T](msg: String): Elab[T] = StateT(_ => Result.internalError[(ElabState, T)](msg))
    /** Report the supplied internal error during elaboration */
    def internalError[T](err: Throwable): Elab[T] = StateT(_ => Result.internalError[(ElabState, T)](err))

    /** Save the current elaboration state */
    def push: Elab[Unit] = StateT.modify(_.push)
    /** Save the current elaboration state and switch to the supplied context and query */
    def push(context: Context, query: Query): Elab[Unit] = StateT.modify(_.push(context, query))
    /** Save the current elaboration state and switch to the supplied schema, context and query */
    def push(schema: Schema, context: Context, query: Query): Elab[Unit] = StateT.modify(_.push(schema, context, query))
    /** Restore the previous elaboration state */
    def pop: Elab[Unit] = StateT.modifyF(s => s.parent.toResultOrError("Cannot pop root state"))
  }

  /**
    * The state managed by the elaboration monad.
    */
  case class ElabState(
    parent: Option[ElabState],
    schema: Schema,
    context: Context,
    vars: Vars,
    fragments: Map[String, UntypedFragment],
    query: Query,
    localEnv: Env,
    attributes: List[(String, Query)],
    childTransform: Query => Elab[Query]
  ) {
    def hasField(fieldName: String): Boolean = Query.hasField(query, fieldName)
    def fieldAlias(fieldName: String): Option[String] = Query.fieldAlias(query, fieldName)
    def hasSibling(fieldName: String): Boolean = parent.exists(s => Query.hasField(s.query, fieldName))
    def resultName: Option[String] = Query.ungroup(query).headOption.flatMap(Query.resultName)
    def env(kvs: Seq[(String, Any)]): ElabState = copy(localEnv = localEnv.add(kvs: _*))
    def env(other: Env): ElabState = copy(localEnv = localEnv.add(other))
    def env[T: ClassTag](nme: String): Option[T] = localEnv.get(nme).orElse(parent.flatMap(_.env(nme)))
    def addAttribute(name: String, query: Query = Empty): ElabState = copy(attributes = (name, query) :: attributes)
    def addChildTransform(f: Query => Elab[Query]): ElabState = copy(childTransform = childTransform.andThen(_.flatMap(f)))
    def push: ElabState = copy(parent = Some(this), localEnv = Env.empty, attributes = Nil, childTransform = Elab.pure)
    def push(context: Context, query: Query): ElabState =
      copy(parent = Some(this), context = context, query = query, localEnv = Env.empty, attributes = Nil, childTransform = Elab.pure)
    def push(schema: Schema, context: Context, query: Query): ElabState =
      copy(parent = Some(this), schema = schema, context = context, query = query, localEnv = Env.empty, attributes = Nil, childTransform = Elab.pure)
  }

  /** A QueryCompiler phase. */
  trait Phase {
    def transformFragments: Elab[Unit] = Elab.unit

    /**
     * Transform the supplied query algebra term `query`.
     */
    def transform(query: Query): Elab[Query] =
      query match {
        case s@UntypedSelect(fieldName, alias, _, _, child) =>
          transformSelect(fieldName, alias, child).map(ec => s.copy(child = ec))

        case s@Select(fieldName, alias, child) =>
          transformSelect(fieldName, alias, child).map(ec => s.copy(child = ec))

        case n@Narrow(subtpe, child)  =>
          for {
            c  <- Elab.context
            _  <- Elab.push(c.asType(subtpe), child)
            ec <- transform(child)
            _  <- Elab.pop
          } yield n.copy(child = ec)

        case f@UntypedFragmentSpread(_, _) => Elab.pure(f)
        case i@UntypedInlineFragment(None, _, child) =>
          transform(child).map(ec => i.copy(child = ec))
        case i@UntypedInlineFragment(Some(tpnme), _, child) =>
          for {
            s      <- Elab.schema
            c      <- Elab.context
            subtpe <- Elab.liftR(Result.fromOption(
                        s.definition(tpnme).orElse(Introspection.schema.definition(tpnme)),
                        s"Unknown type '$tpnme' in type condition of inline fragment"
                      ))
            _      <- Elab.push(c.asType(subtpe), child)
            ec     <- transform(child)
            _      <- Elab.pop
          } yield i.copy(child = ec)

        case i@Introspect(_, child) =>
          for {
            s    <- Elab.schema
            c    <- Elab.context
            iTpe =  if(c.tpe =:= s.queryType) Introspection.schema.queryType else TypenameType
            _    <- Elab.push(Introspection.schema, c.asType(iTpe), child)
            ec   <- transform(child)
            _    <- Elab.pop
          } yield i.copy(child = ec)

        case u@Unique(child) =>
          for {
            c  <- Elab.context
            _  <- Elab.push(c.asType(c.tpe.nonNull.list), child)
            ec <- transform(child)
            _  <- Elab.pop
          } yield u.copy(child = ec)

        case f@Filter(_, child) =>
          for {
            c    <- Elab.context
            item <- Elab.liftR(c.tpe.item.toResultOrError(s"Filter of non-List type ${c.tpe}"))
            _    <- Elab.push(c.asType(item), child)
            ec   <- transform(child)
            _    <- Elab.pop
          } yield f.copy(child = ec)

        case n@Count(child) =>
          for {
            c  <- Elab.context
            pc <- Elab.liftR(c.parent.toResultOrError(s"Count node has no parent"))
            _  <- Elab.push(pc, child)
            ec <- transform(child)
            _  <- Elab.pop
          } yield n.copy(child = ec)

        case g@Group(children) =>
          children.traverse { c =>
            for {
              _  <- Elab.push
              tc <- transform(c)
              _  <- Elab.pop
            } yield tc
          }.map(eqs => g.copy(queries = eqs))

        case c@Component(_, _, child) => transform(child).map(ec => c.copy(child = ec))
        case e@Effect(_, child)       => transform(child).map(ec => e.copy(child = ec))
        case l@Limit(_, child)        => transform(child).map(ec => l.copy(child = ec))
        case o@Offset(_, child)       => transform(child).map(ec => o.copy(child = ec))
        case o@OrderBy(_, child)      => transform(child).map(ec => o.copy(child = ec))
        case e@Environment(_, child)  => transform(child).map(ec => e.copy(child = ec))
        case t@TransformCursor(_, child) => transform(child).map(ec => t.copy(child = ec))
        case Empty                    => Elab.pure(Empty)
      }

    def transformSelect(fieldName: String, alias: Option[String], child: Query): Elab[Query] =
      for {
        c        <- Elab.context
        _        <- validateSubselection(fieldName, child)
        childCtx <- Elab.liftR(c.forField(fieldName, alias))
        _        <- Elab.push(childCtx, child)
        ec       <- transform(child)
        _        <- Elab.pop
      } yield ec

    def validateSubselection(fieldName: String, child: Query): Elab[Unit] =
      for {
        c        <- Elab.context
        childCtx <- Elab.liftR(c.forField(fieldName, None))
        tpe      =  childCtx.tpe
        _        <- {
                      val isLeaf = tpe.isUnderlyingLeaf
                      def obj =  c.tpe.underlyingNamed
                      if (isLeaf && child != Empty)
                        Elab.failure(s"Leaf field '$fieldName' of $obj must have an empty subselection set")
                      else if (!isLeaf && child == Empty)
                        Elab.failure(s"Non-leaf field '$fieldName' of $obj must have a non-empty subselection set")
                      else
                        Elab.pure(())
                    }
      } yield ()

    val TypenameType = ObjectType(s"__Typename", None, List(Field("__typename", None, Nil, StringType, Nil)), Nil, Nil)
  }

  /**
   * A phase which elaborates GraphQL introspection queries into the query algrebra.
   */
  class IntrospectionElaborator(level: IntrospectionLevel) extends Phase {
    override def transformFragments: Elab[Unit] =
      Elab.transformFragments { fs =>
        fs.toList.traverse {
          case (nme, f@UntypedFragment(_, tpnme, _, child)) =>
            for {
              s   <- Elab.schema
              c   <- Elab.context
              tpe <- Elab.liftR(Result.fromOption(s.definition(tpnme).orElse(Introspection.schema.definition(tpnme)), s"Unknown type '$tpnme' in fragment definition"))
              _   <- Elab.push(c.asType(tpe), child)
              ec  <- transform(child)
              _   <- Elab.pop
           } yield (nme, f.copy(child = ec))
        }.map(_.toMap)
      }

    override def transform(query: Query): Elab[Query] =
      query match {
        case s@UntypedSelect(fieldName @ ("__typename" | "__schema" | "__type"), _, _, _, _) =>
          (fieldName, level) match {
            case ("__typename", Disabled) =>
              Elab.failure("Introspection is disabled")
            case ("__schema" | "__type", TypenameOnly | Disabled) =>
              Elab.failure("Introspection is disabled")
            case _ =>
              for {
                schema <- Elab.schema
              } yield Introspect(schema, s)
          }
        case _ => super.transform(query)
      }
  }

  object IntrospectionElaborator {
    def apply(level: IntrospectionLevel): Option[IntrospectionElaborator] =
      level match {
        case Disabled => None
        case other => Some(new IntrospectionElaborator(other))
      }
  }

  /**
   * A phase which elaborates variables, directives, fragment spreads
   * and inline fragments.
   *
   * 1. Query variable values are substituted for all variable
   *    references.
   *
   * 2. `skip` and `include` directives are handled during this phase
   *    and the guarded subqueries are retained or removed as
   *    appropriate.
   *
   * 3. Fragment spread and inline fragments are expanded.
   *
   * 4. types narrowing coercions by resolving the target type
   *    against the schema.
   *
   * 5. verifies that leaves have an empty subselection set and that
   *    structured types have a non-empty subselection set.
   */
  object VariablesSkipAndFragmentElaborator extends Phase {
    override def transform(query: Query): Elab[Query] =
      query match {
        case sel@UntypedSelect(fieldName, alias, args, dirs, child) =>
          isSkipped(dirs).ifM(
            Elab.pure(Empty),
            for {
              _        <- validateSubselection(fieldName, child)
              s        <- Elab.schema
              c        <- Elab.context
              childCtx <- Elab.liftR(c.forField(fieldName, alias))
              vars     <- Elab.vars
              eArgs    <- args.traverse(elaborateBinding(_, vars))
              eDirs    <- Elab.liftR(Directive.elaborateDirectives(s, dirs.filterNot(dir => dir.name == "skip" || dir.name == "include"), vars))
              _        <- Elab.push(childCtx, child)
              ec       <- transform(child)
              _        <- Elab.pop
            } yield sel.copy(args = eArgs, directives = eDirs, child = ec)
          )

        case UntypedFragmentSpread(nme, dirs) =>
          isSkipped(dirs).ifM(
            Elab.pure(Empty),
            for {
              s      <- Elab.schema
              c      <- Elab.context
              f      <- Elab.fragment(nme)
              ctpe   =  c.tpe.underlyingNamed
              subtpe <- Elab.liftR(s.definition(f.tpnme).toResult(s"Unknown type '${f.tpnme}' in type condition of fragment '$nme'"))
              _      <- Elab.failure(s"Fragment '$nme' is not compatible with type '${c.tpe}'").whenA(!fragmentApplies(s, subtpe, ctpe))
              _      <- Elab.push(c.asType(subtpe), f.child)
              ec     <- transform(f.child)
              _      <- Elab.pop
            } yield
              if (ctpe <:< subtpe) ec
              else Narrow(s.uncheckedRef(subtpe), ec)
          )

        case UntypedInlineFragment(tpnme0, dirs, child) =>
          isSkipped(dirs).ifM(
            Elab.pure(Empty),
            for {
              s      <- Elab.schema
              c      <- Elab.context
              ctpe   =  c.tpe.underlyingNamed
              subtpe <- tpnme0 match {
                          case None =>
                            Elab.pure(ctpe)
                          case Some(tpnme) =>
                            Elab.liftR(s.definition(tpnme).toResult(s"Unknown type '$tpnme' in type condition of inline fragment"))
                        }
              _      <- Elab.failure(s"Inline fragment with type condition '$subtpe' is not compatible with type '$ctpe'").whenA(!fragmentApplies(s, subtpe, ctpe))
              _      <- Elab.push(c.asType(subtpe), child)
              ec     <- transform(child)
              _      <- Elab.pop
            } yield
              if (ctpe <:< subtpe) ec
              else Narrow(s.uncheckedRef(subtpe), ec)
          )

        case _ => super.transform(query)
      }

    /**
     * Tests the supplied type condition is satisfied by the supplied context type
     * https://spec.graphql.org/October2021/#sec-Fragment-spread-is-possible
     */
    def fragmentApplies(schema: Schema, typeCond: NamedType, ctpe: NamedType): Boolean = {
      val typeCondPoss = schema.subtypes(typeCond.dealias)
      val ctpePoss = schema.subtypes(ctpe.dealias)
      typeCondPoss.exists(x => ctpePoss.exists(y => x =:= y))
    }

    def elaborateBinding(b: Binding, vars: Vars): Elab[Binding] =
      Elab.liftR(Value.elaborateValue(b.value, vars).map(ev => b.copy(value = ev)))

    def isSkipped(dirs: List[Directive]): Elab[Boolean] =
      dirs.filter(d => d.name == "skip" || d.name == "include") match {
        case Nil => Elab.pure(false)
        case List(Directive(nme, List(Binding("if", value)))) =>
          for {
            c <- extractCond(value)
          } yield (nme == "skip" && c) || (nme == "include" && !c)
        case List(Directive(nme, _)) => Elab.failure(s"Directive '$nme' must have a single Boolean 'if' argument")
        case _ => Elab.failure("skip/include directives must be unique")
      }

    def extractCond(value: Value): Elab[Boolean] =
      value match {
        case VariableRef(varName) =>
          for {
            v  <- Elab.vars
            tv <- Elab.liftR(Result.fromOption(v.get(varName), s"Variable '$varName' is undefined"))
            b  <- tv match {
                    case (tpe, BooleanValue(value)) if tpe.nonNull =:= BooleanType => Elab.pure(value)
                    case _ => Elab.failure(s"Argument of skip/include must be boolean")
                  }
          } yield b
        case BooleanValue(value) => Elab.pure(value)
        case _ => Elab.failure(s"Argument of skip/include must be boolean")
      }
  }

  /**
    * A compiler phase which applies GraphQL field merge rules to an
    * untyped query.
    */
  object MergeFields extends Phase {
    override def transform(query: Query): Elab[Query] =
      Elab.pure(mergeUntypedQueries(List(query)))
  }

  /**
   * A compiler phase which translates `Select` nodes to be directly
   * interpretable.
   *
   * This phase,
   *
   * 1. types bindings according to the schema:
   *    i)   untyped enums are validated and typed according to their
   *         declared type.
   *    ii)  String and Int bindings are translated to ID bindings
   *         where appropriate.
   *    iii) default values are supplied for missing arguments.
   *    iv)  arguments are permuted into the order declared in the
   *         schema.
   *
   * 2. eliminates Select arguments by delegating to a model-specific
   *    `PartialFunction` which is responsible for translating `Select`
   *    nodes into a form which is directly interpretable, for example,
   *    replacing them with a `Filter` or `Unique` node with a
   *    `Predicate` which is parameterized by the arguments, ie.,
   *
   *    ```
   *    UntypedSelect("character", None, List(IDBinding("id", "1000")), Nil, child)
   *    ```
   *    might be translated to,
   *    ```
   *    Select("character, None, Filter(FieldEquals("id", "1000"), child))
   *    ```
   * 3. GraphQL introspection query field arguments are elaborated.
   */
  trait SelectElaborator extends Phase {
    override def transform(query: Query): Elab[Query] =
      query match {
        case sel@UntypedSelect(fieldName, resultName, args, dirs, child) =>
          for {
            c        <- Elab.context
            s        <- Elab.schema
            childCtx <- Elab.liftR(c.forField(fieldName, resultName))
            obj      =  c.tpe.underlyingNamed.dealias
            field    <- obj match {
                          case twf: TypeWithFields =>
                            Elab.liftR(twf.fieldInfo(fieldName).toResult(s"No field '$fieldName' for type ${obj.underlying}"))
                          case _ => Elab.failure(s"Type $obj is not an object or interface type")
                        }
            eArgs    <- Elab.liftR(elaborateFieldArgs(obj, field, args))
            _        <- if (s eq Introspection.schema) elaborateIntrospection(Introspection.schema.uncheckedRef(obj), fieldName, eArgs)
                        else select(s.uncheckedRef(obj), fieldName, eArgs, dirs)
            elab     <- Elab.transform
            env      <- Elab.localEnv
            attrs    <- Elab.attributes
            _        <- Elab.push(childCtx, child)
            ec       <- transform(child)
            _        <- Elab.pop
            e2       <- elab(ec)
          } yield {
            val e1 = Select(sel.name, sel.alias, e2)
            val e0 =
              if(attrs.isEmpty) e1
              else mergeQueries(e1 :: attrs.map { case (nme, child) => Select(nme, child) })

            if (env.isEmpty) e0
            else Environment(env, e0)
          }

        case _ => super.transform(query)
      }

    def select(ref: TypeRef, name: String, args: List[Binding], directives: List[Directive]): Elab[Unit]

    val QueryTypeRef = Introspection.QueryType
    val TypeTypeRef = Introspection.__TypeType
    val FieldTypeRef = Introspection.__FieldType
    val EnumValueTypeRef = Introspection.__EnumValueType

    def elaborateIntrospection(ref: TypeRef, name: String, args: List[Binding]): Elab[Unit] =
      (ref, name, args) match {
        case (QueryTypeRef, "__type", List(Binding("name", StringValue(name)))) =>
          Elab.transformChild(child => Unique(Filter(Eql(TypeTypeRef / "name", Const(Option(name))), child)))

        case (TypeTypeRef, "fields", List(Binding("includeDeprecated", BooleanValue(include)))) =>
          Elab.transformChild(child => if (include) child else Filter(Eql(FieldTypeRef / "isDeprecated", Const(false)), child))
        case (TypeTypeRef, "enumValues", List(Binding("includeDeprecated", BooleanValue(include)))) =>
          Elab.transformChild(child => if (include) child else Filter(Eql(EnumValueTypeRef / "isDeprecated", Const(false)), child))
        case (TypeTypeRef, "inputFields", List(Binding("includeDeprecated", BooleanValue(include)))) =>
          Elab.transformChild(child => if (include) child else Filter(Eql(Introspection.__InputValueType / "isDeprecated", Const(false)), child))
        case (FieldTypeRef, "args", List(Binding("includeDeprecated", BooleanValue(include)))) =>
          Elab.transformChild(child => if (include) child else Filter(Eql(Introspection.__InputValueType / "isDeprecated", Const(false)), child))
        case (Introspection.__DirectiveType, "args", List(Binding("includeDeprecated", BooleanValue(include)))) =>
          Elab.transformChild(child => if (include) child else Filter(Eql(Introspection.__InputValueType / "isDeprecated", Const(false)), child))
        case _ =>
          Elab.unit
      }

    def elaborateFieldArgs(tpe: NamedType, field: Field, args: List[Binding]): Result[List[Binding]] = {
      val infos = field.args
      val unknownArgs = args.filterNot(arg => infos.exists(_.name == arg.name))
      if (unknownArgs.nonEmpty)
        Result.failure(s"Unknown argument(s) ${unknownArgs.map(s => s"'${s.name}'").mkString("", ", ", "")} in field ${field.name} of type ${tpe.name}")
      else {
        val argMap = args.groupMapReduce(_.name)(_.value)((x, _) => x)
        infos.traverse(info => checkValue(info, argMap.get(info.name), s"field '${field.name}' of type '$tpe'").map(v => Binding(info.name, v)))
      }
    }
  }

  object SelectElaborator {
    /**
     * Construct a `SelectElaborator` given a partial function which is called for each
     * Select` node in the query.
     */
    def apply(sel: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]]): SelectElaborator =
      new SelectElaborator {
        def select(ref: TypeRef, name: String, args: List[Binding], directives: List[Directive]): Elab[Unit] =
          if(sel.isDefinedAt((ref, name, args))) sel((ref, name, args))
          else Elab.unit
      }

    /** A select elaborator which discards all field arguments */
    def identity: SelectElaborator = SelectElaborator(_ => Elab.unit)
  }

  /**
   * A compiler phase which partitions a query for execution by multiple
   * composed mappings.
   *
   * This phase transforms the input query by assigning subtrees to component
   * mappings as specified by the supplied `cmapping`.
   *
   * The mapping has `Type` and field name pairs as keys and mapping and
   * join function pairs as values. When the traversal of the input query
   * visits a `Select` node with type `Type.field name` it will replace the
   * `Select` with a `Component` node comprising,
   *
   * 1. the mapping which will be responsible for evaluating the subquery.
   * 2. A join function which will be called during interpretation with,
   *
   *    i) The deferred subquery.
   *    ii)  the cursor at that point in evaluation.
   *
   *    This join function is responsible for computing the continuation
   *    query which will be evaluated by the responsible interpreter.
   *
   *    Because the join is provided with the cursor of the parent
   *    interpreter the subquery can be parameterised with values derived
   *    from the parent query.
   */
  class ComponentElaborator[F[_]] private (cmapping: Map[(Type, String), (Mapping[F], (Query, Cursor) => Result[Query])]) extends Phase {
    override def transform(query: Query): Elab[Query] =
      query match {
        case s@Select(fieldName, resultName, child) =>
          for {
            c        <- Elab.context
            obj      <- Elab.liftR(c.tpe.underlyingObject.toResultOrError(s"Type ${c.tpe} is not an object or interface type"))
            childCtx =  c.forFieldOrAttribute(fieldName, resultName)
            _        <- Elab.push(childCtx, child)
            ec       <- transform(child)
            _        <- Elab.pop
            schema   <- Elab.schema
            ref      =  schema.uncheckedRef(obj)
          } yield
            cmapping.get((ref, fieldName)) match {
              case Some((component, join)) =>
                Component(component, join, s.copy(child = ec))
              case None =>
                s.copy(child = ec)
            }

        case _ => super.transform(query)
      }
  }

  object ComponentElaborator {
    val TrivialJoin = (q: Query, _: Cursor) => q.success

    case class ComponentMapping[F[_]](tpe: TypeRef, fieldName: String, mapping: Mapping[F], join: (Query, Cursor) => Result[Query] = TrivialJoin)

    def apply[F[_]](mappings: Seq[ComponentMapping[F]]): ComponentElaborator[F] =
      new ComponentElaborator(mappings.map(m => ((m.tpe, m.fieldName), (m.mapping, m.join))).toMap)
  }

  /**
   * A compiler phase which partitions a query for execution which may invoke
   * multiple effect handlers.
   *
   * This phase transforms the input query by assigning subtrees to effect
   * handlers as specified by the supplied `effects`, which takes a `Context` and
   * fieldName, retuning an `EffectHandler` (if any).
   *
   * The mapping has `Type` and field name pairs as keys and effect handlers
   * as values. When the traversal of the input query visits a `Select` node
   * with type `Type.field name` it will replace the
   * `Select` with an `Effect` node comprising,
   *
   * 1. the effect handler which will be responsible for running the effect
   *    and evaluating the subquery against its result.
   * 2. the subquery which will be evaluated by the effect handler.
   */
  class EffectElaborator[F[_]] private (effects: (Context, String) => Option[EffectHandler[F]]) extends Phase {
    override def transform(query: Query): Elab[Query] =
      query match {
        case s@Select(fieldName, resultName, child) =>
          for {
            c        <- Elab.context
            childCtx =  c.forFieldOrAttribute(fieldName, resultName)
            _        <- Elab.push(childCtx, child)
            ec       <- transform(child)
            _        <- Elab.pop
          } yield
            effects(c, fieldName) match {
              case Some(handler) =>
                Select(fieldName, resultName, Effect(handler, s.copy(child = ec)))
              case None =>
                s.copy(child = ec)
            }

        case _ => super.transform(query)
      }
  }

  object EffectElaborator {
    case class EffectMapping[F[_]](tpe: TypeRef, fieldName: String, handler: EffectHandler[F])

    def apply[F[_]](effects: (Context, String) => Option[EffectHandler[F]]): EffectElaborator[F] =
      new EffectElaborator(effects)

  }

  /**
    * A compiler phase which estimates the size of a query and applies width
    * and depth limits.
    */
  class QuerySizeValidator(maxDepth: Int, maxWidth: Int) extends Phase {
    override def transform(query: Query): Elab[Query] =
      Elab.fragments.flatMap { frags =>
        querySize(query, frags) match {
          case (depth, _) if depth > maxDepth => Elab.failure(s"Query is too deep: depth is $depth levels, maximum is $maxDepth")
          case (_, width) if width > maxWidth => Elab.failure(s"Query is too wide: width is $width leaves, maximum is $maxWidth")
          case (depth, width) if depth > maxDepth && width > maxWidth => Elab.failure(s"Query is too complex: width/depth is $width/$depth leaves/levels, maximum is $maxWidth/$maxDepth")
          case (_, _) => Elab.pure(query)
        }
      }

    def querySize(query: Query, frags: Map[String, UntypedFragment]): (Int, Int) = {
      def handleGroup(g: Group, depth: Int, width: Int): (Int, Int) = {
        val dws = Query.ungroup(g).map(loop(_, depth, width))
        val (depths, widths) = dws.unzip
        (depths.max, widths.sum)
      }

      @tailrec
      def loop(q: Query, depth: Int, width: Int): (Int, Int) =
        q match {
          case UntypedSelect(_, _, _, _, Empty) => (depth + 1, width + 1)
          case Select(_, _, Empty) => (depth + 1, width + 1)
          case Count(_) => (depth + 1, width + 1)
          case UntypedSelect(_, _, _, _, child) => loop(child, depth + 1, width)
          case Select(_, _, child) => loop(child, depth + 1, width)
          case g: Group => handleGroup(g, depth, width)
          case Component(_, _, child) => loop(child, depth, width)
          case Effect(_, child) => loop(child, depth, width)
          case Environment(_, child) => loop(child, depth, width)
          case Empty => (depth, width)
          case Filter(_, child) => loop(child, depth, width)
          case Introspect(_, _) => (depth, width)
          case Limit(_, child) => loop(child, depth, width)
          case Offset(_, child) => loop(child, depth, width)
          case Narrow(_, child) => loop(child, depth, width)
          case OrderBy(_, child) => loop(child, depth, width)
          case TransformCursor(_, child) => loop(child, depth, width)
          case Unique(child) => loop(child, depth, width)
          case UntypedFragmentSpread(nme, _) =>
            frags.get(nme) match {
              case Some(frag) => loop(frag.child, depth, width)
              case None => (depth, width)
            }
          case UntypedInlineFragment(_, _, child) => loop(child, depth, width)
        }

      loop(query, 0, 0)
    }
  }
}
