// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import atto.Atto._
import cats.data.{ Ior, NonEmptyChain }
import cats.implicits._
import io.circe.Json

import Query._, Value._
import QueryCompiler._
import QueryInterpreter.{ mkError, mkOneError, mkErrorResult }
import Ast.{ Type => _, Value => _, _ }, OperationDefinition._, OperationType._, Selection._
import ScalarType._

/**
 * GraphQL query parser
 */
object QueryParser {
  /**
   *  Parse a query String to a query algebra term.
   *
   *  Yields a Query value on the right and accumulates errors on the left.
   */
  def parseText(text: String, name: Option[String] = None): Result[(Query, UntypedVarDefs)] = {
    def toResult[T](pr: Either[String, T]): Result[T] =
      Ior.fromEither(pr).leftMap(msg => NonEmptyChain.one(mkError(msg)))

    for {
      doc   <- toResult(Parser.Document.parseOnly(text).either)
      query <- parseDocument(doc, name)
    } yield query
  }

  def parseDocument(doc: Document, name: Option[String]): Result[(Query, UntypedVarDefs)] = {
    val ops = doc.collect { case Left(op) => op }
    val fragments = doc.collect { case Right(frag) => (frag.name.value, frag) }.toMap

    (ops, name) match {
      case (List(op: Operation), None) => parseOperation(op, fragments)
      case (List(qs: QueryShorthand), None) => parseQueryShorthand(qs, fragments)
      case (_, None) =>
        mkErrorResult("Operation name required to select unique operation")
      case (ops, _) if ops.exists { case _: QueryShorthand => true ; case _ => false } =>
        mkErrorResult("Query shorthand cannot be combined with multiple operations")
      case (ops, Some(name)) =>
        ops.filter { case Operation(_, Some(Name(`name`)), _, _, _) => true ; case _ => false } match {
          case List(op: Operation) => parseOperation(op, fragments)
          case Nil =>
            mkErrorResult(s"No operation named '$name'")
          case _ =>
            mkErrorResult(s"Multiple operations named '$name'")
        }
    }
  }

  def parseOperation(op: Operation, fragments: Map[String, FragmentDefinition]): Result[(Query, UntypedVarDefs)] = op match {
    case Operation(Query, _, vds, _, sels) =>
      val q = parseSelections(sels, None, fragments)
      val vs = vds.map {
        case VariableDefinition(nme, tpe, _, _) => UntypedVarDef(nme.value, tpe, None)
      }
      q.map(q => (q, vs))
    case _ => mkErrorResult("Selection required")
  }

  def parseQueryShorthand(qs: QueryShorthand, fragments: Map[String, FragmentDefinition]): Result[(Query, UntypedVarDefs)] = qs match {
    case QueryShorthand(sels) => parseSelections(sels, None, fragments).map(q => (q, Nil))
    case _ => mkErrorResult("Selection required")
  }

  def parseSelections(sels: List[Selection], typeCondition: Option[String], fragments: Map[String, FragmentDefinition]): Result[Query] =
    sels.traverse(parseSelection(_, typeCondition, fragments)).map { sels0 =>
      if (sels0.size == 1) sels0.head else Group(sels0)
    }

  def parseSelection(sel: Selection, typeCondition: Option[String], fragments: Map[String, FragmentDefinition]): Result[Query] = sel match {
    case Field(alias, name, args, _, sels) =>
      for {
        args0 <- parseArgs(args)
        sels0 <- parseSelections(sels, None, fragments)
      } yield {
        val sel0 =
          if (sels.isEmpty) Select(name.value, args0, Empty)
          else Select(name.value, args0, sels0)
        val sel = alias match {
          case Some(Name(nme)) => Rename(nme, sel0)
          case None => sel0
        }
        typeCondition match {
          case Some(tpnme) => UntypedNarrow(tpnme, sel)
          case _ => sel
        }
      }
    case FragmentSpread(Name(name), _) =>
      fragments.get(name) match {
        case Some(frag) => parseSelections(frag.selectionSet, Some(frag.typeCondition.name), fragments)
        case None => mkErrorResult(s"Undefined fragment '$name'")
      }
    case InlineFragment(Some(Ast.Type.Named(Name(tpnme))), _, sels) =>
      parseSelections(sels, Some(tpnme), fragments)

    case _ =>
      mkErrorResult("Field or fragment spread required")
  }

  def parseArgs(args: List[(Name, Ast.Value)]): Result[List[Binding]] =
    args.traverse((parseArg _).tupled)

  def parseArg(name: Name, value: Ast.Value): Result[Binding] = {
    value match {
      case Ast.Value.IntValue(i) => Binding(name.value, IntValue(i)).rightIor
      case Ast.Value.FloatValue(d) => Binding(name.value, FloatValue(d)).rightIor
      case Ast.Value.StringValue(s) => Binding(name.value, StringValue(s)).rightIor
      case Ast.Value.BooleanValue(b) => Binding(name.value, BooleanValue(b)).rightIor
      case Ast.Value.EnumValue(e) => Binding(name.value, UntypedEnumValue(e.value)).rightIor
      case Ast.Value.Variable(v) => Binding(name.value, UntypedVariableValue(v.value)).rightIor
      case _ => mkErrorResult("Argument required")
    }
  }
}

/**
 * GraphQL query compiler.
 *
 * A QueryCompiler parses GraphQL queries to query algebra terms, then
 * applies a collection of transformation phases in sequence, yielding a
 * query algebra term which can be directly interpreted.
 */
abstract class QueryCompiler(schema: Schema) {
  /** The phase of this compiler */
  val phases: List[Phase]
  val vfe = new VariablesAndFragmentsElaborator

  /**
   * Compiles the GraphQL query `text` to a query algebra term which
   * can be directly executed.
   *
   * Any errors are accumulated on the left.
   */
  def compile(text: String, untypedEnv: Option[Json] = None): Result[Query] = {
    val queryType = schema.queryType

    val allPhases = vfe :: phases

    for {
      parsed  <- QueryParser.parseText(text)
      varDefs <- compileVarDefs(parsed._2)
      env     <- compileEnv(varDefs, untypedEnv)
      query   <- allPhases.foldLeftM(parsed._1) { (acc, phase) => phase(acc, env, schema, queryType) }
    } yield query
  }

  def compileVarDefs(untypedVarDefs: UntypedVarDefs): Result[VarDefs] =
    untypedVarDefs.traverse {
      case UntypedVarDef(name, untypedTpe, default) =>
        compileType(untypedTpe).map(tpe => VarDef(name, tpe, default))
    }

  def compileEnv(varDefs: VarDefs, untypedEnv: Option[Json]): Result[Env] =
    untypedEnv match {
      case None => Map.empty.rightIor
      case Some(untypedEnv) =>
        untypedEnv.asObject match {
          case None => mkErrorResult(s"Variables must be represented as a Json object")
          case Some(obj) =>
            (varDefs.traverse {
              case VarDef(name, tpe, default) =>
                (obj(name), default) match {
                  case (None, None) if tpe.isNullable => (name -> ((tpe, NullValue))).rightIor
                  case (None, None) => mkErrorResult(s"No value provided for variable '$name'")
                  case (None, Some(value)) => (name -> ((tpe, value))).rightIor
                  case (Some(value), _) => checkValue(tpe, value).map(value => name -> ((tpe, value)))
                }
            }).map(_.toMap)
        }
    }

  def checkValue(tpe: Type, value: Json): Result[Value] =
    tpe match {
      case _: NullableType if value.isNull =>
        NullValue.rightIor
      case NullableType(tpe) =>
        checkValue(tpe, value)
      case IntType =>
        value.asNumber.flatMap(_.toInt).map(IntValue).toRightIor(mkOneError(s"Expected Int found '$value'"))
      case FloatType =>
        value.asNumber.map(_.toDouble).map(FloatValue).toRightIor(mkOneError(s"Expected Float found '$value'"))
      case StringType =>
        value.asString.map(StringValue).toRightIor(mkOneError(s"Expected String found '$value'"))
      case IDType if value.isNumber =>
        value.asNumber.flatMap(_.toInt).map(i => IDValue(i.toString)).toRightIor(mkOneError(s"Expected ID found '$value'"))
      case IDType =>
        value.asString.map(IDValue).toRightIor(mkOneError(s"Expected ID found '$value'"))
      case BooleanType =>
        value.asString.flatMap { _ match {
          case "true"  => Some(BooleanValue(true))
          case "false" => Some(BooleanValue(false))
          case _       => None
        }}.toRightIor(mkOneError(s"Expected Boolean found '$value'"))
      case e: EnumType =>
        value.asString.flatMap(s => e.value(s)).map(TypedEnumValue).toRightIor(mkOneError(s"Expected $e found '$value'"))
      case _ => mkErrorResult(s"Cannot use non-input type '$tpe' for input values")
    }

  def compileType(tpe: Ast.Type): Result[Type] = {
    def loop(tpe: Ast.Type, nonNull: Boolean): Result[Type] = tpe match {
      case Ast.Type.NonNull(Left(named)) => loop(named, true)
      case Ast.Type.NonNull(Right(list)) => loop(list, true)
      case Ast.Type.List(elem) => loop(elem, false).map(e => if (nonNull) ListType(e) else NullableType(ListType(e)))
      case Ast.Type.Named(name) => schema.tpe(name.value) match {
        case NoType => mkErrorResult(s"Undefine typed '${name.value}'")
        case tpe => (if (nonNull) tpe else NullableType(tpe)).rightIor
      }
    }
    loop(tpe, false)
  }
}

object QueryCompiler {
  /** A QueryCompiler phase. */
  trait Phase {
    /**
     * Transform the supplied query algebra term `query` with expected type
     * `tpe`.
     */
    def apply(query: Query, env: Env, schema: Schema, tpe: Type): Result[Query]
  }

  class VariablesAndFragmentsElaborator extends Phase {
    def apply(query: Query, env: Env, schema: Schema, tpe: Type): Result[Query] = {
      def elaborateVariable(b: Binding): Result[Binding] = b match {
        case Binding(name, UntypedVariableValue(varName)) =>
          env.get(varName) match {
            case Some((_, value)) => Binding(name, value).rightIor
            case None => mkErrorResult(s"Undefined variable '$name'")
          }
        case other => other.rightIor
      }

      def loop(query: Query, tpe: Type): Result[Query] =
        query match {
          case Select(fieldName, args, child) =>
            val childTpe = tpe.underlyingField(fieldName)

            for {
              elaboratedChild <- loop(child, childTpe)
              elaboratedArgs  <- args.traverse(elaborateVariable)
            } yield Select(fieldName, elaboratedArgs, elaboratedChild)

          case UntypedNarrow(tpnme, child) =>
            schema.tpe(tpnme) match {
              case NoType => mkErrorResult(s"Unknown type '$tpnme' in type condition")
              case subtpe =>
                loop(child, subtpe).map { ec =>
                  if (tpe.underlyingObject =:= subtpe) ec else Narrow(subtpe, ec)
                }
            }

          case n@Narrow(subtpe, child)  => loop(child, subtpe).map(ec => n.copy(child = ec))
          case w@Wrap(_, child)         => loop(child, tpe).map(ec => w.copy(child = ec))
          case r@Rename(_, child)       => loop(child, tpe).map(ec => r.copy(child = ec))
          case g@Group(queries)         => queries.traverse(q => loop(q, tpe)).map(eqs => g.copy(queries = eqs))
          case u@Unique(_, child)       => loop(child, tpe.nonNull).map(ec => u.copy(child = ec))
          case f@Filter(_, child)       => loop(child, tpe.item).map(ec => f.copy(child = ec))
          case c@Component(_, _, child) => loop(child, tpe).map(ec => c.copy(child = ec))
          case d@Defer(_, child)        => loop(child, tpe).map(ec => d.copy(child = ec))
          case Empty                    => Empty.rightIor
        }

      loop(query, tpe)
    }
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
   *    nodes into a form which is directly interpretable, replacing
   *    them with a `Filter` or `Unique` node with a `Predicate` which
   *    is parameterized by the arguments, eg.
   *
   *    ```
   *    Select("character", List(IDBinding("id", "1000")), child)
   *    ```
   *    might be translated to,
   *    ```
   *    Filter(FieldEquals("id", "1000"), child)
   *    ```
   *
   * 3. types narrowing coercions by resolving the target type
   *    against the schema.
   */
  class SelectElaborator(mapping: Map[Type, PartialFunction[Select, Result[Query]]]) extends Phase {
    def apply(query: Query, env: Env, schema: Schema, tpe: Type): Result[Query] = {
      def loop(query: Query, tpe: Type): Result[Query] =
        query match {
          case Select(fieldName, args, child) =>
            val childTpe = tpe.underlyingField(fieldName)
            val elaborator: Select => Result[Query] = mapping.get(tpe.underlyingObject) match {
              case Some(e) => (s: Select) => if (e.isDefinedAt(s)) e(s) else s.rightIor
              case _ => (s: Select) => s.rightIor
            }

            for {
              elaboratedChild <- loop(child, childTpe)
              elaboratedArgs <- elaborateArgs(tpe, fieldName, args)
              elaborated <- elaborator(Select(fieldName, elaboratedArgs, elaboratedChild))
            } yield elaborated

          case n@Narrow(subtpe, child)  => loop(child, subtpe).map(ec => n.copy(child = ec))
          case w@Wrap(_, child)         => loop(child, tpe).map(ec => w.copy(child = ec))
          case r@Rename(_, child)       => loop(child, tpe).map(ec => r.copy(child = ec))
          case g@Group(queries)         => queries.traverse(q => loop(q, tpe)).map(eqs => g.copy(queries = eqs))
          case u@Unique(_, child)       => loop(child, tpe.nonNull).map(ec => u.copy(child = ec))
          case f@Filter(_, child)       => loop(child, tpe.item).map(ec => f.copy(child = ec))
          case c@Component(_, _, child) => loop(child, tpe).map(ec => c.copy(child = ec))
          case d@Defer(_, child)        => loop(child, tpe).map(ec => d.copy(child = ec))
          case Empty                    => Empty.rightIor

          case n: UntypedNarrow         => mkErrorResult(s"Unexpected UntypeNarrow ${n.render}")
        }

      loop(query, tpe)
    }

    def elaborateArgs(tpe: Type, fieldName: String, args: List[Binding]): Result[List[Binding]] =
      tpe.underlyingObject match {
        case twf: TypeWithFields =>
          twf.fieldInfo(fieldName) match {
            case Some(field) =>
              val infos = field.args
              val argMap = args.groupMapReduce(_.name)(identity)((x, _) => x)
              infos.traverse { info =>
                argMap.get(info.name) match {
                  case Some(arg) => Binding.forArg(arg, info)
                  case None      => Binding.defaultForInputValue(info)
                }
              }
            case _ => mkErrorResult(s"No field '$fieldName' in type $tpe")
          }
        case _ => mkErrorResult(s"Type $tpe is not an object or interface type")
      }
  }

  /**
   * A compiler phase which partitions a query for execution by multiple
   * composed interpreters.
   *
   * This phase transforms the input query by assigning subtrees to component
   * interpreters as specified by the supplied `mapping`.
   *
   * The mapping has `Type` and field name pairs as keys and component id and
   * join function pairs as values. When the traversal of the input query
   * visits a `Select` node with type `Type.field name` it will replace the
   * `Select` with a `Component` node comprising,
   *
   * 1. the component id of the interpreter which will be responsible for
   *    evaluating the subquery.
   * 2. A join function which will be called during interpretation with,
   *
   *    i)  the cursor at that point in evaluation.
   *    ii) The deferred subquery.
   *
   *    This join function is responsible for computing the continuation
   *    query which will be evaluated by the responsible interpreter.
   *
   *    Because the join is provided with the cursor of the parent
   *    interpreter the subquery can be parameterised with values derived
   *    from the parent query.
   */
  class ComponentElaborator private (mapping: Map[(Type, String), (String, (Cursor, Query) => Result[Query])]) extends Phase {
    def apply(query: Query, env: Env, schema: Schema, tpe: Type): Result[Query] = {
      def loop(query: Query, tpe: Type): Result[Query] =
        query match {
          case PossiblyRenamedSelect(Select(fieldName, args, child), resultName) =>
            val childTpe = tpe.underlyingField(fieldName)
            mapping.get((tpe.underlyingObject, fieldName)) match {
              case Some((cid, join)) =>
                loop(child, childTpe).map { elaboratedChild =>
                  Wrap(resultName, Component(cid, join, PossiblyRenamedSelect(Select(fieldName, args, elaboratedChild), resultName)))
                }
              case None =>
                loop(child, childTpe).map { elaboratedChild =>
                  PossiblyRenamedSelect(Select(fieldName, args, elaboratedChild), resultName)
                }
            }

          case n@Narrow(subtpe, child)  => loop(child, subtpe).map(ec => n.copy(child = ec))
          case w@Wrap(_, child)         => loop(child, tpe).map(ec => w.copy(child = ec))
          case r@Rename(_, child)       => loop(child, tpe).map(ec => r.copy(child = ec))
          case g@Group(queries)         => queries.traverse(q => loop(q, tpe)).map(eqs => g.copy(queries = eqs))
          case u@Unique(_, child)       => loop(child, tpe.nonNull).map(ec => u.copy(child = ec))
          case f@Filter(_, child)       => loop(child, tpe.item).map(ec => f.copy(child = ec))
          case c@Component(_, _, child) => loop(child, tpe).map(ec => c.copy(child = ec))
          case d@Defer(_, child)        => loop(child, tpe).map(ec => d.copy(child = ec))
          case Empty                    => Empty.rightIor

          case n: UntypedNarrow         => mkErrorResult(s"Unexpected UntypeNarrow ${n.render}")
        }

      loop(query, tpe)
    }
  }

  object ComponentElaborator {
    val TrivialJoin = (_: Cursor, q: Query) => q.rightIor

    case class Mapping(tpe: Type, fieldName: String, componentId: String, join: (Cursor, Query) => Result[Query] = TrivialJoin)

    def apply(mappings: Mapping*): ComponentElaborator =
      new ComponentElaborator(mappings.map(m => ((m.tpe, m.fieldName), (m.componentId, m.join))).toMap)
  }
}
