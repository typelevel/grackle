// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import atto.Atto._
import cats.data.{ Ior, NonEmptyChain }
import cats.implicits._

import Query._, Binding._
import QueryCompiler._
import QueryInterpreter.{ mkError, mkErrorResult }
import Ast.{ Type => _, _ }, OperationDefinition._, OperationType._, Selection._, Value._

/**
 * GraphQL query parser
 */
object QueryParser {
  /**
   *  Parse a query String to a query algebra term.
   *
   *  Yields a Query value on the right and accumulates errors on the left.
   */
  def parseText(text: String, name: Option[String] = None): Result[Query] = {
    def toResult[T](pr: Either[String, T]): Result[T] =
      Ior.fromEither(pr).leftMap(msg => NonEmptyChain.one(mkError(msg)))

    for {
      doc   <- toResult(Parser.Document.parseOnly(text).either)
      query <- parseDocument(doc, name)
    } yield query
  }

  def parseDocument(doc: Document, name: Option[String]): Result[Query] = {
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

  def parseOperation(op: Operation, fragments: Map[String, FragmentDefinition]): Result[Query] = op match {
    case Operation(Query, _, _, _, sels) =>
      parseSelections(sels, None, fragments)
    case _ => mkErrorResult("Selection required")
  }

  def parseQueryShorthand(qs: QueryShorthand, fragments: Map[String, FragmentDefinition]): Result[Query] = qs match {
    case QueryShorthand(sels) => parseSelections(sels, None, fragments)
    case _ => mkErrorResult("Selection required")
  }

  def parseSelections(sels: List[Selection], typeCondition: Option[String], fragments: Map[String, FragmentDefinition]): Result[Query] =
    sels.traverse(parseSelection(_, typeCondition, fragments)).map { sels0 =>
      if (sels0.size == 1) sels0.head else Group(sels0)
    }

  def parseSelection(sel: Selection, typeCondition: Option[String], fragments: Map[String, FragmentDefinition]): Result[Query] = sel match {
    case Field(_, name, args, _, sels) =>
      for {
        args0 <- parseArgs(args)
        sels0 <- parseSelections(sels, None, fragments)
      } yield {
        val sel =
          if (sels.isEmpty) Select(name.value, args0, Empty)
          else Select(name.value, args0, sels0)
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

  def parseArgs(args: List[(Name, Value)]): Result[List[Binding]] =
    args.traverse((parseArg _).tupled)

  def parseArg(name: Name, value: Value): Result[Binding] = value match {
    case IntValue(i) => IntBinding(name.value, i).rightIor
    case FloatValue(d) => FloatBinding(name.value, d).rightIor
    case StringValue(s) => StringBinding(name.value, s).rightIor
    case BooleanValue(b) => BooleanBinding(name.value, b).rightIor
    case EnumValue(e) => UntypedEnumBinding(name.value, e.value).rightIor
    case _ => mkErrorResult("Argument required")
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

  /**
   * Compiles the GraphQL query `text` to a query algebra term which
   * can be directly executed.
   *
   * Any errors are accumulated on the left.
   */
  def compile(text: String): Result[Query] = {
    val query = QueryParser.parseText(text)
    val queryType = schema.queryType
    phases.foldLeft(query) { (acc, phase) => acc.flatMap(phase(_, schema, queryType)) }
  }
}

object QueryCompiler {
  /** A QueryCompiler phase. */
  trait Phase {
    /**
     * Transform the supplied query algebra term `query` with expected type
     * `tpe`.
     */
    def apply(query: Query, schema: Schema, tpe: Type): Result[Query]
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
    def apply(query: Query, schema: Schema, tpe: Type): Result[Query] = {
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
          case g@Group(queries)         => queries.traverse(q => loop(q, tpe)).map(eqs => g.copy(queries = eqs))
          case u@Unique(_, child)       => loop(child, tpe.nonNull).map(ec => u.copy(child = ec))
          case f@Filter(_, child)       => loop(child, tpe.item).map(ec => f.copy(child = ec))
          case c@Component(_, _, child) => loop(child, tpe).map(ec => c.copy(child = ec))
          case d@Defer(_, child)        => loop(child, tpe).map(ec => d.copy(child = ec))
          case Empty                    => Empty.rightIor
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
                  case None => Binding.defaultForInputValue(info)
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
    def apply(query: Query, schema: Schema, tpe: Type): Result[Query] = {
      def loop(query: Query, tpe: Type): Result[Query] =
        query match {
          case Select(fieldName, args, child) =>
            val childTpe = tpe.underlyingField(fieldName)
            mapping.get((tpe.underlyingObject, fieldName)) match {
              case Some((cid, join)) =>
                loop(child, childTpe).map { elaboratedChild =>
                  Wrap(fieldName, Component(cid, join, Select(fieldName, args, elaboratedChild)))
                }
              case None =>
                loop(child, childTpe).map { elaboratedChild =>
                  Select(fieldName, args, elaboratedChild)
                }
            }

          case n@Narrow(subtpe, child)  => loop(child, subtpe).map(ec => n.copy(child = ec))
          case w@Wrap(_, child)         => loop(child, tpe).map(ec => w.copy(child = ec))
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
