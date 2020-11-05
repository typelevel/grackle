// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import atto.Atto._
import cats.data.Ior
import cats.implicits._
import io.circe.Json

import Query._, Predicate._, Value._, UntypedOperation._
import QueryCompiler._
import QueryInterpreter.{ mkErrorResult, mkOneError }
import ScalarType._

/**
 * GraphQL query parser
 */
object QueryParser {
  import Ast.{ Type => _, Value => _, _ }, OperationDefinition._, Selection._

  /**
   *  Parse a query String to a query algebra term.
   *
   *  Yields a Query value on the right and accumulates errors on the left.
   */
  def parseText(text: String, name: Option[String] = None): Result[UntypedOperation] = {
    def toResult[T](pr: Either[String, T]): Result[T] =
      Ior.fromEither(pr).leftMap(mkOneError(_))

    for {
      doc   <- toResult(GraphQLParser.Document.parseOnly(text).either)
      query <- parseDocument(doc, name)
    } yield query
  }

  def parseDocument(doc: Document, name: Option[String]): Result[UntypedOperation] = {
    val ops = doc.collect { case op: OperationDefinition => op }
    val fragments = doc.collect { case frag: FragmentDefinition => (frag.name.value, frag) }.toMap

    (ops, name) match {
      case (Nil, _) => mkErrorResult("At least one operation required")
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

  def parseOperation(op: Operation, fragments: Map[String, FragmentDefinition]): Result[UntypedOperation] = op match {
    case Operation(opType, _, vds, _, sels) =>
      val q = parseSelections(sels, None, fragments)
      val vs = vds.map {
        case VariableDefinition(nme, tpe, _, _) => UntypedVarDef(nme.value, tpe, None)
      }
      q.map(q => 
        opType match {
          case OperationType.Query => UntypedQuery(q, vs)
          case OperationType.Mutation => UntypedMutation(q, vs)
          case OperationType.Subscription => UntypedSubscription(q, vs)
        }
      )
    case _ => mkErrorResult("Selection required")
  }

  def parseQueryShorthand(qs: QueryShorthand, fragments: Map[String, FragmentDefinition]): Result[UntypedOperation] = qs match {
    case QueryShorthand(sels) => parseSelections(sels, None, fragments).map(q => UntypedQuery(q, Nil))
    case _ => mkErrorResult("Selection required")
  }

  def parseSelections(sels: List[Selection], typeCondition: Option[String], fragments: Map[String, FragmentDefinition]): Result[Query] =
    sels.traverse(parseSelection(_, typeCondition, fragments)).map { sels0 =>
      if (sels0.size == 1) sels0.head else Group(sels0)
    }

  def parseSelection(sel: Selection, typeCondition: Option[String], fragments: Map[String, FragmentDefinition]): Result[Query] = sel match {
    case Field(alias, name, args, directives, sels) =>
      for {
        args0 <- parseArgs(args)
        sels0 <- parseSelections(sels, None, fragments)
        skip  <- parseSkipInclude(directives)
      } yield {
        val sel0 =
          if (sels.isEmpty) Select(name.value, args0, Empty)
          else Select(name.value, args0, sels0)
        val sel1 = alias match {
          case Some(Name(nme)) => Rename(nme, sel0)
          case None => sel0
        }
        val sel2 = typeCondition match {
          case Some(tpnme) => UntypedNarrow(tpnme, sel1)
          case _ => sel1
        }
        val sel3 = skip match {
          case Some((si, value)) => Skip(si, value, sel2)
          case _ => sel2
        }
        sel3
      }

    case FragmentSpread(Name(name), directives) =>
      for {
        frag  <- fragments.get(name).toRightIor(mkOneError(s"Undefined fragment '$name'"))
        skip  <- parseSkipInclude(directives)
        sels0 <- parseSelections(frag.selectionSet, Some(frag.typeCondition.name), fragments)
      } yield {
        val sels = skip match {
          case Some((si, value)) => Skip(si, value, sels0)
          case _ => sels0
        }
        sels
      }

    case InlineFragment(Some(Ast.Type.Named(Name(tpnme))), directives, sels) =>
      for {
        skip  <- parseSkipInclude(directives)
        sels0 <- parseSelections(sels, Some(tpnme), fragments)
      } yield {
        val sels = skip match {
          case Some((si, value)) => Skip(si, value, sels0)
          case _ => sels0
        }
        sels
      }

    case _ =>
      mkErrorResult("Field or fragment spread required")
  }

  def parseSkipInclude(directives: List[Directive]): Result[Option[(Boolean, Value)]] =
    directives.collect { case dir@Directive(Name("skip"|"include"), _) => dir } match {
      case Nil => None.rightIor
      case Directive(Name(si), List((Name("if"), value))) :: Nil => parseValue(value).map(v => Some((si == "skip", v)))
      case Directive(Name(si), _) :: Nil => mkErrorResult(s"$si must have a single Boolean 'if' argument")
      case _ => mkErrorResult(s"Only a single skip/include allowed at a given location")
    }

  def parseArgs(args: List[(Name, Ast.Value)]): Result[List[Binding]] =
    args.traverse((parseArg _).tupled)

  def parseArg(name: Name, value: Ast.Value): Result[Binding] =
    parseValue(value).map(v => Binding(name.value, v))

  def parseValue(value: Ast.Value): Result[Value] = {
    value match {
      case Ast.Value.IntValue(i) => IntValue(i).rightIor
      case Ast.Value.FloatValue(d) => FloatValue(d).rightIor
      case Ast.Value.StringValue(s) => StringValue(s).rightIor
      case Ast.Value.BooleanValue(b) => BooleanValue(b).rightIor
      case Ast.Value.EnumValue(e) => UntypedEnumValue(e.value).rightIor
      case Ast.Value.Variable(v) => UntypedVariableValue(v.value).rightIor
      case Ast.Value.NullValue => NullValue.rightIor
      case Ast.Value.ListValue(vs) => vs.traverse(parseValue).map(ListValue)
      case Ast.Value.ObjectValue(fs) =>
        fs.traverse { case (name, value) =>
          parseValue(value).map(v => (name.value, v))
        }.map(ObjectValue)
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
class QueryCompiler(schema: Schema, phases: List[Phase]) {
  /**
   * Compiles the GraphQL query `text` to a query algebra term which
   * can be directly executed.
   *
   * Any errors are accumulated on the left.
   */
  def compile(text: String, name: Option[String] = None, untypedEnv: Option[Json] = None, useIntrospection: Boolean = true): Result[Query] = {
    val queryType = schema.queryType

    val allPhases =
      if (useIntrospection) IntrospectionElaborator :: VariablesAndSkipElaborator :: phases else VariablesAndSkipElaborator :: phases

    for {
      parsed  <- QueryParser.parseText(text, name)
      varDefs <- compileVarDefs(parsed.variables)
      env     <- compileEnv(varDefs, untypedEnv)
      query   <- allPhases.foldLeftM(parsed.query) { (acc, phase) => phase.transform(acc, env, schema, queryType) }
    } yield query
  }

  def compileVarDefs(untypedVarDefs: UntypedVarDefs): Result[VarDefs] =
    untypedVarDefs.traverse {
      case UntypedVarDef(name, untypedTpe, default) =>
        compileType(untypedTpe).map(tpe => InputValue(name, None, tpe, default))
    }

  def compileEnv(varDefs: VarDefs, untypedEnv: Option[Json]): Result[Env] =
    untypedEnv match {
      case None => Map.empty.rightIor
      case Some(untypedEnv) =>
        untypedEnv.asObject match {
          case None =>
            mkErrorResult(s"Variables must be represented as a Json object")
          case Some(obj) =>
            varDefs.traverse(iv => checkVarValue(iv, obj(iv.name)).map(v => (iv.name, (iv.tpe, v)))).map(_.toMap)
        }
    }

  def compileType(tpe: Ast.Type): Result[Type] = {
    def loop(tpe: Ast.Type, nonNull: Boolean): Result[Type] = tpe match {
      case Ast.Type.NonNull(Left(named)) => loop(named, true)
      case Ast.Type.NonNull(Right(list)) => loop(list, true)
      case Ast.Type.List(elem) => loop(elem, false).map(e => if (nonNull) ListType(e) else NullableType(ListType(e)))
      case Ast.Type.Named(name) => schema.definition(name.value) match {
        case None => mkErrorResult(s"Undefine typed '${name.value}'")
        case Some(tpe) => (if (nonNull) tpe else NullableType(tpe)).rightIor
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
    def transform(query: Query, env: Env, schema: Schema, tpe: Type): Result[Query] =
      query match {
        case s@Select(fieldName, _, child)    =>
          val childTpe = tpe.underlyingField(fieldName)
          if (childTpe =:= NoType) mkErrorResult(s"Unknown field '$fieldName' in select")
          else transform(child, env, schema, childTpe).map(ec => s.copy(child = ec))

        case UntypedNarrow(tpnme, child) =>
          schema.definition(tpnme) match {
            case None => mkErrorResult(s"Unknown type '$tpnme' in type condition")
            case Some(subtpe) =>
              transform(child, env, schema, subtpe).map { ec =>
                if (tpe.underlyingObject =:= subtpe) ec else Narrow(schema.ref(tpnme), ec)
              }
          }

        case i@Introspect(_, child) if tpe =:= schema.queryType =>
          transform(child, env, Introspection.schema, Introspection.schema.queryType).map(ec => i.copy(child = ec))

        case i@Introspect(_, child) =>
          val typenameTpe = ObjectType(s"__Typename", None, List(Field("__typename", None, Nil, StringType, false, None)), Nil)
          transform(child, env, Introspection.schema, typenameTpe).map(ec => i.copy(child = ec))

        case n@Narrow(subtpe, child)  => transform(child, env, schema, subtpe).map(ec => n.copy(child = ec))
        case w@Wrap(_, child)         => transform(child, env, schema, tpe).map(ec => w.copy(child = ec))
        case r@Rename(_, child)       => transform(child, env, schema, tpe).map(ec => r.copy(child = ec))
        case g@Group(children)        => children.traverse(q => transform(q, env, schema, tpe)).map(eqs => g.copy(queries = eqs))
        case g@GroupList(children)    => children.traverse(q => transform(q, env, schema, tpe)).map(eqs => g.copy(queries = eqs))
        case u@Unique(_, child)       => transform(child, env, schema, tpe.nonNull).map(ec => u.copy(child = ec))
        case f@Filter(_, child)       => transform(child, env, schema, tpe.item).map(ec => f.copy(child = ec))
        case c@Component(_, _, child) => transform(child, env, schema, tpe).map(ec => c.copy(child = ec))
        case d@Defer(_, child, _)     => transform(child, env, schema, tpe).map(ec => d.copy(child = ec))
        case s@Skip(_, _, child)      => transform(child, env, schema, tpe).map(ec => s.copy(child = ec))
        case l@Limit(_, child)        => transform(child, env, schema, tpe).map(ec => l.copy(child = ec))
        case o@OrderBy(_, child)      => transform(child, env, schema, tpe).map(ec => o.copy(child = ec))
        case g@GroupBy(_, child)      => transform(child, env, schema, tpe).map(ec => g.copy(child = ec))
        case c@Context(_, child)      => transform(child, env, schema, tpe).map(ec => c.copy(child = ec))
        case Empty                    => Empty.rightIor
      }
  }

  object IntrospectionElaborator extends Phase {
    override def transform(query: Query, env: Env, schema: Schema, tpe: Type): Result[Query] =
      query match {
        case s@PossiblyRenamedSelect(Select("__typename" | "__schema" | "__type", _, _), _) =>
          Introspect(schema, s).rightIor
        case _ => super.transform(query, env, schema, tpe)
      }
  }

  object VariablesAndSkipElaborator extends Phase {
    override def transform(query: Query, env: Env, schema: Schema, tpe: Type): Result[Query] =
      query match {
        case Group(children) =>
          children.traverse(q => transform(q, env, schema, tpe)).map { eqs =>
            eqs.filterNot(_ == Empty) match {
              case Nil => Empty
              case eq :: Nil => eq
              case eqs => Group(eqs)
            }
          }
        case Select(fieldName, args, child) =>
          tpe.withUnderlyingField(fieldName) { childTpe =>
            for {
              elaboratedChild <- transform(child, env, schema, childTpe)
              elaboratedArgs  <- args.traverse(elaborateVariable(env))
            } yield Select(fieldName, elaboratedArgs, elaboratedChild)
          }
        case Skip(skip, cond, child) =>
          for {
            c  <- extractCond(env, cond)
            elaboratedChild <- if(c == skip) Empty.rightIor else transform(child, env, schema, tpe)
          } yield elaboratedChild

        case _ => super.transform(query, env, schema, tpe)
      }

    def elaborateVariable(env: Env)(b: Binding): Result[Binding] = b match {
      case Binding(name, UntypedVariableValue(varName)) =>
        env.get(varName) match {
          case Some((_, value)) => Binding(name, value).rightIor
          case None => mkErrorResult(s"Undefined variable '$varName'")
        }
      case other => other.rightIor
    }

    def extractCond(env: Env, value: Value): Result[Boolean] =
      value match {
        case UntypedVariableValue(varName) =>
          env.get(varName) match {
            case Some((tpe, BooleanValue(value))) if tpe.nonNull =:= BooleanType => value.rightIor
            case Some((_, _)) => mkErrorResult(s"Argument of skip/include must be boolean")
            case None => mkErrorResult(s"Undefined variable '$varName'")
          }
        case BooleanValue(value) => value.rightIor
        case _ => mkErrorResult(s"Argument of skip/include must be boolean")
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
  class SelectElaborator(mapping: Map[TypeRef, PartialFunction[Select, Result[Query]]]) extends Phase {
    override def transform(query: Query, env: Env, schema: Schema, tpe: Type): Result[Query] =
      query match {
        case Select(fieldName, args, child) =>
          tpe.withUnderlyingField(fieldName) { childTpe =>
            val mapping0 = if (schema eq Introspection.schema) introspectionMapping else mapping
            val elaborator: Select => Result[Query] =
              schema.ref(tpe.underlyingObject).flatMap(mapping0.get) match {
              case Some(e) => (s: Select) => if (e.isDefinedAt(s)) e(s) else s.rightIor
              case _ => (s: Select) => s.rightIor
            }

            for {
              elaboratedChild <- transform(child, env, schema, childTpe)
              elaboratedArgs <- elaborateArgs(tpe, fieldName, args)
              elaborated <- elaborator(Select(fieldName, elaboratedArgs, elaboratedChild))
            } yield elaborated
          }

        case _ => super.transform(query, env, schema, tpe)
      }

    def elaborateArgs(tpe: Type, fieldName: String, args: List[Binding]): Result[List[Binding]] =
      tpe.underlyingObject match {
        case twf: TypeWithFields =>
          twf.fieldInfo(fieldName) match {
            case Some(field) =>
              val infos = field.args
              val argMap = args.groupMapReduce(_.name)(_.value)((x, _) => x)
              infos.traverse(info => checkValue(info, argMap.get(info.name)).map(v => Binding(info.name, v)))
            case _ => mkErrorResult(s"No field '$fieldName' in type $tpe")
          }
        case _ => mkErrorResult(s"Type $tpe is not an object or interface type")
      }
  }

  val introspectionMapping: Map[TypeRef, PartialFunction[Select, Result[Query]]] = Map(
    Introspection.schema.ref("Query") -> {
      case sel@Select("__type", List(Binding("name", StringValue(name))), _) =>
        sel.eliminateArgs(child => Unique(Eql(FieldPath(List("name")), Const(name)), child)).rightIor
    },
    Introspection.schema.ref("__Type") -> {
      case sel@Select("fields", List(Binding("includeDeprecated", BooleanValue(include))), _) =>
        sel.eliminateArgs(child => if (include) child else Filter(Eql(FieldPath(List("isDeprecated")), Const(false)), child)).rightIor
      case sel@Select("enumValues", List(Binding("includeDeprecated", BooleanValue(include))), _) =>
        sel.eliminateArgs(child => if (include) child else Filter(Eql(FieldPath(List("isDeprecated")), Const(false)), child)).rightIor
    }
  )

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
  class ComponentElaborator[F[_]] private (cmapping: Map[(Type, String), (Mapping[F], (Cursor, Query) => Result[Query])]) extends Phase {
    override def transform(query: Query, env: Env, schema: Schema, tpe: Type): Result[Query] =
      query match {
        case PossiblyRenamedSelect(Select(fieldName, args, child), resultName) =>
          tpe.withUnderlyingField(fieldName) { childTpe =>
            schema.ref(tpe.underlyingObject).flatMap(ref => cmapping.get((ref, fieldName))) match {
              case Some((mapping, join)) =>
                transform(child, env, schema, childTpe).map { elaboratedChild =>
                  Wrap(resultName, Component(mapping, join, PossiblyRenamedSelect(Select(fieldName, args, elaboratedChild), resultName)))
                }
              case None =>
                transform(child, env, schema, childTpe).map { elaboratedChild =>
                  PossiblyRenamedSelect(Select(fieldName, args, elaboratedChild), resultName)
                }
            }
          }

        case _ => super.transform(query, env, schema, tpe)
      }
  }

  object ComponentElaborator {
    val TrivialJoin = (_: Cursor, q: Query) => q.rightIor

    case class ComponentMapping[F[_]](tpe: TypeRef, fieldName: String, mapping: Mapping[F], join: (Cursor, Query) => Result[Query] = TrivialJoin)

    def apply[F[_]](mappings: List[ComponentMapping[F]]): ComponentElaborator[F] =
      new ComponentElaborator(mappings.map(m => ((m.tpe, m.fieldName), (m.mapping, m.join))).toMap)
  }
}
