// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle

import scala.annotation.tailrec

import cats.data.Ior
import cats.parse.{LocationMap, Parser}
import cats.implicits._
import io.circe.Json

import Query._, Predicate._, Path._, Value._, UntypedOperation._
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
    def toResult[T](pr: Either[Parser.Error, T]): Result[T] =
      Ior.fromEither(pr).leftMap { e =>
        val lm = LocationMap(text)
        val (row, col) = lm.toLineCol(e.failedAtOffset).get
        val line = lm.getLine(row).get
        val error =
          s"""Parse error at line $row column $col
             |$line
             |${List.fill(col)(" ").mkString}^""".stripMargin
        mkOneError(error)
      }

    for {
      doc   <- toResult(GraphQLParser.Document.parseAll(text))
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

  def parseOperation(op: Operation, fragments: Map[String, FragmentDefinition]): Result[UntypedOperation] = {
    val Operation(opType, _, vds, _, sels) = op
    val q = parseSelections(sels, None, fragments)
    val vs = vds.map {
      case VariableDefinition(nme, tpe, _) => UntypedVarDef(nme.value, tpe, None)
    }
    q.map(q =>
      opType match {
        case OperationType.Query => UntypedQuery(q, vs)
        case OperationType.Mutation => UntypedMutation(q, vs)
        case OperationType.Subscription => UntypedSubscription(q, vs)
      }
    )
  }

  def parseQueryShorthand(qs: QueryShorthand, fragments: Map[String, FragmentDefinition]): Result[UntypedOperation] =
    parseSelections(qs.selectionSet, None, fragments).map(q => UntypedQuery(q, Nil))

  def parseSelections(sels: List[Selection], typeCondition: Option[String], fragments: Map[String, FragmentDefinition]): Result[Query] =
    sels.traverse(parseSelection(_, typeCondition, fragments)).map { sels0 =>
      if (sels0.sizeCompare(1) == 0) sels0.head else Group(sels0)
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
      case Ast.Value.ListValue(vs) => vs.traverse(parseValue).map(ListValue(_))
      case Ast.Value.ObjectValue(fs) =>
        fs.traverse { case (name, value) =>
          parseValue(value).map(v => (name.value, v))
        }.map(ObjectValue(_))
    }
  }
}

object QueryMinimizer {
  import Ast._

  def minimizeText(text: String): Either[String, String] = {
    for {
      doc <- GraphQLParser.Document.parseAll(text).leftMap(_.expected.toList.mkString(","))
    } yield minimizeDocument(doc)
  }

  def minimizeDocument(doc: Document): String = {
    import OperationDefinition._
    import OperationType._
    import Selection._
    import Value._

    def renderDefinition(defn: Definition): String =
      defn match {
        case e: ExecutableDefinition => renderExecutableDefinition(e)
        case _ => ""
      }

    def renderExecutableDefinition(ex: ExecutableDefinition): String =
      ex match {
        case op: OperationDefinition => renderOperationDefinition(op)
        case frag: FragmentDefinition => renderFragmentDefinition(frag)
      }

    def renderOperationDefinition(op: OperationDefinition): String =
      op match {
        case qs: QueryShorthand => renderSelectionSet(qs.selectionSet)
        case op: Operation => renderOperation(op)
      }

    def renderOperation(op: Operation): String =
      renderOperationType(op.operationType) +
      op.name.map(nme => s" ${nme.value}").getOrElse("") +
      renderVariableDefns(op.variables)+
      renderDirectives(op.directives)+
      renderSelectionSet(op.selectionSet)

    def renderOperationType(op: OperationType): String =
      op match {
        case Query => "query"
        case Mutation => "mutation"
        case Subscription => "subscription"
      }

    def renderDirectives(dirs: List[Directive]): String =
      dirs.map { case Directive(name, args) => s"@${name.value}${renderArguments(args)}" }.mkString

    def renderVariableDefns(vars: List[VariableDefinition]): String =
      vars match {
        case Nil => ""
        case _ =>
          vars.map {
            case VariableDefinition(name, tpe, default) =>
              s"$$${name.value}:${tpe.name}${default.map(v => s"=${renderValue(v)}").getOrElse("")}"
          }.mkString("(", ",", ")")
      }

    def renderSelectionSet(sels: List[Selection]): String =
      sels match {
        case Nil => ""
        case _ => sels.map(renderSelection).mkString("{", ",", "}")
      }

    def renderSelection(sel: Selection): String =
      sel match {
        case f: Field => renderField(f)
        case s: FragmentSpread => renderFragmentSpread(s)
        case i: InlineFragment => renderInlineFragment(i)
      }

    def renderField(f: Field) = {
      f.alias.map(a => s"${a.value}:").getOrElse("")+
      f.name.value+
      renderArguments(f.arguments)+
      renderDirectives(f.directives)+
      renderSelectionSet(f.selectionSet)
    }

    def renderArguments(args: List[(Name, Value)]): String =
      args match {
        case Nil => ""
        case _ => args.map { case (n, v) => s"${n.value}:${renderValue(v)}" }.mkString("(", ",", ")")
      }

    def renderInputObject(args: List[(Name, Value)]): String =
      args match {
        case Nil => ""
        case _ => args.map { case (n, v) => s"${n.value}:${renderValue(v)}" }.mkString("{", ",", "}")
      }

    def renderTypeCondition(tpe: Type): String =
      s"on ${tpe.name}"

    def renderFragmentDefinition(frag: FragmentDefinition): String =
      s"fragment ${frag.name.value} ${renderTypeCondition(frag.typeCondition)}${renderDirectives(frag.directives)}${renderSelectionSet(frag.selectionSet)}"

    def renderFragmentSpread(spread: FragmentSpread): String =
      s"...${spread.name.value}${renderDirectives(spread.directives)}"

    def renderInlineFragment(frag: InlineFragment): String =
      s"...${frag.typeCondition.map(renderTypeCondition).getOrElse("")}${renderDirectives(frag.directives)}${renderSelectionSet(frag.selectionSet)}"

    def renderValue(v: Value): String =
      v match {
        case Variable(name) => s"$$${name.value}"
        case IntValue(value) => value.toString
        case FloatValue(value) => value.toString
        case StringValue(value) => s""""$value""""
        case BooleanValue(value) => value.toString
        case NullValue => "null"
        case EnumValue(name) => name.value
        case ListValue(values) => values.map(renderValue).mkString("[", ",", "]")
        case ObjectValue(fields) => renderInputObject(fields)
      }

    doc.map(renderDefinition).mkString(",")
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
  import IntrospectionLevel._

  /**
   * Compiles the GraphQL query `text` to a query algebra term which
   * can be directly executed.
   *
   * Any errors are accumulated on the left.
   */
  def compile(text: String, name: Option[String] = None, untypedVars: Option[Json] = None, introspectionLevel: IntrospectionLevel = Full): Result[Operation] =
    QueryParser.parseText(text, name).flatMap(compileUntyped(_, untypedVars, introspectionLevel))

  def compileUntyped(parsed: UntypedOperation, untypedVars: Option[Json] = None, introspectionLevel: IntrospectionLevel = Full): Result[Operation] = {

    val allPhases =
      IntrospectionElaborator(introspectionLevel).toList ++ (VariablesAndSkipElaborator :: phases)

    for {
      varDefs <- compileVarDefs(parsed.variables)
      vars    <- compileVars(varDefs, untypedVars)
      rootTpe <- parsed.rootTpe(schema)
      query   <- allPhases.foldLeftM(parsed.query) { (acc, phase) => phase.transform(acc, vars, schema, rootTpe) }
    } yield Operation(query, rootTpe)
  }

  def compileVarDefs(untypedVarDefs: UntypedVarDefs): Result[VarDefs] =
    untypedVarDefs.traverse {
      case UntypedVarDef(name, untypedTpe, default) =>
        compileType(untypedTpe).map(tpe => InputValue(name, None, tpe, default))
    }

  def compileVars(varDefs: VarDefs, untypedVars: Option[Json]): Result[Vars] =
    untypedVars match {
      case None => Map.empty.rightIor
      case Some(untypedVars) =>
        untypedVars.asObject match {
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
  sealed trait IntrospectionLevel
  object IntrospectionLevel {
    case object Full extends IntrospectionLevel
    case object TypenameOnly extends IntrospectionLevel
    case object Disabled extends IntrospectionLevel
  }

  import IntrospectionLevel._

  /** A QueryCompiler phase. */
  trait Phase {
    /**
     * Transform the supplied query algebra term `query` with expected type
     * `tpe`.
     */
    def transform(query: Query, vars: Vars, schema: Schema, tpe: Type): Result[Query] =
      query match {
        case s@Select(fieldName, _, child)    =>
          (for {
            obj      <- tpe.underlyingObject
            childTpe <- obj.field(fieldName)
          } yield {
            val isLeaf = childTpe.isUnderlyingLeaf
            if (isLeaf && child != Empty)
              mkErrorResult(s"Leaf field '$fieldName' of $obj must have an empty subselection set")
            else if (!isLeaf && child == Empty)
              mkErrorResult(s"Non-leaf field '$fieldName' of $obj must have a non-empty subselection set")
            else
              transform(child, vars, schema, childTpe).map(ec => s.copy(child = ec))
          }).getOrElse(mkErrorResult(s"Unknown field '$fieldName' in select"))

        case UntypedNarrow(tpnme, child) =>
          (for {
            subtpe <- schema.definition(tpnme)
          } yield {
            transform(child, vars, schema, subtpe).map { ec =>
              if (tpe.underlyingObject.map(_ =:= subtpe).getOrElse(false)) ec else Narrow(schema.ref(tpnme), ec)
            }
          }).getOrElse(mkErrorResult(s"Unknown type '$tpnme' in type condition"))

        case i@Introspect(_, child) if tpe =:= schema.queryType =>
          transform(child, vars, Introspection.schema, Introspection.schema.queryType).map(ec => i.copy(child = ec))

        case i@Introspect(_, child) =>
          val typenameTpe = ObjectType(s"__Typename", None, List(Field("__typename", None, Nil, StringType, false, None)), Nil)
          transform(child, vars, Introspection.schema, typenameTpe).map(ec => i.copy(child = ec))

        case n@Narrow(subtpe, child)  => transform(child, vars, schema, subtpe).map(ec => n.copy(child = ec))
        case w@Wrap(_, child)         => transform(child, vars, schema, tpe).map(ec => w.copy(child = ec))
        case r@Rename(_, child)       => transform(child, vars, schema, tpe).map(ec => r.copy(child = ec))
        case c@Count(_, child)        => transform(child, vars, schema, tpe).map(ec => c.copy(child = ec))
        case g@Group(children)        => children.traverse(q => transform(q, vars, schema, tpe)).map(eqs => g.copy(queries = eqs))
        case g@GroupList(children)    => children.traverse(q => transform(q, vars, schema, tpe)).map(eqs => g.copy(queries = eqs))
        case u@Unique(child)          => transform(child, vars, schema, tpe.nonNull.list).map(ec => u.copy(child = ec))
        case f@Filter(_, child)       => tpe.item.toRightIor(mkOneError(s"Filter of non-List type $tpe")).flatMap(item => transform(child, vars, schema, item).map(ec => f.copy(child = ec)))
        case c@Component(_, _, child) => transform(child, vars, schema, tpe).map(ec => c.copy(child = ec))
        case d@Defer(_, child, _)     => transform(child, vars, schema, tpe).map(ec => d.copy(child = ec))
        case s@Skip(_, _, child)      => transform(child, vars, schema, tpe).map(ec => s.copy(child = ec))
        case l@Limit(_, child)        => transform(child, vars, schema, tpe).map(ec => l.copy(child = ec))
        case o@OrderBy(_, child)      => transform(child, vars, schema, tpe).map(ec => o.copy(child = ec))
        case e@Environment(_, child)  => transform(child, vars, schema, tpe).map(ec => e.copy(child = ec))
        case Skipped                  => Skipped.rightIor
        case Empty                    => Empty.rightIor
      }
  }

  class IntrospectionElaborator(level: IntrospectionLevel) extends Phase {
    override def transform(query: Query, vars: Vars, schema: Schema, tpe: Type): Result[Query] =
      query match {
        case s@PossiblyRenamedSelect(Select(fieldName @ ("__typename" | "__schema" | "__type"), _, _), _) =>
          (fieldName, level) match {
            case ("__typename", Disabled) =>
              mkErrorResult("Introspection is disabled")
            case ("__schema" | "__type", TypenameOnly | Disabled) =>
              mkErrorResult("Introspection is disabled")
            case _ =>
              Introspect(schema, s).rightIor
          }
        case _ => super.transform(query, vars, schema, tpe)
      }
  }

  object IntrospectionElaborator {
    def apply(level: IntrospectionLevel): Option[IntrospectionElaborator] =
      level match {
        case Disabled => None
        case other => Some(new IntrospectionElaborator(other))
      }
  }

  object VariablesAndSkipElaborator extends Phase {
    override def transform(query: Query, vars: Vars, schema: Schema, tpe: Type): Result[Query] =
      query match {
        case Group(children) =>
          children.traverse(q => transform(q, vars, schema, tpe)).map { eqs =>
            eqs.filterNot(_ == Skipped) match {
              case Nil => Skipped
              case eq :: Nil => eq
              case eqs => Group(eqs)
            }
          }
        case Select(fieldName, args, child) =>
          tpe.withUnderlyingField(fieldName) { childTpe =>
            for {
              elaboratedChild <- transform(child, vars, schema, childTpe)
              elaboratedArgs  <- args.traverse(elaborateBinding(vars))
            } yield Select(fieldName, elaboratedArgs, elaboratedChild)
          }
        case Skip(skip, cond, child) =>
          for {
            c  <- extractCond(vars, cond)
            elaboratedChild <- if(c == skip) Skipped.rightIor else transform(child, vars, schema, tpe)
          } yield elaboratedChild

        case _ => super.transform(query, vars, schema, tpe)
      }

    def elaborateBinding(vars: Vars)(b: Binding): Result[Binding] =
      elaborateValue(vars)(b.value).map(ev => b.copy(value = ev))

    def elaborateValue(vars: Vars)(value: Value): Result[Value] =
      value match {
        case UntypedVariableValue(varName) =>
          vars.get(varName) match {
            case Some((_, value)) => value.rightIor
            case None => mkErrorResult(s"Undefined variable '$varName'")
          }
        case ObjectValue(fields) =>
            val (keys, values) = fields.unzip
            values.traverse(elaborateValue(vars)).map(evs => ObjectValue(keys.zip(evs)))
        case ListValue(elems) => elems.traverse(elaborateValue(vars)).map(ListValue.apply)
        case other => other.rightIor
      }


    def extractCond(vars: Vars, value: Value): Result[Boolean] =
      value match {
        case UntypedVariableValue(varName) =>
          vars.get(varName) match {
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
   *
   * 4. verifies that leaves have an empty subselection set and that
   *    structured types have a non-empty subselection set.
   *
   * 5. eliminates Skipped nodes.
   */
  class SelectElaborator(mapping: Map[TypeRef, PartialFunction[Select, Result[Query]]]) extends Phase {
    override def transform(query: Query, vars: Vars, schema: Schema, tpe: Type): Result[Query] =
      query match {
        case Select(fieldName, args, child) =>
          tpe.withUnderlyingField(fieldName) { childTpe =>
            val mapping0 = if (schema eq Introspection.schema) introspectionMapping else mapping
            val elaborator: Select => Result[Query] =
              (for {
                obj <- tpe.underlyingObject
                ref <- schema.ref(obj)
                e   <- mapping0.get(ref)
              } yield (s: Select) => if (e.isDefinedAt(s)) e(s) else s.rightIor).getOrElse((s: Select) => s.rightIor)

            val obj = tpe.underlyingObject
            val isLeaf = childTpe.isUnderlyingLeaf
            if (isLeaf && child != Empty)
              mkErrorResult(s"Leaf field '$fieldName' of $obj must have an empty subselection set")
            else if (!isLeaf && child == Empty)
              mkErrorResult(s"Non-leaf field '$fieldName' of $obj must have a non-empty subselection set")
            else
              for {
                elaboratedChild <- transform(child, vars, schema, childTpe)
                elaboratedArgs <- elaborateArgs(tpe, fieldName, args)
                elaborated <- elaborator(Select(fieldName, elaboratedArgs, elaboratedChild))
              } yield elaborated
          }

        case _: Rename =>
          super.transform(query, vars, schema, tpe).map(_ match {
            case Rename(nme, Environment(e, child)) => Environment(e, Rename(nme, child))
            case q => q
          })

        case Skipped => Empty.rightIor

        case _ => super.transform(query, vars, schema, tpe)
      }

    def elaborateArgs(tpe: Type, fieldName: String, args: List[Binding]): Result[List[Binding]] =
      tpe.underlyingObject match {
        case Some(twf: TypeWithFields) =>
          twf.fieldInfo(fieldName) match {
            case Some(field) =>
              val infos = field.args
              val unknownArgs = args.filterNot(arg => infos.exists(_.name == arg.name))
              if (unknownArgs.nonEmpty)
                mkErrorResult(s"Unknown argument(s) ${unknownArgs.map(s => s"'${s.name}'").mkString("", ", ", "")} in field $fieldName of type ${twf.name}")
              else {
                val argMap = args.groupMapReduce(_.name)(_.value)((x, _) => x)
                infos.traverse(info => checkValue(info, argMap.get(info.name)).map(v => Binding(info.name, v)))
              }
            case _ => mkErrorResult(s"No field '$fieldName' in type $tpe")
          }
        case _ => mkErrorResult(s"Type $tpe is not an object or interface type")
      }
  }

  val introspectionMapping: Map[TypeRef, PartialFunction[Select, Result[Query]]] = Map(
    Introspection.schema.ref("Query") -> {
      case sel@Select("__type", List(Binding("name", StringValue(name))), _) =>
        sel.eliminateArgs(child => Unique(Filter(Eql(UniquePath(List("name")), Const(Option(name))), child))).rightIor
    },
    Introspection.schema.ref("__Type") -> {
      case sel@Select("fields", List(Binding("includeDeprecated", BooleanValue(include))), _) =>
        sel.eliminateArgs(child => if (include) child else Filter(Eql(UniquePath(List("isDeprecated")), Const(false)), child)).rightIor
      case sel@Select("enumValues", List(Binding("includeDeprecated", BooleanValue(include))), _) =>
        sel.eliminateArgs(child => if (include) child else Filter(Eql(UniquePath(List("isDeprecated")), Const(false)), child)).rightIor
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
    override def transform(query: Query, vars: Vars, schema: Schema, tpe: Type): Result[Query] =
      query match {
        case PossiblyRenamedSelect(Select(fieldName, args, child), resultName) =>
          (for {
            obj      <- tpe.underlyingObject
            childTpe <- obj.field(fieldName)
          } yield {
            transform(child, vars, schema, childTpe).map { elaboratedChild =>
              schema.ref(obj).flatMap(ref => cmapping.get((ref, fieldName))) match {
                case Some((mapping, join)) =>
                  Wrap(resultName, Component(mapping, join, PossiblyRenamedSelect(Select(fieldName, args, elaboratedChild), resultName)))
                case None =>
                  PossiblyRenamedSelect(Select(fieldName, args, elaboratedChild), resultName)
              }
            }
          }).getOrElse(mkErrorResult(s"Type $tpe has no field '$fieldName'"))

        case _ => super.transform(query, vars, schema, tpe)
      }
  }

  object ComponentElaborator {
    val TrivialJoin = (_: Cursor, q: Query) => q.rightIor

    case class ComponentMapping[F[_]](tpe: TypeRef, fieldName: String, mapping: Mapping[F], join: (Cursor, Query) => Result[Query] = TrivialJoin)

    def apply[F[_]](mappings: List[ComponentMapping[F]]): ComponentElaborator[F] =
      new ComponentElaborator(mappings.map(m => ((m.tpe, m.fieldName), (m.mapping, m.join))).toMap)
  }

  class QuerySizeValidator(maxDepth: Int, maxWidth: Int) extends Phase {
    override def transform(query: Query, vars: Vars, schema: Schema, tpe: Type): Result[Query] =
      querySize(query) match {
        case (depth, _) if depth > maxDepth => mkErrorResult(s"Query is too deep: depth is $depth levels, maximum is $maxDepth")
        case (_, width) if width > maxWidth => mkErrorResult(s"Query is too wide: width is $width leaves, maximum is $maxWidth")
        case (depth, width) if depth > maxDepth && width > maxWidth => mkErrorResult(s"Query is too complex: width/depth is $width/$depth leaves/levels, maximum is $maxWidth/$maxDepth")
        case (_, _) => Ior.Right(query)
      }

    def querySize(query: Query): (Int, Int) = {
      def handleGroupedQueries(childQueries: List[Query], depth: Int, width: Int): (Int, Int) = {
        val fragmentQueries = childQueries.diff(childQueries.collect { case n: Narrow => n })
        val childSizes =
          if (fragmentQueries.isEmpty) childQueries.map(gq => loop(gq, depth, width, true))
          else childQueries.map(gq => loop(gq, depth + 1, width, true))

        val childDepths = (childSizes.map(size => size._1)).max
        val childWidths = childSizes.map(_._2).sum
        (childDepths, childWidths)
      }
      @tailrec
      def loop(q: Query, depth: Int, width: Int, group: Boolean): (Int, Int) =
        q match {
          case Select(_, _, Empty) => if (group) (depth, width + 1) else (depth + 1, width + 1)
          case Count(_, _) => if (group) (depth, width + 1) else (depth + 1, width + 1)
          case Select(_, _, child) => if (group) loop(child, depth, width, false) else loop(child, depth + 1, width, false)
          case Group(queries) => handleGroupedQueries(queries, depth, width)
          case GroupList(queries) => handleGroupedQueries(queries, depth, width)
          case Component(_, _, child) => loop(child, depth, width, false)
          case Environment(_, child) => loop(child, depth, width, false)
          case Empty => (depth, width)
          case Defer(_, child, _) => loop(child, depth, width, false)
          case Filter(_, child) => loop(child, depth, width, false)
          case Introspect(_, _) => (depth, width)
          case Limit(_, child) => loop(child, depth, width, false)
          case Narrow(_, child) => loop(child, depth, width, true)
          case OrderBy(_, child) => loop(child, depth, width, false)
          case Rename(_, child) => loop(child, depth, width, false)
          case Skip(_, _, child) => loop(child, depth, width, false)
          case Skipped => (depth, width)
          case Unique(child) => loop(child, depth, width, false)
          case UntypedNarrow(_, child) => loop(child, depth, width, false)
          case Wrap(_, child) => loop(child, depth, width, false)
        }

      loop(query, 0, 0, false)
    }
  }
}
