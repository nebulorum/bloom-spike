package org.bloomlang.bloom.compiler.analyzer

import org.bloomlang.bloom.compiler.ast.ProgramTree.ProgramTree
import org.bloomlang.bloom.compiler.ast._
import org.kiama.==>
import org.kiama.attribution.{Decorators, Attribution}
import org.kiama.util.Messaging._
import org.kiama.util._

object SymbolTable extends Environments {

  trait BloomEntity extends Entity {
    def description: String
  }

  case class CollectionEntity(definedAt: Collection) extends BloomEntity {
    def description = definedAt match {
      case t: Table => s"table '${t.tableIdn.idn}'"
    }
  }

  case class TypeEntity(definedAt: TypeDeclaration) extends BloomEntity {
    def description = s"type '${definedAt.typeIdn.idn}'"
  }

  case class AliasEntity(definedAt: Alias) extends BloomEntity {
    def description = s"alias '${definedAt.alias.idn}' to collection '${definedAt.collection.idn.idn}"
  }

  case class FieldEntity(definedAt: FieldDeclaration) extends BloomEntity {
    def description = s"field '${definedAt.idn.idn}'"
  }

  case class FunctionEntity(definedAt: FunctionDeclaration) extends BloomEntity {
    def description = s"function '${definedAt.funcIdn.idn}'"
  }

}

class Analyzer(tree: ProgramTree) extends Attribution {

  import SymbolTable._

  val decorators = new Decorators(tree)

  import decorators._

  lazy val errors: Messages =
    collectmessages(tree) {
      case im@ImportModule(module) if moduleDefinition(module).isEmpty =>
        message(im, s"Module '$module' not defined.")

      case ip@ImportPackage(pkg) if packageDefinition(pkg).isEmpty =>
        message(ip, s"Package '$pkg' not found.")

      case iu@IdnUse(idn) if entityWithName(iu) == UnknownEntity() =>
        message(iu, s"Symbol '$idn' not defined.")

      //Need because duplicate definition in imported modules
      case iu@IdnUse(idn) if hasMultipleDefinition(iu) =>
        message(iu, s"Symbol '$idn' was defined multiple times.")

      case id@IdnDef(idn) if hasMultipleDefinition(id) =>
        message(id, s"Symbol '$idn' was defined multiple times.")

      case CollectionRef(id@IdnUse(idn)) =>
        checkuse(entityWithName(id)) {
          case CollectionEntity(_) => noMessages
          case entity => message(idn, s"Expected reference to collection, found ${describeEntity(entity)}.")
        }
      case TypeRef(id: IdnUse) =>
        checkuse(entityWithName(id)) {
          case TypeEntity(_) => noMessages
          case entity => message(id, s"Expected reference to type, found ${describeEntity(entity)}.")
        }
      case FieldAccessor(id: IdnUse, fid: IdnUse) =>
        checkuse(entityWithName(id)) {
          case AliasEntity(_) => noMessages
          case entity => message(id, s"Expected collection alias, found ${describeEntity(entity)}.")
        } ++ checkuse(entityWithName(fid)) {
          case FieldEntity(_) => noMessages
          case entity => message(fid, s"Expected field, found ${describeEntity(entity)}.")
        }
      case Rule(collection, producer) =>
        entityWithName(collection.idn) match {
          case CollectionEntity(t: Table) =>
            val fieldTypes = t.declaration.fields.map(_.typ)
            val expressions = producer.tupleExpressions
              checkTupleTypes(collection.idn, fieldTypes, expressions)
          case _ =>
            noMessages
        }
    }

  private def checkTupleTypes(idn: IdnUse, expectedTypes: Seq[TypeRef], expressions: Seq[Expression]): Messages = {
    val messages: Seq[Messages] = for {
      (expType, expression) <- expectedTypes zip expressions
    } yield checkExpressionType(entityType(entityWithName(expType.idn)), expression)
    messages.flatten.toVector ++ checkTupleArity(idn, expectedTypes, expressions)
  }

  private def checkTupleArity(use: IdnUse, fields: Seq[TypeRef], gen: Seq[Expression]): Messages =
    message(use, s"Incorrect arity, expected ${fields.size} found ${gen.size}",
      fields.size != gen.size)

  private def checkExpressionType(expectedType: Option[TypeDeclaration], expression: Expression): Messages = {
    expression match {
      case accessor: FieldAccessor =>
        val assessorType = entityTypeIfField(entityWithName(accessor.field))
        checkIfTypeMatches(expression, expectedType, assessorType)

      case fCall: FunctionCall =>
        val fType = functionReturnTypeIfFunction(entityWithName(fCall.function))
        val returnMsg = checkIfTypeMatches(fCall, expectedType, fType)
        val argumentMessages: Messages = entityWithName(fCall.function) match {
          case FunctionEntity(fd) =>
            checkTupleTypes(fCall.function, fd.paramTypes, fCall.arguments)
          case _ => noMessages
        }
        returnMsg ++ argumentMessages
      case _ =>
        noMessages
    }
  }

  private def checkIfTypeMatches(node: Node, expectType: Option[TypeDeclaration], effectiveType: Option[TypeDeclaration]) =
    if (expectType.isDefined && effectiveType.isDefined && expectType != effectiveType)
      message(node, s"Expected type '${expectType.get.typeIdn.idn}' found '${effectiveType.get.typeIdn.idn}'.")
    else
      noMessages

  private def entityTypeIfField(entity: Entity): Option[TypeDeclaration] =
    entity match {
      case fe@FieldEntity(fd) => entityType(fe)
      case _ => None
    }

  private def functionReturnTypeIfFunction(entity: Entity): Option[TypeDeclaration] =
    entity match {
      case fe@FunctionEntity(fd) =>
        entityType(entityWithName(fd.returnType.idn))
      case _ => None
    }

  private def entityType(entity: Entity): Option[TypeDeclaration] =
    entity match {
      case FieldEntity(FieldDeclaration(_, TypeRef(typ))) => entityType(entityWithName(typ))
      case TypeEntity(typ) => Some(typ)
      case _ => None
    }

  private def describeEntity(entity: Entity): String =
    entity match {
      case entity: BloomEntity => entity.description
      case rest => rest.toString
    }

  private def hasMultipleDefinition(node: Identifier) =
    entityWithName(node) == MultipleEntity()

  private def findField(entity: Entity, fieldIdn: IdnUse): Entity = {
    entity match {
      case AliasEntity(alias@Alias(CollectionRef(cid: IdnUse), _)) =>
        entityWithName(cid) match {
          case CollectionEntity(table: Table) =>
            lookup(finalEnvAt(table.declaration), fieldIdn.idn, UnknownEntity())
          case _ => UnknownEntity()
        }
      case _ => UnknownEntity()
    }
  }

  private lazy val entityWithName: Identifier => Entity = {
    attr {
      // Follow field to collection declaration f == f1 ensure we only check the field usage point
      case tree.parent.pair(f1, FieldAccessor(alias, f)) if f == f1 =>
        findField(entityWithName(alias), f)
      case node =>
        // Cant use finalEnvAt(node) because this will cause two messages to popup on at each location.
        lookup(defCompoundEnv(node), node.idn, UnknownEntity())
    }
  }

  def moduleDefinition(name: String): Option[Module] = definedModules.find(m => m.name == name)

  private def packageDefinition(name: String): Option[Package] = definedPackages.find(p => p.name == name)

  private lazy val definedModules: Seq[Module] =
    for {
      c <- tree.root.containers
      m <- c.statements.collect { case m: Module => m }
    } yield m

  private lazy val definedPackages: Seq[Package] = tree.root.packages

  lazy val defEnv: Chain[Environment] = chain(defEnvIn, defEnvOut)
  lazy val defCompoundEnv: Chain[Environment] = chain(defCompoundEnvIn, defCompoundEnvOut)

  private def defEnvIn(in: Node => Environment): Node ==> Environment = {
    case _: Program =>
      rootenv()

    case node@(_: Module | _: Package | _: CollectionProduct | _: FieldDeclarations) =>
      enter(in(node))
  }

  private def defEnvOut(out: Node => Environment): Node ==> Environment = {
    case node@(_: Module | _: Package | _: CollectionProduct | _: FieldDeclarations) =>
      leave(out(node))

    case node@IdnDef(i) =>
      defineIfNew(out(node), i, defEntity(node))
  }

  private def defCompoundEnvIn(in: Node => Environment): Node ==> Environment = {
    case _: Program =>
      rootenv()

    case node@(_: Module | _: Package | _: CollectionProduct | _: FieldDeclarations) =>
      enter(in(node))
  }

  private def defCompoundEnvOut(out: Node => Environment): Node ==> Environment = {
    case node@(_: Module | _: Package | _: CollectionProduct | _: FieldDeclarations) =>
      leave(out(node))

    case i@ImportModule(module) if moduleDefinition(module).isDefined =>
      mergeEnvironment(moduleDefinition(module).get, out(i))

    case i@ImportPackage(name) if packageDefinition(name).isDefined =>
      mergeEnvironment(packageDefinition(name).get, out(i))

    case node@IdnDef(i) =>
      defineIfNew(out(node), i, defEntity(node))
  }

  /**
   * This is the environment at the end of the block.
   */
  lazy val finalEnvAt: Node => Environment =
    attr {
      case tree.lastChild.pair(_: Module | _: Package | _: CollectionProduct | _: FieldDeclarations, c) =>
        defCompoundEnv(c)

      case tree.parent(p) =>
        finalEnvAt(p)
    }

  private def mergeEnvironment(otherEnvironmentDefiningNode: Node, originalEnv: Environment): Environment = {
    val envToMerge = defEnv(tree.lastChild(otherEnvironmentDefiningNode).head)
    envToMerge.head.foldLeft(originalEnv)((e, sd) => defineIfNew(e, sd._1, sd._2))
  }

  lazy val defEntity: IdnDef => Entity =
    attr {
      case tree.parent(p) =>
        p match {
          case decl: FunctionDeclaration => FunctionEntity(decl)
          case decl: TypeDeclaration => TypeEntity(decl)
          case decl: Table => CollectionEntity(decl)
          case decl: Alias => AliasEntity(decl)
          case decl: FieldDeclaration => FieldEntity(decl)
        }
    }

}