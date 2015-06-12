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
      case t: Table => s"table '${t.tableIdn.name}'"
    }
  }

  case class TypeEntity(definedAt: TypeDeclaration) extends BloomEntity {
    def description = s"type '${definedAt.typeIdn.name}'"
  }

  case class AliasEntity(definedAt: Alias) extends BloomEntity {
    def description = s"alias '${definedAt.alias.name}' to collection '${definedAt.collection.idn.name}"
  }

  case class FieldEntity(definedAt: FieldDeclaration) extends BloomEntity {
    def description = s"field '${definedAt.idn.name}'"
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

      case iu@IdnUse(idn) if entityWithName(iu, idn) == UnknownEntity() =>
        message(iu, s"Symbol '$idn' not defined.")

      //Need because duplicate definition in imported modules
      case iu@IdnUse(idn) if hasMultipleDefinition(iu, idn) =>
        message(iu, s"Symbol '$idn' was defined multiple times.")

      case id@IdnDef(idn) if hasMultipleDefinition(id, idn) =>
        message(id, s"Symbol '$idn' was defined multiple times.")

      case CollectionRef(id@IdnUse(idn)) =>
        checkuse(entityWithName(id, idn)) {
          case CollectionEntity(_) => noMessages
          case entity => message(idn, s"Expected reference to collection, found ${describeEntity(entity)}.")
        }
      case FieldDeclaration(_, id@IdnUse(idn)) =>
        checkuse(entityWithName(id, idn)) {
          case TypeEntity(_) => noMessages
          case entity => message(idn, s"Expected reference to type, found ${describeEntity(entity)}.")
        }
      case FieldAccessor(id@IdnUse(idn), fid@IdnUse(fidn)) =>
        checkuse(entityWithName(id, idn)) {
          case AliasEntity(_) => noMessages
          case entity => message(idn, s"Expected collection alias, found ${describeEntity(entity)}.")
        } ++ checkuse(entityWithName(fid, fidn)) {
          case FieldEntity(_) => noMessages
          case entity => message(idn, s"Expected field, found ${describeEntity(entity)}.")
        }
    }

  private def describeEntity(entity: Entity): String =
    entity match {
      case entity: BloomEntity => entity.description
      case rest => rest.toString
    }

  private def hasMultipleDefinition(node: Node, identifier: String) =
    entityWithName(node, identifier) == MultipleEntity()

  private def findField(entity: Entity, fieldIdn: IdnUse): Entity = {
    entity match {
      case AliasEntity(alias@Alias(CollectionRef(IdnUse(tableName)),_)) =>
        entityWithName(alias, tableName) match {
          case CollectionEntity(table: Table) =>
            entityWithName(tree.lastChild(table.declaration).head, fieldIdn.name)
          case _ => UnknownEntity()
        }
      case _ => UnknownEntity()
    }
  }

  private def entityWithName(node: Node, identifier: String):Entity = {
    node match {
      // Follow field to collection declaration f == f1 ensure we only check the field usage point
      case tree.parent.pair(f1, FieldAccessor(alias, f)) if f == f1 =>
        findField(entityWithName(alias, alias.name), f)
      case _ =>
        lookup(defModuleEnv(node), identifier, UnknownEntity())
    }
  }

  def moduleDefinition(name: String): Option[Module] = {
    definedModules.find(m => m.name == name)
  }

  private def packageDefinition(name: String): Option[Package] = definedPackages.find(p => p.name == name)

  private lazy val definedModules: Seq[Module] =
    for {
      c <- tree.root.containers
      m <- c.statements.collect { case m: Module => m }
    } yield m

  private lazy val definedPackages: Seq[Package] = tree.root.packages

  lazy val defEnv: Chain[Environment] = chain(defEnvIn, defEnvOut)
  lazy val defModuleEnv: Chain[Environment] = chain(defModuleEnvIn, defModuleEnvOut)

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

  private def defModuleEnvIn(in: Node => Environment): Node ==> Environment = {
    case _: Program =>
      rootenv()

    case node@(_: Module | _: Package | _: CollectionProduct | _: FieldDeclarations) =>
      enter(in(node))
  }

  private def defModuleEnvOut(out: Node => Environment): Node ==> Environment = {
    case node@(_: Module | _: Package | _: CollectionProduct | _: FieldDeclarations) =>
      leave(out(node))

    case i@ImportModule(module) if moduleDefinition(module).isDefined =>
      val m = moduleDefinition(module).get
      val mEnv = defEnv(tree.lastChild(m).head)
      mEnv.head.foldLeft(out(i))((e, sd) => defineIfNew(e, sd._1, sd._2))

    case i@ImportPackage(name) if packageDefinition(name).isDefined =>
      val m = packageDefinition(name).get
      val mEnv = defEnv(tree.lastChild(m).head)
      mEnv.head.foldLeft(out(i))((e, sd) => defineIfNew(e, sd._1, sd._2))

    case node@IdnDef(i) =>
      defineIfNew(out(node), i, defEntity(node))
  }

  lazy val defEntity: IdnDef => Entity =
    attr {
      case tree.parent(p) =>
        p match {
          case decl: TypeDeclaration => TypeEntity(decl)
          case decl: Table => CollectionEntity(decl)
          case decl: Alias => AliasEntity(decl)
          case decl: FieldDeclaration => FieldEntity(decl)
        }
    }

}