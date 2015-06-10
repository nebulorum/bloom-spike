package org.bloomlang.bloom.compiler.analyzer

import org.bloomlang.bloom.compiler.ast.ProgramTree.ProgramTree
import org.bloomlang.bloom.compiler.ast._
import org.kiama.==>
import org.kiama.attribution.{Decorators, Attribution}
import org.kiama.util.Messaging._
import org.kiama.util._

object SymbolTable extends Environments {

  case class CollectionEntity(definedAt: Collection) extends Entity

  case class TypeEntity(definedAt: TypeDeclaration) extends Entity

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

      case iu@IdnUse(idn) if !isDefinedInEnv(defModuleEnv(iu), idn) =>
        message(iu, s"Symbol '$idn' not defined.")

      case iu@IdnUse(idn) if hasMultipleDefinition(iu, idn) =>
        message(iu, s"Symbol '$idn' was defined multiple times.")

      case id@IdnDef(idn) if hasMultipleDefinition(id, idn) =>
        message(id, s"Symbol '$idn' was defined multiple times.")

      case CollectionRef(id@IdnUse(idn)) =>
        checkuse(lookup(defModuleEnv(id), idn, UnknownEntity())) {
          case CollectionEntity(_) => noMessages
          case other => message(idn, s"Expected collection declaration, found ${describeEntity(other)}.")
        }
      case FieldDeclaration(_, id@IdnUse(idn)) =>
        checkuse(lookup(defModuleEnv(id), idn, UnknownEntity())) {
          case TypeEntity(_) => noMessages
          case entity => message(idn, s"Expected type declaration, found ${describeEntity(entity)}.")
        }
    }

  private def describeEntity(entity: Entity): String =
    entity match {
      case CollectionEntity(Table(IdnDef(name), _)) => s"table '$name'"
      case entity: TypeEntity => entity.definedAt.describe
      case rest => rest.toString
    }

  private def hasMultipleDefinition(node: Node, identifier: String) =
    lookup(defModuleEnv(node), identifier, UnknownEntity()) == MultipleEntity()

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

    case node@(_: Module | _: Package) =>
      enter(in(node))
  }

  private def defEnvOut(out: Node => Environment): Node ==> Environment = {
    case node@(_: Module | _: Package) =>
      leave(out(node))

    case node@IdnDef(i) =>
      defineIfNew(out(node), i, defEntity(node))
  }

  private def defModuleEnvIn(in: Node => Environment): Node ==> Environment = {
    case _: Program =>
      rootenv()

    case node@(_: Module | _: Package) =>
      enter(in(node))
  }

  private def defModuleEnvOut(out: Node => Environment): Node ==> Environment = {
    case node@(_: Module | _: Package) =>
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
        }
    }

}