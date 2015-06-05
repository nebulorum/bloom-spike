package org.bloomlang.bloom.compiler.analyzer

import org.bloomlang.bloom.compiler.ast.ProgramTree.ProgramTree
import org.bloomlang.bloom.compiler.ast._
import org.kiama.==>
import org.kiama.attribution.{Decorators, Attribution}
import org.kiama.util.Messaging._
import org.kiama.util.{Entity, Environments}

object SymbolTable extends Environments {

  case class MyEntity(definedAt: Node) extends Entity

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
    }

  def moduleDefinition(name: String): Option[Module] = {
    definedModules.find(m => m.name.name == name)
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

    case node: Module =>
      enter(in(node))
  }

  private def defEnvOut(out: Node => Environment): Node ==> Environment = {
    case node: Module =>
      leave(out(node))

    case n@IdnDef(i) =>
      defineIfNew(out(n), i, defEntity(i))
  }

  private def defModuleEnvIn(in: Node => Environment): Node ==> Environment = {
    case _: Program =>
      rootenv()

    case node: Module =>
      enter(in(node))
  }

  private def defModuleEnvOut(out: Node => Environment): Node ==> Environment = {
    case node: Module =>
      leave(out(node))

    case i@ImportModule(module) if moduleDefinition(module).isDefined =>
      val m = moduleDefinition(module).get
      val mEnv = defEnv(tree.lastChild(m).head)
      mEnv.head.foldLeft(out(i))((e, sd) => defineIfNew(e, sd._1, sd._2))

    case n@IdnDef(i) =>
      defineIfNew(out(n), i, defEntity(i))
  }

  def defEntity(s: String): Entity = MyEntity(null)

}