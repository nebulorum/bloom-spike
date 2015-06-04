package org.bloomlang.bloom.compiler.analyzer

import org.bloomlang.bloom.compiler.ast.ProgramTree.ProgramTree
import org.bloomlang.bloom.compiler.ast._
import org.kiama.==>
import org.kiama.attribution.{Decorators, Attribution}
import org.kiama.util.{Entity, Environments}

object SymbolTable extends Environments {
  case class MyEntity(definedAt: Node) extends Entity
}

class Analyzer(tree: ProgramTree) extends Attribution {

  import SymbolTable._

  val decorators = new Decorators(tree)

  import decorators._

  def moduleDef(name: String): Option[Module] = {
    moduleDefinition(name)(tree.root)
  }

  val moduleDefinition: String => Node => Option[Module] = {
    paramAttr {
      name => {
        case p: Program => definedModules(p).find(m => m.name.name == name)
        case m@Module(IdnDef(moduleName), _) if name == moduleName => Some(m)
        case n => moduleDefinition(name)(tree.root)
      }
    }
  }

  lazy val defEnv: Chain[Environment] = chain(defEnvIn, defEnvOut)
  lazy val defModuleEnv: Chain[Environment] = chain(defModuleEnvIn, defModuleEnvOut)

  private def defEnvIn(in: Node => Environment): Node ==> Environment = {
    case _:Program =>
      rootenv()

    case node: Module =>
      enter(in(node))
  }

  private def defEnvOut(out: Node => Environment): Node ==> Environment = {
    case node:Module =>
      leave(out(node))

    case n@IdnDef(i) =>
      defineIfNew(out(n),i,defEntity(i))
  }

  private def defModuleEnvIn(in: Node => Environment): Node ==> Environment = {
    case _:Program =>
      rootenv()

    case node: Module =>
      enter(in(node))
  }

  private def defModuleEnvOut(out: Node => Environment): Node ==> Environment = {
    case node:Module =>
      leave(out(node))

    case i@ImportModule(module) if moduleDef(module).isDefined =>
      val m = moduleDef(module).get
      val mEnv = defEnv(tree.lastChild(m).head)
      mEnv.head.foldLeft(out(i))((e,sd) => defineIfNew(e, sd._1, sd._2))
      
    case n@IdnDef(i) =>
      defineIfNew(out(n),i,defEntity(i))
  }

  def defEntity(s:String):Entity = MyEntity(null)

  lazy val definedModules: Program => Seq[Module] = {
    attr {
      case p: Program =>
        for {
          c <- p.containers
          m <- c.statements.collect { case m: Module => m }
        } yield m
    }
  }
}