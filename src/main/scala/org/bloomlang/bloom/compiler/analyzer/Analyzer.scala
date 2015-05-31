package org.bloomlang.bloom.compiler.analyzer

import org.bloomlang.bloom.compiler.ast.ProgramTree.ProgramTree
import org.bloomlang.bloom.compiler.ast._
import org.kiama.==>
import org.kiama.attribution.{Decorators, Attribution}
import org.kiama.util.{MultipleEntity, UnknownEntity, Entity, Environments}


object SymbolTable extends Environments {

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

  lazy val env: Chain[Environment] = chain(defEnvIn, defEnvOut)

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

  def defEntity(s:String):Entity = MultipleEntity()

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