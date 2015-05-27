package org.bloomlang.bloom.compiler.analyzer

import org.bloomlang.bloom.compiler.ast.ProgramTree.ProgramTree
import org.bloomlang.bloom.compiler.ast._
import org.kiama.attribution.Attribution

class Analyzer(program: ProgramTree) extends Attribution {

  def moduleDef(name: String): Option[Module] = {
    moduleDefinition(name)(program.root)
  }

  val moduleDefinition: String => Node => Option[Module] = {
    paramAttr {
      name => {
        case p: Program => definedModules(p).find(m => m.name == name)
        case m@Module(moduleName) if name == moduleName => Some(m)
        case n => moduleDefinition(name)(program.root)
      }
    }
  }

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