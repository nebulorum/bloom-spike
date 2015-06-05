package org.bloomlang.bloom.compiler.ast

import org.kiama.relation.Tree

trait Node extends Product

object ProgramTree {
  type ProgramTree = Tree[Node, Program]

  def apply(program:Program):ProgramTree = new Tree[Node, Program](program)
}

case class Program(packages:Seq[Package], containers: Seq[ModuleContainer]) extends Node

case class ModuleContainer(statements: Seq[Node]) extends Node

case class Module(name: IdnDef, statements: Seq[Node]) extends Node

case class Package(name: String) extends Node

trait Collection extends Node

case class Table(name: IdnDef) extends Collection

case class Rule(lhs: IdnUse) extends Node

case class ImportPackage(importedPackage: String) extends Node

case class ImportModule(importedModule: String) extends Node

case class IdnDef(name: String) extends Node

case class IdnUse(name: String) extends Node