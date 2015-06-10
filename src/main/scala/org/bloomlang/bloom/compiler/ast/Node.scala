package org.bloomlang.bloom.compiler.ast

import org.kiama.relation.Tree

trait Node extends Product

object ProgramTree {
  type ProgramTree = Tree[Node, Program]

  def apply(program:Program):ProgramTree = new Tree[Node, Program](program)
}

case class Program(packages:Seq[Package], containers: Seq[ModuleContainer]) extends Node

case class ModuleContainer(statements: Seq[Node]) extends Node

case class Module(name: String, statements: Seq[Node]) extends Node

case class Package(name: String, declarations: Seq[PackageDeclaration]) extends Node

trait PackageDeclaration extends Node {
  def describe:String
}

case class TypeDeclaration(typeName: IdnDef) extends PackageDeclaration {
  def describe = s"type '${typeName.name}'"
}

trait Collection extends Node

case class FieldDeclaration(name:String, typ: IdnUse) extends Node

case class Table(name: IdnDef, fields: Seq[FieldDeclaration]) extends Collection

case class CollectionRef(idn: IdnUse) extends Node

case class Rule(lhs: CollectionRef) extends Node

case class ImportPackage(importedPackage: String) extends Node

case class ImportModule(importedModule: String) extends Node

case class IdnDef(name: String) extends Node

case class IdnUse(name: String) extends Node