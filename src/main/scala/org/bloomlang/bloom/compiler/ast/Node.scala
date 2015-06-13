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

trait PackageDeclaration extends Node

case class TypeDeclaration(typeIdn: IdnDef) extends PackageDeclaration

trait Collection extends Node

case class FieldDeclaration(idn: IdnDef, typ: IdnUse) extends Node

case class FieldDeclarations(fields: Seq[FieldDeclaration]) extends Node

case class Table(tableIdn: IdnDef, declaration: FieldDeclarations) extends Collection

case class CollectionRef(idn: IdnUse) extends Node

case class Alias(collection: CollectionRef, alias:IdnDef) extends Node

case class CollectionProduct(product: Seq[Alias], tupleExpressions:Seq[FieldAccessor]) extends Node

case class FieldAccessor(alias: IdnUse, field: IdnUse) extends Node

case class Rule(lhs: CollectionRef, product: CollectionProduct) extends Node

case class ImportPackage(importedPackage: String) extends Node

case class ImportModule(importedModule: String) extends Node

trait Identifier extends Node {
  def idn: String
}

case class IdnDef(idn: String) extends Identifier

case class IdnUse(idn: String) extends Identifier