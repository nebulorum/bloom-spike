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

case class FunctionDeclaration(funcIdn: IdnDef, returnType: TypeRef, paramTypes: Seq[TypeRef]) extends PackageDeclaration

trait Collection extends Node

case class FieldDeclaration(idn: IdnDef, typ: TypeRef) extends Node

case class FieldDeclarations(fields: Seq[FieldDeclaration]) extends Node

case class Table(tableIdn: IdnDef, declaration: FieldDeclarations) extends Collection

trait NamespaceCollection extends Node

case class NamespaceDeref(idn:IdnUse, next: NamespaceCollection) extends NamespaceCollection

case class CollectionRef(idn: IdnUse) extends NamespaceCollection

case class TypeRef(idn: IdnUse) extends Node

case class Alias(collection: NamespaceCollection, alias:IdnDef) extends Node

case class CollectionProduct(product: Seq[Alias], selection: Option[Expression], tupleExpressions:Seq[Expression]) extends Node

trait Expression extends Node

case class FieldAccessor(alias: IdnUse, field: IdnUse) extends Expression

case class FunctionCall(function: IdnUse, arguments: Seq[Expression]) extends Expression

case class Rule(lhs: NamespaceCollection, product: CollectionProduct) extends Node

case class ImportPackage(importedPackage: String) extends Node

case class ImportModule(importedModule: String) extends Node

case class ImportModuleWithAlias(importedModule: String, idn: IdnDef) extends Node

trait Identifier extends Node {
  def idn: String
}

case class IdnDef(idn: String) extends Identifier

case class IdnUse(idn: String) extends Identifier