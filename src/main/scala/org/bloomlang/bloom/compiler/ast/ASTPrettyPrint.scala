package org.bloomlang.bloom.compiler.ast

import org.kiama.output.PrettyPrinter

import org.kiama.output.PrettyPrinterTypes.Document


class ASTPrettyPrint extends PrettyPrinter {

  def format(program: Program): Document = pretty(toDoc(program), 5)

  def toDoc(node: Node): Doc = {
    node match {
      case Program(packages, containers) =>
        fileBreak("BEGIN") <>
          vsep(packages.toList map toDoc) <@> vsep(containers.toList map toDoc) <> fileBreak("END")
      case ModuleContainer(statements) =>
        fileBreak("source") <> vsep(statements.toList map toDoc)
      case FunctionDeclaration(idn, retType, args) =>
        "function" <+> toDoc(idn) <+> brackets(ssep(args.toList map toDoc, ", ")) <+> "->" <+> toDoc(retType)
      case Package(name, desc) =>
        fileBreak("packages") <>
          "package" <+> name <> line <> braces(nest(line <> vsep(desc.toList map toDoc)) <> line)
      case TypeDeclaration(idn) => "type" <+> toDoc(idn)

      case ImportPackage(ip) => "import" <+> ip
      case ImportModule(im) => "import" <+> im

      case Module(name, statements) =>
        "module" <+> name <> line <> braces(nest(line <> vsep(statements.toList map toDoc)) <> line) <> line
      case Rule(lhs, rhs) =>
        lhs.idn.idn <+> "<==" <+> toDoc(rhs)
      case Table(idn, fields) =>
        "table" <+> idn.idn <+> brackets(ssep(fields.fields.toList map toDoc, ", "))

      case FieldDeclaration(idn, typeRef) =>
        toDoc(idn) <+> ":" <+> toDoc(typeRef)
      case CollectionProduct(products, optionalSelection, expressions) =>
        if (optionalSelection.isDefined) {
          brackets(ssep(products.toList map toDoc, " * ")) <+>
            "having" <+> parens(toDoc(optionalSelection.get)) <+>
            "=> project" <+> brackets(ssep(expressions.toList map toDoc, ", "))
        } else {
          brackets(ssep(products.toList map toDoc, " * ")) <+>
            "=> project" <+> brackets(ssep(expressions.toList map toDoc, ", "))
        }
      case Alias(cr, alias) =>
        cr.idn.idn <+> "->" <+> alias.idn
      case FieldAccessor(alias, field) =>
        toDoc(alias) <> "." <> toDoc(field)
      case FunctionCall(fname, args) =>
        fname.idn <> parens(ssep(args.toList map toDoc, ", "))

      case idn: Identifier => idn.idn
      case TypeRef(t) => t.idn

      case s => "?" + s.toString
    }
  }

  private def fileBreak(fileName: String): Doc = {
    val separator = "-" * 10

    separator <+> fileName <+> separator <> line
  }
}