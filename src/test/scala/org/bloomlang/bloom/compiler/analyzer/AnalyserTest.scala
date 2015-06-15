package org.bloomlang.bloom.compiler.analyzer

import org.bloomlang.bloom.compiler.ast._
import org.scalatest._
import Matchers._
import org.kiama.util.Messaging._

class AnalyserTest extends FunSuite {

  val systemPackage = makePackage("system.bloom",
    makeType("Int"),
    makeType("String"))

  val table1 = makeTable("table1", "key" -> "Int", "value" -> "String")
  val table2 = makeTable("table2", "id" -> "Int", "value" -> "Int")

  val nullTable1 = makeTable("table1")
  val nullTable2 = makeTable("table2")

  val rule1 = makeRule("table1", makeProduct("table1" -> "a"), makeTupleProducer("a.key", "a.value"))

  val module1 = makeModule("SomeModule",
    ImportModule("OtherModule"),
    table1,
    rule1)

  val otherModule = makeModule("OtherModule", table2)

  val importSystemBloom = ImportPackage("system.bloom")

  val userModules = ModuleContainer(Seq(
    importSystemBloom,
    module1,
    otherModule))

  val program = Program(Seq(systemPackage), Seq(userModules))
  val analyzer = new Analyzer(ProgramTree(program))

  test("can locate modules in a program") {
    analyzer.moduleDefinition("SomeModule") shouldBe Some(module1)
    analyzer.moduleDefinition("OtherModule") shouldBe Some(otherModule)
    analyzer.moduleDefinition("NotThereModule") shouldBe None
  }

  test("can find table on another module without alias") {
    definedSymbols(analyzer, rule1) shouldBe Set("table1")
    definedSymbols(analyzer, table2) shouldBe Set("table2")
    definedSymbols2(analyzer, rule1) shouldBe Set("table1", "table2")
    definedSymbols2(analyzer, table1.declaration.fields.last) shouldBe Set("key", "value")
    definedSymbols2(analyzer, table2.declaration.fields.last) shouldBe Set("id", "value")
  }

  test("report missing module on import") {
    analyzeProgram(systemPackage)(importSystemBloom, module1).
      errorLabels shouldBe Seq("Module 'OtherModule' not defined.")
  }

  test("report missing package import") {
    analyzeProgram()(importSystemBloom).
      errorLabels shouldBe Seq("Package 'system.bloom' not found.")
  }

  test("report missing table definition") {
    analyzeProgram()(makeModule("a", rule1)).
      errorLabels should contain("Symbol 'table1' not defined.")
  }

  test("report double definition of a symbol") {
    analyzeProgram(systemPackage)(importSystemBloom, makeModule("a", table1, makeTable("table1"))).
      errorLabels shouldBe Seq("Symbol 'table1' was defined multiple times.")
  }

  test("report double definition of a type") {
    analyzeProgram(makePackage("sys", makeType("Int"), makeType("Int")))().
      errorLabels shouldBe Seq("Symbol 'Int' was defined multiple times.")
  }

  test("report missing type definition if package not imported") {
    analyzeProgram(systemPackage)(makeModule("A", makeRule("Int", Seq(), Seq()))).
      errorLabels shouldBe Seq("Symbol 'Int' not defined.")
  }

  test("report using type in place of collection") {
    analyzeProgram(systemPackage)(importSystemBloom, makeModule("A", makeRule("Int", Seq(), Seq()))).
      errorLabels shouldBe Seq("Expected reference to collection, found type 'Int'.")
  }

  test("report double definition of a symbol in module import and local") {
    analyzeProgram(systemPackage)(
      importSystemBloom, otherModule,
      makeModule("a", ImportModule("OtherModule"), makeTable("table2"))).
      errorLabels shouldBe Seq("Symbol 'table2' was defined multiple times.")
  }

  test("report double definition of a symbol in module two different modules") {
    analyzeProgram()(
      makeModule("A", makeTable("table1")),
      makeModule("B", makeTable("table1")),
      makeModule("C", ImportModule("A"), ImportModule("B"), rule1)).
      errorLabels should contain("Symbol 'table1' was defined multiple times.")
  }

  test("report missing type on a table definition") {
    analyzeProgram()(
      makeModule("BadCollection", makeTable("badTable", "id" -> "Int"))).
      errorLabels shouldBe Seq("Symbol 'Int' not defined.")
  }

  test("report using collection in type position") {
    analyzeProgram(systemPackage)(
      importSystemBloom,
      makeModule("UseTableOnTypePosition",
        table1,
        makeTable("table2", "id" -> "table1"))).
      errorLabels shouldBe Seq("Expected reference to type, found table 'table1'.")
  }

  test("report duplicated collection alias") {
    analyzeProgram(systemPackage)(
      importSystemBloom,
      makeModule("UseDuplicateAlias",
        nullTable1, nullTable2,
        makeRule("table1", makeProduct("table1" -> "s", "table2" -> "s"), Seq()))).
      errorLabels shouldBe Seq("Symbol 's' was defined multiple times.")
  }

  test("report product using undefined symbol") {
    analyzeProgram(systemPackage)(
      importSystemBloom,
      makeModule("UseNotDefinedTable",
        nullTable1,
        makeRule("table1", makeProduct("tableNotThere" -> "tnt"), Seq()))).
      errorLabels shouldBe Seq("Symbol 'tableNotThere' not defined.")
  }
  test("report product using non collection in rule alias") {
    analyzeProgram(systemPackage)(
      importSystemBloom,
      makeModule("UseTypeInRHS",
        nullTable1,
        makeRule("table1", makeProduct("String" -> "s"), Seq()))).
      errorLabels shouldBe Seq("Expected reference to collection, found type 'String'.")
  }

  test("alias can be identical to existing collection name") {
    analyzeProgram(systemPackage)(
      importSystemBloom,
      makeModule("UseTableInAlias",
        nullTable1, nullTable2,
        makeRule("table1", makeProduct("table1" -> "a", "table1" -> "table1"), Seq()))).
      errorLabels shouldBe noMessages
  }

  test("report rule RHS using unknown collection alias") {
    analyzeProgram(systemPackage)(
      importSystemBloom,
      makeModule("UseUnknownAlias",
        makeTable("table1", "key" -> "Int"),
        makeRule("table1", makeProduct("table1" -> "a"), makeTupleProducer("b.key"))
      )).errorLabels shouldBe Seq("Symbol 'b' not defined.", "Symbol 'key' not defined.")
  }

  test("report rule RHS field accessor must be an alias to collection and field") {
    analyzeProgram(systemPackage)(
      importSystemBloom,
      makeModule("UseOtherSymbolOnAliasPosition",
        table1,
        makeRule("table1", makeProduct("table1" -> "a"), makeTupleProducer("Int.id", "table1.key"))
      )).errorLabels shouldBe
      Seq(
        "Expected collection alias, found type 'Int'.",
        "Expected collection alias, found table 'table1'.",
        "Symbol 'id' not defined.",
        "Symbol 'key' not defined.")
  }

  test("report rule RHS field must refer to fields") {
    analyzeProgram(systemPackage)(
      importSystemBloom,
      makeModule("UseOtherSymbolOnAliasPosition",
        table1,
        makeRule("table1", makeProduct("table1" -> "a"), makeTupleProducer("a.Int", "a.table1"))
      )).errorLabels shouldBe
      Seq(
        "Expected field, found type 'Int'.",
        "Expected field, found table 'table1'.")
  }

  test("report incorrect arity in the RHS of rules") {
    analyzeProgram(systemPackage)(
      importSystemBloom,
      makeModule("UseOtherSymbolOnAliasPosition",
        table1,
        makeRule("table1", makeProduct("table1" -> "a"), makeTupleProducer("a.key"))
      )).errorLabels shouldBe Seq("Incorrect arity in rule, expected 2 found 1")
  }

  test("report incorrect type on RHS tuple producer") {
    analyzeProgram(systemPackage)(
      importSystemBloom,
      makeModule("UseOtherSymbolOnAliasPosition",
        table1,
        makeRule("table1", makeProduct("table1" -> "a"), makeTupleProducer("a.value", "a.key"))
      )).errorLabels shouldBe Seq("Expected type 'Int' found 'String'.", "Expected type 'String' found 'Int'.")
  }

  test("good program should have no messages") {
    analyzer.errors shouldBe noMessages
  }

  private def analyzeProgram(pkgs: Package*)(stmts: Node*) =
    new TestAnalyzer(Program(pkgs, Seq(ModuleContainer(stmts))))

  private def makeModule(moduleName: String, stmts: Node*) =
    Module(moduleName, stmts)

  private def makeTable(tableName: String, fields: (String, String)*): Table = {
    Table(IdnDef(tableName), FieldDeclarations(fields.map(p => FieldDeclaration(IdnDef(p._1), IdnUse(p._2)))))
  }

  private def makeRule(lhs: String, product: Seq[Alias], producer: Seq[FieldAccessor]) =
    Rule(CollectionRef(IdnUse(lhs)), CollectionProduct(product, producer))

  private def makeTupleProducer(values: String*): Seq[FieldAccessor] = values.map(v => {
    val parts = v.split("\\.")
    fieldAccess(parts(0), parts(1))
  })

  private def fieldAccess(alias: String, field: String) = FieldAccessor(IdnUse(alias), IdnUse(field))

  private def makeProduct(aliases: (String, String)*) =
    aliases.map(p => Alias(CollectionRef(IdnUse(p._1)), IdnDef(p._2)))

  private def makeType(name: String) = TypeDeclaration(IdnDef(name))

  private def makePackage(packageName: String, declarations: PackageDeclaration*) = Package(packageName, declarations)

  private def definedSymbols(analyzer: Analyzer, node: Node) = analyzer.defEnv(node).head.keySet

  private def definedSymbols2(analyzer: Analyzer, node: Node) = analyzer.finalEnvAt(node).head.keySet

  class TestAnalyzer(val program: Program) {
    val analyzer = new Analyzer(ProgramTree(program))

    def errorLabels = analyzer.errors.map(_.label)
  }

}