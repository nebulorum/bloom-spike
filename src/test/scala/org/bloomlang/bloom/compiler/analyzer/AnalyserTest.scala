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

  val rule1 = makeRule("table1", null)

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
      errorLabels shouldBe Seq("Symbol 'table1' not defined.")
  }

  test("report double definition of a symbol") {
    analyzeProgram(systemPackage)(makeModule("a", importSystemBloom, table1, makeTable("table1"))).
      errorLabels shouldBe Seq("Symbol 'table1' was defined multiple times.")
  }

  test("report double definition of a type") {
    analyzeProgram(makePackage("sys", makeType("Int"), makeType("Int")))().
      errorLabels shouldBe Seq("Symbol 'Int' was defined multiple times.")
  }

  test("report missing type definition if package not imported") {
    analyzeProgram(systemPackage)(makeModule("A", makeRule("Int", null))).
      errorLabels shouldBe Seq("Symbol 'Int' not defined.")
  }

  test("report using type in place of collection") {
    analyzeProgram(systemPackage)(importSystemBloom, makeModule("A", makeRule("Int", null))).
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
      errorLabels shouldBe Seq("Symbol 'table1' was defined multiple times.")
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
        table1, table2,
        makeRule("table1", makeProduct("table1" -> "s", "table2" -> "s")))).
      errorLabels shouldBe Seq("Symbol 's' was defined multiple times.")
  }

  test("report product using undefined symbol") {
    analyzeProgram(systemPackage)(
      importSystemBloom,
      makeModule("UseNotDefinedTable",
        table1,
        makeRule("table1", makeProduct("tableNotThere" -> "tnt")))).
      errorLabels shouldBe Seq("Symbol 'tableNotThere' not defined.")
  }
  test("report product using non collection in rule alias") {
    analyzeProgram(systemPackage)(
      importSystemBloom,
      makeModule("UseTypeInRHS",
        table1,
        makeRule("table1", makeProduct("String" -> "s")))).
      errorLabels shouldBe Seq("Expected reference to collection, found type 'String'.")
  }

  test("alias can be identical to existing collection name") {
    analyzeProgram(systemPackage)(
      importSystemBloom,
      makeModule("UseTableInAlias",
        table1, table2,
        makeRule("table1", makeProduct("table2" -> "a", "table1" -> "table1")))).
      errorLabels shouldBe noMessages
  }

  test("good program should have no messages") {
    analyzer.errors shouldBe noMessages
  }

  private def analyzeProgram(pkgs: Package*)(stmts: Node*) =
    new TestAnalyzer(Program(pkgs, Seq(ModuleContainer(stmts))))

  private def makeModule(moduleName: String, stmts: Node*) =
    Module(moduleName, stmts)

  private def makeTable(tableName: String, fields: (String, String)*): Table = {
    Table(IdnDef(tableName), fields.map(p => FieldDeclaration(p._1, IdnUse(p._2))))
  }

  private def makeRule(lhs: String, product: CollectionProduct) = Rule(CollectionRef(IdnUse(lhs)), product)

  private def makeProduct(aliases: (String, String)*) =
    CollectionProduct(aliases.map(p => Alias(CollectionRef(IdnUse(p._1)), IdnDef(p._2))))

  private def makeType(name: String) = TypeDeclaration(IdnDef(name))

  private def makePackage(packageName: String, declarations: PackageDeclaration*) = Package(packageName, declarations)

  private def definedSymbols(analyzer: Analyzer, node: Node) = analyzer.defEnv(node).head.keySet

  private def definedSymbols2(analyzer: Analyzer, node: Node) = analyzer.defModuleEnv(node).head.keySet

  class TestAnalyzer(val program: Program) {
    val analyzer = new Analyzer(ProgramTree(program))

    def errorLabels = analyzer.errors.map(_.label)
  }

}