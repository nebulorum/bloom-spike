package org.bloomlang.bloom.compiler.analyzer

import org.bloomlang.bloom.compiler.ast._
import org.scalatest._
import Matchers._
import org.kiama.util.Messaging._

class AnalyserTest extends FunSuite {

  val package1 = Package("system.bloom")

  val table1 = makeTable("table1")
  val table2 = makeTable("table2")

  val rule1 = Rule(IdnUse("table1"))
  val module1 = makeModule("SomeModule", 
    ImportModule("OtherModule"),
    table1,
    rule1)
  val otherModule = makeModule("OtherModule", table2)

  val importSystemBloom = ImportPackage("system.bloom")

  val userModules = ModuleContainer(Seq(
    importSystemBloom,
    module1,
    otherModule
  ))

  val program = Program(Seq(package1), Seq(userModules))
  val analyzer = new Analyzer(ProgramTree(program))

  test("can locate modules in a program") {
    analyzer.moduleDefinition("SomeModule") shouldBe Some(module1)
    analyzer.moduleDefinition("OtherModule") shouldBe Some(otherModule)
    analyzer.moduleDefinition("NotThereModule") shouldBe None
  }

  test("can find table on another module without alias") {
    definedSymbols(analyzer, rule1) shouldBe Set("SomeModule", "table1")
    definedSymbols(analyzer, table2) shouldBe Set("OtherModule", "table2")
    definedSymbols2(analyzer, rule1) shouldBe Set("OtherModule", "SomeModule", "table1", "table2")
  }

  test("report missing module on import") {
    val test  = makeTestProgram()(module1)
    test.errorLabels shouldBe Seq("Module 'OtherModule' not defined.")
  }
  
  test("report missing package import") {
    val test = makeTestProgram()(importSystemBloom, otherModule)
    test.errorLabels shouldBe Seq("Package 'system.bloom' not found.")
  }

  test("report missing table definition") {
    val test = makeTestProgram()(Module(IdnDef("a"), Seq(rule1)))
    test.errorLabels shouldBe Seq("Unknown collection 'table1'.")
  }

  test("report double definition of a symbol") {
    val test = makeTestProgram()(makeModule("a", table1, makeTable("table1")))
    test.errorLabels shouldBe Seq("Symbol 'table1' was defined multiple times.")
  }

  test("report double definition of a symbol in module import and local") {
    val test = makeTestProgram()(
      otherModule,
      makeModule("a", ImportModule("OtherModule"), makeTable("table2")))
    test.errorLabels shouldBe Seq("Symbol 'table2' was defined multiple times.")
  }

  test("report double definition of a symbol in module two different modules") {
    val test = makeTestProgram()(
      makeModule("A", makeTable("table1")),
      makeModule("B", makeTable("table1")),
      makeModule("C", ImportModule("A"), ImportModule("B"), rule1))
    test.errorLabels shouldBe Seq("Symbol 'table1' was defined multiple times.")
  }

  test("good program should have no messages") {
    analyzer.errors shouldBe noMessages
  }

  private def makeTestProgram(pkgs: Package*)(stmts: Node*) =
    new TestAnalyzer(Program(pkgs, Seq(ModuleContainer(stmts))))

  private def makeModule(moduleName:String, stmts: Node*) =
    Module(IdnDef(moduleName), stmts)

  private def makeTable(tableName: String): Table = {
    Table(IdnDef(tableName))
  }

  private def definedSymbols(analyzer: Analyzer, node: Node) = analyzer.defEnv(node).head.keySet

  private def definedSymbols2(analyzer: Analyzer, node: Node) = analyzer.defModuleEnv(node).head.keySet

  class TestAnalyzer(val program: Program) {
    val analyzer = new Analyzer(ProgramTree(program))
    def errorLabels = analyzer.errors.map(_.label)
  }

}