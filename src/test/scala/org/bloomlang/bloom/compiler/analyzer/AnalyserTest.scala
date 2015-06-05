package org.bloomlang.bloom.compiler.analyzer

import org.bloomlang.bloom.compiler.ast._
import org.scalatest._
import Matchers._
import org.kiama.util.Messaging._

class AnalyserTest extends FunSuite {

  val package1 = Package("system.bloom")

  val table1 = Table(IdnDef("table1"))
  val table2 = Table(IdnDef("table2"))
  val rule1 = Rule("table1")
  val module1 = Module(IdnDef("SomeModule"), Seq(
    ImportModule("OtherModule"),
    table1,
    rule1))
  val module2 = Module(IdnDef("OtherModule"), Seq(table2))

  val importSystemBloom = ImportPackage("system.bloom")

  val userModules = ModuleContainer(Seq(
    importSystemBloom,
    module1,
    module2
  ))

  val program = Program(Seq(package1), Seq(userModules))
  val analyzer = new Analyzer(ProgramTree(program))

  test("can locate modules in a program") {
    analyzer.moduleDefinition("SomeModule") shouldBe Some(module1)
    analyzer.moduleDefinition("OtherModule") shouldBe Some(module2)
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
    val test = makeTestProgram()(importSystemBloom, module2)
    test.errorLabels shouldBe Seq("Package 'system.bloom' not found.")
  }

  test("good program should have no messages") {
    analyzer.errors shouldBe noMessages
  }

  private def makeTestProgram(pkgs: Package*)(stmts: Node*) =
    new TestAnalyzer(Program(pkgs, Seq(ModuleContainer(stmts))))

  private def definedSymbols(analyzer: Analyzer, node: Node) = analyzer.defEnv(node).head.keySet

  private def definedSymbols2(analyzer: Analyzer, node: Node) = analyzer.defModuleEnv(node).head.keySet

  class TestAnalyzer(val program: Program) {
    val analyzer = new Analyzer(ProgramTree(program))
    def errorLabels = analyzer.errors.map(_.label)
  }

}