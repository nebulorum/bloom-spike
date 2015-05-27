package org.bloomlang.bloom.compiler.analyzer

import org.bloomlang.bloom.compiler.ast._
import org.scalatest.FunSuite

class AnalyserTest extends FunSuite {

  val module1 = Module("SomeModule")
  val module2 = Module("OtherModule")
  val package1 = Package("system.bloom")

  val userModules = ModuleContainer(Seq(
    ImportPackage("system.bloom"),
    module1,
    module2
  ))

  val program = Program(Seq(package1), Seq(userModules))

  test("can locate modules in a program") {
    val analyzer = new Analyzer(ProgramTree(program))
    assert(analyzer.moduleDef("SomeModule") == Some(module1))
    assert(analyzer.moduleDef("OtherModule") == Some(module2))
    assert(analyzer.moduleDef("NotThereModule") == None)
  }
}