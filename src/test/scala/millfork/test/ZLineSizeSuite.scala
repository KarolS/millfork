package millfork.test

import millfork.{CompilationOptions, Cpu, CpuFamily, JobContext}
import millfork.assembly.z80.{LocalVariableAddressViaIX, NoRegisters, ZLine}
import millfork.compiler.LabelGenerator
import millfork.env.{Constant, Environment, NumericConstant}
import millfork.output.Z80Assembler
import millfork.test.emu._
import org.scalatest.{FunSuite, Matchers}


/**
  * @author Karol Stasiak
  */
class ZLineSizeSuite extends FunSuite with Matchers {
  private def runCase(line: ZLine): Unit = {
    val platform = EmuPlatform.get(Cpu.Z80)
    val jobContext = JobContext(TestErrorReporting.log, new LabelGenerator)
    val env = new Environment(None, "", CpuFamily.I80, jobContext)
    val options = CompilationOptions(platform, Map(), None, 0, jobContext)
    val correctSize = new Z80Assembler(null, env, platform).emitInstruction("default", options, 0x100, line) - 0x100
    val guessedSize = line.sizeInBytes
    guessedSize should equal(correctSize)
  }

  test("Z80 instruction size") {
    import millfork.assembly.z80.ZOpcode._
    import millfork.node.ZRegister._
    import millfork.env.Constant.Zero
    runCase(ZLine.implied(RET))
    runCase(ZLine.implied(NEG))
    runCase(ZLine.implied(RETI))
    runCase(ZLine.implied(RETN))
    runCase(ZLine.implied(EX_DE_HL))
    runCase(ZLine.implied(HALT))
    runCase(ZLine.ldViaIx(0, A))
    runCase(ZLine.ld8(B, C))
    runCase(ZLine.ldImm8(B, 9))
    runCase(ZLine.ldAbs8(A, Zero))
    runCase(ZLine.ldAbs16(HL, Zero))
    runCase(ZLine.ldImm16(HL, Zero))
    runCase(ZLine.register(SLA, E))
    runCase(ZLine.register(BIT0, E))
    runCase(ZLine.register(JP, IX))
    runCase(ZLine.register(JP, HL))
    runCase(ZLine(IM, NoRegisters, NumericConstant(1, 1), true))
    runCase(ZLine.register(DEC, HL))
    runCase(ZLine.register(DEC, LocalVariableAddressViaIX(7)))
    runCase(ZLine.ld8(A, MEM_HL))
    runCase(ZLine.ld8(A, MEM_BC))
    runCase(ZLine.ld8(A, MEM_DE))
    runCase(ZLine.ld8(MEM_HL, A))
    runCase(ZLine.ld8(MEM_BC, A))
    runCase(ZLine.ld8(MEM_DE, A))
  }
}
