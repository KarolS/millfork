package millfork.assembly.mos.opt

import millfork.CompilationOptions
import millfork.assembly.AssemblyOptimization
import millfork.assembly.mos.{AssemblyLine, State}
import millfork.compiler.mos.MosReturnDispatch
import millfork.node.{MosNiceFunctionProperty, NiceFunctionProperty}
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos.AddrMode._

/**
  * @author Karol Stasiak
  */
//noinspection ZeroIndexToHead
object VeryLateMosAssemblyOptimizations {

  val StoresOfImmediatesDifferingByOneViaX = new RuleBasedAssemblyOptimization("Use X to store immediates differing by 1",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(LDA) & HasAddrMode(Immediate) & MatchNumericImmediate(0) & HasIndex8) ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(ZeroPage, Absolute) & DoesntMatterWhatItDoesWith(State.A, State.X)) ~
      (Linear & Not(HasOpcode(LDA) & HasAddrMode(Immediate))).*.capture(10) ~
      (Elidable & HasOpcode(LDA) & HasAddrMode(Immediate) & MatchNumericImmediate(1) & HasIndex8) ~
      Where(ctx => ctx.get[Int](0).+(1).&(0xff) == ctx.get[Int](1)) ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(ZeroPage, Absolute) & DoesntMatterWhatItDoesWith(State.A, State.X)) ~~> { code =>
      code(0).copy(opcode = LDX) ::
        code(1).copy(opcode = STX) :: (
        code.drop(2).dropRight(2) ++ List(
          AssemblyLine.implied(INX).pos(code(code.size - 2).source),
          code.last.copy(opcode = STX)))
    }
  )

  val StoresOfImmediatesDifferingByOneViaY = new RuleBasedAssemblyOptimization("Use Y to store immediates differing by 1",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(LDA) & HasAddrMode(Immediate) & MatchNumericImmediate(0) & HasIndex8) ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(ZeroPage, Absolute) & DoesntMatterWhatItDoesWith(State.A, State.Y)) ~
      (Linear & Not(HasOpcode(LDA) & HasAddrMode(Immediate))).*.capture(10) ~
      (Elidable & HasOpcode(LDA) & HasAddrMode(Immediate) & MatchNumericImmediate(1) & HasIndex8) ~
      Where(ctx => ctx.get[Int](0).+(1).&(0xff) == ctx.get[Int](1)) ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(ZeroPage, Absolute) & DoesntMatterWhatItDoesWith(State.A, State.Y)) ~~> { code =>
      code(0).copy(opcode = LDY) ::
        code(1).copy(opcode = STY) :: (
        code.drop(2).dropRight(2) ++ List(
          AssemblyLine.implied(INY).pos(code(code.size - 2).source),
          code.last.copy(opcode = STY)))
    }
  )

  def None(nice: Set[NiceFunctionProperty], options: CompilationOptions): Seq[AssemblyOptimization[AssemblyLine]] = Nil

  def All(nice: Set[NiceFunctionProperty], options: CompilationOptions): Seq[AssemblyOptimization[AssemblyLine]] = {
    val result = Seq.newBuilder[AssemblyOptimization[AssemblyLine]]
    if (!nice(MosNiceFunctionProperty.DoesntChangeX)){
      result += StoresOfImmediatesDifferingByOneViaX
    }
    if (!nice(MosNiceFunctionProperty.DoesntChangeY)){
      result += StoresOfImmediatesDifferingByOneViaY
    }
    result.result()
  }
}
