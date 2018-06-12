package millfork.assembly.mos.opt

import millfork.{CompilationFlag, CompilationOptions, NonOverlappingIntervals}
import millfork.assembly._
import millfork.assembly.mos.Opcode._
import millfork.assembly.AddrMode._
import millfork.assembly.mos.{AssemblyLine, Opcode}
import millfork.env._
import millfork.error.ErrorReporting

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * @author Karol Stasiak
  */
object SingleAssignmentVariableOptimization extends AssemblyOptimization[AssemblyLine] {

  private val BadOpcodes = Set(RTS, JSR, JMP, RTI)
  private val GoodOpcodes = Set(
    LDA, LDX, LAX, LDY, EOR, AND, ORA, ADC, SBC, CMP, CPX, CPY, BIT,
    JMP, BRA, BNE, BEQ, BVS, BVC, BCC, BCS, BPL, BMI,
    INX, INY, DEX, DEY,
    TAX, TAY, TYA, TXA, TXS, TSX,
    PLA, PLY, PLX, PHA, PHY, PHX,
    ANC, SBX, XAA, LXA,
  )
  private val CpxyAddrModes = Set(Absolute, ZeroPage, Immediate)
  private val MostAddrModes = Set(AbsoluteX, AbsoluteY, IndexedY, IndexedX, ZeroPageX, Absolute, ZeroPage, Immediate)
  private val LdxAddrModes = Set(AbsoluteY, Absolute, ZeroPage, Immediate)
  private val LdyAddrModes = Set(AbsoluteX, ZeroPageX, Absolute, ZeroPage, Immediate)

  override def name = "Single assignment variable optimization"


  override def optimize(f: NormalFunction, code: List[AssemblyLine], options: CompilationOptions): List[AssemblyLine] = {
    val paramVariables = f.params match {
      case NormalParamSignature(ps) =>
        ps.map(_.name).toSet
      case _ =>
        // assembly functions do not get this optimization
        return code
    }
    val stillUsedVariables = code.flatMap {
      case AssemblyLine(_, _, MemoryAddressConstant(th), _) => Some(th.name)
      case _ => None
    }.toSet
    val variablesWithAddressesTaken = code.flatMap {
      case AssemblyLine(_, _, SubbyteConstant(MemoryAddressConstant(th), _), _) => Some(th.name)
      case _ => None
    }.toSet
    val localVariables = f.environment.getAllLocalVariables.filter {
      case MemoryVariable(name, typ, VariableAllocationMethod.Auto | VariableAllocationMethod.Register) =>
        typ.size == 1 && !paramVariables(name) && stillUsedVariables(name) && !variablesWithAddressesTaken(name)
      case _ => false
    }
    if (localVariables.isEmpty) return code

    val variablesWithSources = localVariables.flatMap(v => findSourceForVariable(v.name, code).toOption.map(v -> _))

    val finalGoodVariables = variablesWithSources.filter{case (v, source) =>
      val lifetime = VariableLifetime(v.name, code)
      val slice = code.slice(lifetime.start, lifetime.end)
      slice.forall(l => GoodOpcodes.contains(l.opcode) || !BadOpcodes.contains(l.opcode) && source.forall(s =>  HelperCheckers.memoryAccessDoesntOverlap(s,l)))
    }.flatMap{case (v,source) =>
      replaceVariable(v.name, source, code.zip(ReverseFlowAnalyzer.analyze(f, code))).map(v -> _)
    }

    if (finalGoodVariables.isEmpty) return code
    val (bestVar, bestCode) = finalGoodVariables.minBy(_._2.view.map(_.sizeInBytes).sum)
    ErrorReporting.debug(s"Removing effectively const variable ${bestVar.name}")
    reportOptimizedBlock(code, bestCode)
    bestCode
  }

  private def findSourceForVariable(variable: String, code:List[AssemblyLine]): Either[Boolean, List[AssemblyLine]] = code match {
    case Nil => Left(true)
    case (load@AssemblyLine(LDA, _, _, _)) :: AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(v), true) :: xs
      if v.name == variable =>
      findSourceForVariable(variable, xs) match {
        case Left(true) => Right(List(load))
        case _ => Left(false)
      }
    case AssemblyLine(LAX | LDX | LDY | ADC | SBC | CMP | CPX | CPY | AND | EOR | ORA, Absolute | ZeroPage, MemoryAddressConstant(v), _) :: xs
      if v.name == variable =>
      findSourceForVariable(variable, xs)
    case AssemblyLine(_, _, MemoryAddressConstant(v), _) :: _
      if v.name == variable => Left(false)
    case x :: xs => findSourceForVariable(variable, xs)
  }

  private def isSingleLda(replacement: List[AssemblyLine], addrModes: Set[AddrMode.Value]): Boolean = {
    if (replacement.length != 1) return false
    val line = replacement.head
    line.opcode == LDA && addrModes(line.addrMode)
  }

  private def getAddrModes(opcode: Opcode.Value): Set[AddrMode.Value] = opcode match {
    case LDX => LdxAddrModes
    case LDY => LdyAddrModes
    case CPX | CPY => CpxyAddrModes
    case _ => MostAddrModes
  }

  private def replaceVariable(variable: String, replacement: List[AssemblyLine], code: List[(AssemblyLine, CpuImportance)]): Option[List[AssemblyLine]] = code match {
    case Nil => Some(Nil)
    case (AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(v), true), _) :: xs
      if v.name == variable =>
      replaceVariable(variable, replacement, xs)
    case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(v), true), imp) :: xs
      if v.name == variable =>
      replaceVariable(variable, replacement, xs).map(replacement ++ _)
    case (AssemblyLine(op@(LAX | LDX | LDY | ADC | SBC | CMP | CPX | CPY | AND | EOR | ORA), Absolute | ZeroPage, MemoryAddressConstant(v), true), imp) :: xs
      if v.name == variable =>
      if (isSingleLda(replacement, getAddrModes(op))) replaceVariable(variable, replacement, xs).map(replacement.map(_.copy(opcode = op)) ++ _)
      else None
    case (AssemblyLine(_, _, MemoryAddressConstant(v), _), _) :: xs if v.name == variable => None
    case x :: xs => replaceVariable(variable, replacement, xs).map(x._1 :: _)
  }

  protected def reportOptimizedBlock(oldCode: List[AssemblyLine], newCode: List[AssemblyLine]): Unit = {
    oldCode.foreach(l => ErrorReporting.trace(l.toString))
    ErrorReporting.trace("     ↓")
    newCode.foreach(l => ErrorReporting.trace(l.toString))
  }
}
