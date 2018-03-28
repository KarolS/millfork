package millfork.assembly.opt

import millfork.CompilationOptions
import millfork.assembly._
import millfork.env._
import millfork.error.ErrorReporting
import millfork.node.Register

import scala.collection.immutable

/**
  * @author Karol Stasiak
  */

sealed trait Importance {
  def ~(that: Importance): Importance = (this, that) match {
    case (_, Important) | (Important, _) => Important
    case (_, Unimportant) | (Unimportant, _) => Unimportant
    case (UnknownImportance, UnknownImportance) => UnknownImportance
  }
}

case object Important extends Importance {
  override def toString = "!"
}

case object Unimportant extends Importance {
  override def toString = "*"
}

case object UnknownImportance extends Importance {
  override def toString = "?"
}

//noinspection RedundantNewCaseClass
case class CpuImportance(a: Importance = UnknownImportance,
                         ah: Importance = UnknownImportance,
                         x: Importance = UnknownImportance,
                         y: Importance = UnknownImportance,
                         iz: Importance = UnknownImportance,
                         n: Importance = UnknownImportance,
                         z: Importance = UnknownImportance,
                         v: Importance = UnknownImportance,
                         c: Importance = UnknownImportance,
                         d: Importance = UnknownImportance,
                         m: Importance = UnknownImportance,
                         w: Importance = UnknownImportance,
                         r0: Importance = UnknownImportance,
                         r1: Importance = UnknownImportance,
                        ) {
  override def toString: String = s"A=$a,B=$ah,X=$x,Y=$y,Z=$iz; Z=$z,N=$n,C=$c,V=$v,D=$d,M=$m,X=$w; R0=$r0,R1:$r1"

  def ~(that: CpuImportance) = new CpuImportance(
    a = this.a ~ that.a,
    x = this.x ~ that.x,
    y = this.y ~ that.y,
    iz = this.iz ~ that.iz,
    z = this.z ~ that.z,
    n = this.n ~ that.n,
    c = this.c ~ that.c,
    v = this.v ~ that.v,
    d = this.d ~ that.d,
    m = this.m ~ that.m,
    w = this.w ~ that.w,
    r0 = this.r0 ~ that.r0,
    r1 = this.r1 ~ that.r1,
  )

  def isUnimportant(state: State.Value): Boolean = state match {
    // UnknownImportance is usually an effect of unreachable code
    case State.A => a != Important
    case State.AH => ah != Important
    case State.X => x != Important
    case State.Y => y != Important
    case State.IZ => iz != Important
    case State.Z => z != Important
    case State.N => n != Important
    case State.C => c != Important
    case State.V => v != Important
    case State.D => d != Important
    case State.M => m != Important
    case State.W => w != Important
  }

  def isPseudoregisterUnimportant(index: Int): Boolean = index match {
    case 0 => r0 != Important
    case 1 => r1 != Important
    case _ => false
  }
}

object ReverseFlowAnalyzer {

  val aluAdders = Set(Opcode.ADC, Opcode.SBC, Opcode.ISC, Opcode.DCP, Opcode.ADC_W, Opcode.SBC_W)
  val actuallyRead = Set(AddrMode.IndexedZ, AddrMode.IndexedSY, AddrMode.IndexedY, AddrMode.LongIndexedY, AddrMode.LongIndexedZ, AddrMode.IndexedX, AddrMode.Indirect, AddrMode.AbsoluteIndexedX)
  val absoluteLike = Set(AddrMode.ZeroPage, AddrMode.Absolute, AddrMode.LongAbsolute)

  //noinspection RedundantNewCaseClass
  def analyze(f: NormalFunction, code: List[AssemblyLine]): List[CpuImportance] = {
    val importanceArray = Array.fill[CpuImportance](code.length)(new CpuImportance())
    val codeArray = code.toArray

    var changed = true
    val finalImportance = new CpuImportance(
      a = Important, ah = Important,
      x = Important, y = Important, iz = Important,
      c = Important, v = Important, d = Important, z = Important, n = Important,
      m = Important, w = Important,
      r0 = Important, r1 = Important)
    changed = true
    while (changed) {
      changed = false
      var currentImportance: CpuImportance = finalImportance
      for (i <- codeArray.indices.reverse) {
        import millfork.assembly.Opcode._
        import millfork.assembly.AddrMode._
        if (importanceArray(i) != currentImportance) {
          changed = true
          importanceArray(i) = currentImportance
        }
        val currentLine = codeArray(i)
        currentLine match {
          case AssemblyLine(opcode, Relative | LongRelative, MemoryAddressConstant(Label(l)), _) if OpcodeClasses.ShortConditionalBranching(opcode) =>
            val L = l
            val labelIndex = codeArray.indexWhere {
              case AssemblyLine(LABEL, _, MemoryAddressConstant(Label(L)), _) => true
              case _ => false
            }
            currentImportance = if (labelIndex < 0) finalImportance else importanceArray(labelIndex) ~ currentImportance
          case _ =>
        }
        currentLine match {

          case AssemblyLine(JSR | JMP, Absolute | LongAbsolute, MemoryAddressConstant(fun: FunctionInMemory), _) =>
            // this case has to be handled first, because the generic JSR importance handler is too conservative
            var result = new CpuImportance(
              a = Unimportant,
              ah = Unimportant,
              x = Unimportant,
              y = Unimportant,
              iz = Unimportant,
              z = Unimportant,
              n = Unimportant,
              c = Unimportant,
              v = Unimportant,
              d = Important,
              m = Important,
              w = Important,
              r0 = Unimportant,
              r1 = Unimportant)
            fun.params match {
              case AssemblyParamSignature(params) =>
                params.foreach(_.variable match {
                  case RegisterVariable(Register.A, _) =>
                    result = result.copy(a = Important)
                  case RegisterVariable(Register.AW, _) =>
                    result = result.copy(a = Important, ah = Important)
                  case RegisterVariable(Register.X, _) =>
                    result = result.copy(x = Important)
                  case RegisterVariable(Register.Y, _) =>
                    result = result.copy(y = Important)
                  case RegisterVariable(Register.AX | Register.XA, _) =>
                    result = result.copy(a = Important, x = Important)
                  case RegisterVariable(Register.YA | Register.YA, _) =>
                    result = result.copy(a = Important, y = Important)
                  case RegisterVariable(Register.XY | Register.YX, _) =>
                    result = result.copy(x = Important, y = Important)
                  case _ =>
                })
              case _ =>
            }
            if (ZeropageRegisterOptimizations.functionsThatUsePseudoregisterAsInput(fun.name)) {
              result = result.copy(r0 = Important, r1 = Important)
            }
            currentImportance = result

          case AssemblyLine(ANC, _, NumericConstant(0, _), _) =>
            currentImportance = currentImportance.copy(c = Unimportant, n = Unimportant, z = Unimportant, a = Unimportant)
          case AssemblyLine(AND, _, NumericConstant(0, _), _) =>
            currentImportance = currentImportance.copy(n = Unimportant, z = Unimportant, a = Unimportant)

          case AssemblyLine(opcode, addrMode, _, _) if ReverseFlowAnalyzerPerOpcode.hasDefinition(opcode) =>
            currentImportance = ReverseFlowAnalyzerPerOpcode.get(opcode)(currentImportance)
            if (addrMode == AbsoluteX || addrMode == LongAbsoluteX || addrMode == IndexedX || addrMode == ZeroPageX || addrMode == AbsoluteIndexedX)
              currentImportance = currentImportance.copy(x = Important)
            else if (addrMode == AbsoluteY || addrMode == IndexedY || addrMode == ZeroPageY || addrMode == LongIndexedY || addrMode == IndexedSY)
              currentImportance = currentImportance.copy(y = Important)
            else if (addrMode == IndexedZ /*|| addrMode == LongIndexedZ*/ )
              currentImportance = currentImportance.copy(iz = Important)

          case AssemblyLine(JMP | BRA, Absolute | Relative | LongAbsolute | LongRelative, MemoryAddressConstant(Label(l)), _) =>
            val L = l
            val labelIndex = codeArray.indexWhere {
              case AssemblyLine(LABEL, _, MemoryAddressConstant(Label(L)), _) => true
              case _ => false
            }
            currentImportance = if (labelIndex < 0) finalImportance else importanceArray(labelIndex)

          case AssemblyLine(JMP, Indirect | AbsoluteIndexedX | LongIndirect, _, _) =>
            currentImportance = finalImportance

          case AssemblyLine(REP | SEP, _, NumericConstant(n, _), _) =>
            if ((n & 1) != 0) currentImportance = currentImportance.copy(c = Unimportant)
            if ((n & 2) != 0) currentImportance = currentImportance.copy(z = Unimportant)
            if ((n & 8) != 0) currentImportance = currentImportance.copy(d = Unimportant)
            if ((n & 0x10) != 0) currentImportance = currentImportance.copy(w = Unimportant)
            if ((n & 0x20) != 0) currentImportance = currentImportance.copy(m = Unimportant)
            if ((n & 0x40) != 0) currentImportance = currentImportance.copy(v = Unimportant)
            if ((n & 0x80) != 0) currentImportance = currentImportance.copy(n = Unimportant)

          case AssemblyLine(opcode, addrMode, _, _) =>
            val reallyIgnoreC =
              currentImportance.c == Unimportant &&
                currentImportance.v == Unimportant &&
                currentImportance.n == Unimportant &&
                currentImportance.z == Unimportant &&
                currentImportance.a == Unimportant &&
                aluAdders.contains(opcode)
            val reallyIgnoreA =
              currentImportance.c == Unimportant &&
                currentImportance.v == Unimportant &&
                currentImportance.n == Unimportant &&
                currentImportance.z == Unimportant &&
                currentImportance.a == Unimportant &&
                aluAdders.contains(opcode)
            if (OpcodeClasses.ChangesC(opcode)) currentImportance = currentImportance.copy(c = Unimportant)
            if (OpcodeClasses.ChangesV(opcode)) currentImportance = currentImportance.copy(v = Unimportant)
            if (OpcodeClasses.ChangesNAndZ(opcode)) currentImportance = currentImportance.copy(n = Unimportant, z = Unimportant)
            if (OpcodeClasses.OverwritesA(opcode)) currentImportance = currentImportance.copy(a = Unimportant)
            if (OpcodeClasses.OverwritesAH(opcode)) currentImportance = currentImportance.copy(ah = Unimportant)
            if (OpcodeClasses.OverwritesX(opcode)) currentImportance = currentImportance.copy(x = Unimportant)
            if (OpcodeClasses.OverwritesY(opcode)) currentImportance = currentImportance.copy(y = Unimportant)
            if (OpcodeClasses.OverwritesIZ(opcode)) currentImportance = currentImportance.copy(iz = Unimportant)
            if (OpcodeClasses.ReadsC(opcode) && !reallyIgnoreC) currentImportance = currentImportance.copy(c = Important)
            if (OpcodeClasses.ReadsD(opcode)) currentImportance = currentImportance.copy(d = Important)
            if (OpcodeClasses.ReadsV(opcode)) currentImportance = currentImportance.copy(v = Important)
            if (OpcodeClasses.ReadsXAlways(opcode)) currentImportance = currentImportance.copy(x = Important)
            if (OpcodeClasses.ReadsYAlways(opcode)) currentImportance = currentImportance.copy(y = Important)
            if (OpcodeClasses.ReadsIZAlways(opcode)) currentImportance = currentImportance.copy(iz = Important)
            if (OpcodeClasses.ReadsM(opcode)) currentImportance = currentImportance.copy(m = Important)
            if (OpcodeClasses.ReadsW(opcode)) currentImportance = currentImportance.copy(w = Important)
            if (OpcodeClasses.ReadsAAlways(opcode) && !reallyIgnoreA) currentImportance = currentImportance.copy(a = Important)
            if (OpcodeClasses.ReadsAHAlways(opcode)) currentImportance = currentImportance.copy(ah = Important)
            if (OpcodeClasses.ReadsAIfImplied(opcode) && addrMode == Implied) currentImportance = currentImportance.copy(a = Important)
            if (OpcodeClasses.ReadsAHIfImplied(opcode) && addrMode == Implied) currentImportance = currentImportance.copy(ah = Important)
            if (addrMode == AbsoluteX || addrMode == LongAbsoluteX || addrMode == IndexedX || addrMode == ZeroPageX || addrMode == AbsoluteIndexedX)
              currentImportance = currentImportance.copy(x = Important)
            else if (addrMode == AbsoluteY || addrMode == IndexedY || addrMode == ZeroPageY || addrMode == LongIndexedY || addrMode == IndexedSY)
              currentImportance = currentImportance.copy(y = Important)
            else if (addrMode == IndexedZ /*|| addrMode == LongIndexedZ*/ )
              currentImportance = currentImportance.copy(iz = Important)
        }
        if (absoluteLike(currentLine.addrMode)) {
          if (OpcodeClasses.StoresByte(currentLine.opcode)) {
            currentLine.parameter match {
              case MemoryAddressConstant(th: Thing)
                if th.name == "__reg" => currentImportance = currentImportance.copy(r0 = Unimportant)
              case CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th: Thing), NumericConstant(1, _))
                if th.name == "__reg" => currentImportance = currentImportance.copy(r1 = Unimportant)
              case _ => ()
            }
          }
          if (OpcodeClasses.StoresWord(currentLine.opcode)) {
            currentLine.parameter match {
              case MemoryAddressConstant(th: Thing)
                if th.name == "__reg" => currentImportance = currentImportance.copy(r0 = Unimportant, r1 = Unimportant)
              case _ => ()
            }
          }
        }
        if (actuallyRead(currentLine.addrMode)) {
          currentLine.parameter match {
            case MemoryAddressConstant(th: Thing)
              if th.name == "__reg" => currentImportance = currentImportance.copy(r0 = Important, r1 = Important)
            case _ => ()
          }
        } else if (OpcodeClasses.ReadsM(currentLine.opcode) || OpcodeClasses.ReadsMemoryIfNotImpliedOrImmediate(currentLine.opcode)) {
          if (OpcodeClasses.AccessesWordInMemory(currentLine.opcode)) {
            currentLine.parameter match {
              case MemoryAddressConstant(th: Thing)
                if th.name == "__reg" => currentImportance = currentImportance.copy(r0 = Important, r1 = Important)
              case _ => ()
            }
          } else {
            currentLine.parameter match {
              case MemoryAddressConstant(th: Thing)
                if th.name == "__reg" => currentImportance = currentImportance.copy(r0 = Important)
              case CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th: Thing), NumericConstant(1, _))
                if th.name == "__reg" => currentImportance = currentImportance.copy(r1 = Important)
              case _ => ()
            }
          }
        }
      }
    }
    //        importanceArray.zip(codeArray).foreach{
    //          case (i, y) => if (y.isPrintable) println(f"$y%-32s $i%-32s")
    //        }
    //        println("---------------------")

    importanceArray.toList
  }
}
