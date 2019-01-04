package millfork.assembly.mos.opt

import millfork.assembly._
import millfork.assembly.mos._
import millfork.assembly.opt.FlowCache
import millfork.env._
import millfork.node.MosRegister

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
                         r2: Importance = UnknownImportance,
                         r3: Importance = UnknownImportance,
                        ) {

  def setPseudoRegister(regOffset: Int, importance: Importance): CpuImportance = regOffset match {
    case 0 => this.copy(r0 = importance)
    case 1 => this.copy(r1 = importance)
    case 2 => this.copy(r2 = importance)
    case 3 => this.copy(r3 = importance)
    case _ => this
  }
  def setPseudoRegisterWord(regOffset: Int, importance: Importance): CpuImportance = regOffset match {
    case 0 => this.copy(r0 = importance, r1 = importance)
    case 1 => this.copy(r1 = importance, r2 = importance)
    case 2 => this.copy(r2 = importance, r3 = importance)
    case 3 => this.copy(r3 = importance)
    case _ => this
  }

  override def toString: String = s"A=$a,B=$ah,X=$x,Y=$y,Z=$iz; Z=$z,N=$n,C=$c,V=$v,D=$d,M=$m,X=$w; R0=$r0,R1=$r1,R2=$r2,R3=$r3"

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
    r2 = this.r2 ~ that.r2,
    r3 = this.r3 ~ that.r3,
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
    case 2 => r2 != Important
    case 3 => r3 != Important
    case _ => false
  }
}

object ReverseFlowAnalyzer {

  val cache = new FlowCache[AssemblyLine, CpuImportance]("mos reverse")

  val functionsThatReadC = Set("__adc_decimal", "__sbc_decimal")
  private val aluAdders = Set(Opcode.ADC, Opcode.SBC, Opcode.ISC, Opcode.DCP, Opcode.ADC_W, Opcode.SBC_W)
  private val readAsPointer = Set(AddrMode.IndexedZ, AddrMode.IndexedSY, AddrMode.IndexedY, AddrMode.LongIndexedY, AddrMode.LongIndexedZ, AddrMode.IndexedX, AddrMode.Indirect, AddrMode.AbsoluteIndexedX)
  private val absoluteLike = Set(AddrMode.ZeroPage, AddrMode.Absolute, AddrMode.LongAbsolute)
  private val importanceBeforeJsr: CpuImportance = CpuImportance(
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
    r1 = Unimportant,
    r2 = Unimportant,
    r3 = Unimportant)
  private val finalImportance: CpuImportance = CpuImportance(
    a = Important, ah = Important,
    x = Important, y = Important, iz = Important,
    c = Important, v = Important, d = Important, z = Important, n = Important,
    m = Important, w = Important,
    r0 = Important, r1 = Important, r2 = Important, r3 = Important)

  //noinspection RedundantNewCaseClass
  def analyze(f: NormalFunction, code: List[AssemblyLine], optimizationContext: OptimizationContext): List[CpuImportance] = {
    cache.get(code).foreach(return _)
    val niceFunctionProperties = optimizationContext.niceFunctionProperties
    val importanceArray = Array.fill[CpuImportance](code.length)(new CpuImportance())
    val codeArray = code.toArray

    var changed = true
    changed = true
    while (changed) {
      changed = false
      var currentImportance = finalImportance
      for (i <- codeArray.indices.reverse) {
        import millfork.assembly.mos.Opcode._
        import AddrMode._
        import millfork.node.MosNiceFunctionProperty._
        if (importanceArray(i) != currentImportance) {
          changed = true
          importanceArray(i) = currentImportance
        }
        val currentLine = codeArray(i)
        currentLine match {
          case AssemblyLine0(opcode, Relative | LongRelative, MemoryAddressConstant(Label(l))) if OpcodeClasses.ShortConditionalBranching(opcode) =>
            val L = l
            val labelIndex = codeArray.indexWhere {
              case AssemblyLine0(LABEL, _, MemoryAddressConstant(Label(L))) => true
              case _ => false
            }
            currentImportance = if (labelIndex < 0) finalImportance else importanceArray(labelIndex) ~ currentImportance
          case _ =>
        }
        currentLine match {

          case AssemblyLine0(JSR | JMP, Absolute | LongAbsolute, MemoryAddressConstant(fun: FunctionInMemory)) =>
            // this case has to be handled first, because the generic JSR importance handler is too conservative
            var result = importanceBeforeJsr
            fun.params match {
              case AssemblyParamSignature(params) =>
                params.foreach(_.variable match {
                  case RegisterVariable(MosRegister.A, _) =>
                    result = result.copy(a = Important)
                  case RegisterVariable(MosRegister.AW, _) =>
                    result = result.copy(a = Important, ah = Important)
                  case RegisterVariable(MosRegister.X, _) =>
                    result = result.copy(x = Important)
                  case RegisterVariable(MosRegister.Y, _) =>
                    result = result.copy(y = Important)
                  case RegisterVariable(MosRegister.AX | MosRegister.XA, _) =>
                    result = result.copy(a = Important, x = Important)
                  case RegisterVariable(MosRegister.YA | MosRegister.YA, _) =>
                    result = result.copy(a = Important, y = Important)
                  case RegisterVariable(MosRegister.XY | MosRegister.YX, _) =>
                    result = result.copy(x = Important, y = Important)
                  case _ =>
                })
              case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 1 =>
                result = result.copy(a = Important)
              case _ =>
            }
            currentImportance = result.copy(
              a = if (niceFunctionProperties(DoesntChangeA -> fun.name)) currentImportance.a ~ result.a else result.a,
              ah = if (niceFunctionProperties(DoesntChangeAH -> fun.name)) currentImportance.ah ~ result.ah else result.ah,
              x = if (niceFunctionProperties(DoesntChangeX -> fun.name)) currentImportance.x ~ result.x else result.x,
              y = if (niceFunctionProperties(DoesntChangeY -> fun.name)) currentImportance.y ~ result.y else result.y,
              iz = if (niceFunctionProperties(DoesntChangeIZ -> fun.name)) currentImportance.iz ~ result.iz else result.iz,
              c = if (functionsThatReadC(fun.name)) Important else if (niceFunctionProperties(DoesntChangeC -> fun.name)) currentImportance.c ~ result.c else result.c,
              d = if (niceFunctionProperties(DoesntConcernD -> fun.name)) currentImportance.d else result.d,
              r0 =
                if (ZeropageRegisterOptimizations.functionsThatUsePseudoregisterAsInput.getOrElse(fun.name, Set())(0))
                  Important
                else if (niceFunctionProperties(DoesntChangeZpRegister -> fun.name))
                  currentImportance.r0 ~ result.r0
                else result.r0,
              r1 =
                if (ZeropageRegisterOptimizations.functionsThatUsePseudoregisterAsInput.getOrElse(fun.name, Set())(1))
                  Important
                else if (niceFunctionProperties(DoesntChangeZpRegister -> fun.name))
                  currentImportance.r1 ~ result.r1
                else result.r1,
              r2 =
                if (ZeropageRegisterOptimizations.functionsThatUsePseudoregisterAsInput.getOrElse(fun.name, Set())(2))
                  Important
                else if (niceFunctionProperties(DoesntChangeZpRegister -> fun.name))
                  currentImportance.r2 ~ result.r2
                else result.r2,
              r3 =
                if (ZeropageRegisterOptimizations.functionsThatUsePseudoregisterAsInput.getOrElse(fun.name, Set())(3))
                  Important
                else if (niceFunctionProperties(DoesntChangeZpRegister -> fun.name))
                  currentImportance.r3 ~ result.r3
                else result.r3
            )

          case AssemblyLine0(ANC, _, NumericConstant(0, _)) =>
            currentImportance = currentImportance.copy(c = Unimportant, n = Unimportant, z = Unimportant, a = Unimportant)
          case AssemblyLine0(AND, _, NumericConstant(0, _)) =>
            currentImportance = currentImportance.copy(n = Unimportant, z = Unimportant, a = Unimportant)

          case AssemblyLine0(opcode, Implied, _) if ReverseFlowAnalyzerPerImpiedOpcode.hasDefinition(opcode) =>
            currentImportance = ReverseFlowAnalyzerPerImpiedOpcode.get(opcode)(currentImportance)

          case AssemblyLine0(opcode, addrMode, _) if addrMode != Implied && ReverseFlowAnalyzerPerOpcode.hasDefinition(opcode) =>
            currentImportance = ReverseFlowAnalyzerPerOpcode.get(opcode)(currentImportance)
            if (addrMode == AbsoluteX || addrMode == LongAbsoluteX || addrMode == IndexedX || addrMode == ZeroPageX || addrMode == AbsoluteIndexedX)
              currentImportance = currentImportance.copy(x = Important)
            else if (addrMode == AbsoluteY || addrMode == IndexedY || addrMode == ZeroPageY || addrMode == LongIndexedY || addrMode == IndexedSY)
              currentImportance = currentImportance.copy(y = Important)
            else if (addrMode == IndexedZ /*|| addrMode == LongIndexedZ*/ )
              currentImportance = currentImportance.copy(iz = Important)

          case AssemblyLine0(JMP | BRA, Absolute | Relative | LongAbsolute | LongRelative, MemoryAddressConstant(Label(l))) =>
            val L = l
            val labelIndex = codeArray.indexWhere {
              case AssemblyLine0(LABEL, _, MemoryAddressConstant(Label(L))) => true
              case _ => false
            }
            currentImportance = if (labelIndex < 0) finalImportance else importanceArray(labelIndex)

          case AssemblyLine0(JMP, _, _) =>
            currentImportance = finalImportance

          case AssemblyLine0(REP | SEP, _, NumericConstant(n, _)) =>
            if ((n & 1) != 0) currentImportance = currentImportance.copy(c = Unimportant)
            if ((n & 2) != 0) currentImportance = currentImportance.copy(z = Unimportant)
            if ((n & 8) != 0) currentImportance = currentImportance.copy(d = Unimportant)
            if ((n & 0x10) != 0) currentImportance = currentImportance.copy(w = Unimportant)
            if ((n & 0x20) != 0) currentImportance = currentImportance.copy(m = Unimportant)
            if ((n & 0x40) != 0) currentImportance = currentImportance.copy(v = Unimportant)
            if ((n & 0x80) != 0) currentImportance = currentImportance.copy(n = Unimportant)

          case AssemblyLine0(opcode, addrMode, _) =>
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
        val isAbsoluteLike = absoluteLike(currentLine.addrMode)
        val isReadAsPointer = readAsPointer(currentLine.addrMode)
        if (isAbsoluteLike || isReadAsPointer) {
          (currentLine.parameter match {
            case MemoryAddressConstant(th: Thing)
              if th.name == "__reg" =>
              Some(0)
            case CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th: Thing), NumericConstant(n, _))
              if th.name == "__reg" =>
              Some(n.toInt)
            case _ =>
              None
          }) match {
            case None =>
            case Some(regOffset) =>
              if (isAbsoluteLike) {
                if (OpcodeClasses.StoresByte(currentLine.opcode)) {
                  currentImportance = currentImportance.setPseudoRegister(regOffset, Unimportant)
                } else if (OpcodeClasses.StoresWord(currentLine.opcode)) {
                  currentImportance = currentImportance.setPseudoRegisterWord(regOffset, Unimportant)
                }
                if (OpcodeClasses.ReadsMemoryIfNotImpliedOrImmediate(currentLine.opcode)) {
                  if (OpcodeClasses.AccessesWordInMemory(currentLine.opcode)) {
                    currentImportance = currentImportance.setPseudoRegisterWord(regOffset, Important)
                  } else {
                    currentImportance = currentImportance.setPseudoRegister(regOffset, Important)
                  }
                }
              }
              if (isReadAsPointer) {
                currentImportance = currentImportance.setPseudoRegisterWord(regOffset, Important)
              }
          }
        }
      }
    }
//            importanceArray.zip(codeArray).foreach{
//              case (i, y) => if (y.isPrintable) println(f"$y%-32s $i%-32s")
//            }
//            println("---------------------")

    cache.put(code, importanceArray.toList)
  }
}
