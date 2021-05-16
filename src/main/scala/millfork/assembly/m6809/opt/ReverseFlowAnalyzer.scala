package millfork.assembly.m6809.opt

import jdk.jfr.BooleanFlag
import millfork.assembly._
import millfork.assembly.m6809.{Absolute, MLine, MLine0, MOpcode, MState}
import millfork.assembly.opt.FlowCache
import millfork.env._
import millfork.node.M6809NiceFunctionProperty.DoesntChangeB
import millfork.node.{M6809Register, MosRegister}

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
                         b: Importance = UnknownImportance,
                         x: Importance = UnknownImportance,
                         y: Importance = UnknownImportance,
                         u: Importance = UnknownImportance,
                         nf: Importance = UnknownImportance,
                         zf: Importance = UnknownImportance,
                         vf: Importance = UnknownImportance,
                         cf: Importance = UnknownImportance,
                         hf: Importance = UnknownImportance,
                        ) {


  override def toString: String = s"A=$a,B=$b,X=$x,Y=$y; Z=$zf,N=$nf,C=$cf,V=$vf,H=$hf"

  def ~(that: CpuImportance) = new CpuImportance(
    a = this.a ~ that.a,
    b = this.b ~ that.b,
    x = this.x ~ that.x,
    y = this.y ~ that.y,
    u = this.u ~ that.u,
    nf = this.nf ~ that.nf,
    cf = this.cf ~ that.cf,
    hf = this.hf ~ that.hf,
    vf = this.vf ~ that.vf,
    zf = this.zf ~ that.zf,
  )

  def isUnimportant(state: MState.Value): Boolean = state match {
    // UnknownImportance is usually an effect of unreachable code
    case MState.A => a != Important
    case MState.B => b != Important
    case MState.X => x != Important
    case MState.Y => y != Important
    case MState.U => u != Important
    case MState.ZF => zf != Important
    case MState.NF => nf != Important
    case MState.CF => cf != Important
    case MState.VF => vf != Important
    case MState.HF => hf != Important
  }

}

object ReverseFlowAnalyzer {
  val readsA: Set[String] = Set("call")
  val readsB: Set[String] = Set("call")
  val readsX: Set[String] = Set("call")
  val readsY: Set[String] = Set("")

  val cache = new FlowCache[MLine, CpuImportance]("m6809 reverse")
  private val importanceBeforeJsr: CpuImportance = CpuImportance(
    a = Unimportant,
    b = Unimportant,
    x = Unimportant,
    y = Unimportant,
    u = Important,
    zf = Unimportant,
    nf = Unimportant,
    cf = Unimportant,
    vf = Unimportant,
    hf = Unimportant)
  private val finalImportance: CpuImportance = CpuImportance(
    a = Important, b = Important,
    x = Important, y = Important, u = Important,
    cf = Unimportant, vf = Unimportant, hf = Unimportant, zf = Unimportant, nf = Unimportant)

  //noinspection RedundantNewCaseClass
  def analyze(f: NormalFunction, code: List[MLine], optimizationContext: OptimizationContext): List[CpuImportance] = {
    cache.get(code).foreach(return _)
    val niceFunctionProperties = optimizationContext.niceFunctionProperties
    val importanceArray = Array.fill[CpuImportance](code.length)(new CpuImportance())
    val codeArray = code.toArray

    var changed = true
    changed = true
    val actualFinalImportance = f.returnType match {
      case FlagBooleanType(_, _, _) => finalImportance.copy(cf = Important, zf = Important, nf = Important, vf = Important)
      case t if t.size == 1 => finalImportance.copy(a = Unimportant)
      case t if t.size == 0 => finalImportance.copy(a = Unimportant, b = Unimportant)
      case _ => finalImportance
    }
    while (changed) {
      changed = false
      var currentImportance = actualFinalImportance
      for (i <- codeArray.indices.reverse) {
        import millfork.assembly.m6809.MOpcode._
        import millfork.node.M6809NiceFunctionProperty._
        if (importanceArray(i) != currentImportance) {
          changed = true
          importanceArray(i) = currentImportance
        }
        val currentLine = codeArray(i)
        currentLine match {
          case MLine0(opcode, _, MemoryAddressConstant(Label(l))) if MOpcode.ConditionalBranching(opcode) =>
            val L = l
            val labelIndex = codeArray.indexWhere {
              case MLine0(LABEL, _, MemoryAddressConstant(Label(L))) => true
              case _ => false
            }
            currentImportance = if (labelIndex < 0) actualFinalImportance else importanceArray(labelIndex) ~ currentImportance
          case MLine0(JMP | BRA, _, MemoryAddressConstant(Label(l))) =>
            val L = l
            val labelIndex = codeArray.indexWhere {
              case MLine0(LABEL, _, MemoryAddressConstant(Label(L))) => true
              case _ => false
            }
            currentImportance = if (labelIndex < 0) actualFinalImportance else importanceArray(labelIndex)
          case _ =>
        }
        currentLine match {

          case MLine0(RTS, _, _) =>
            currentImportance = actualFinalImportance
          case MLine0(LABEL, _, _) =>
            // do nothing
          case MLine0(JSR | JMP, Absolute(false), MemoryAddressConstant(fun: FunctionInMemory)) =>
            // this case has to be handled first, because the generic JSR importance handler is too conservative
            var result = importanceBeforeJsr
            fun.params match {
              case AssemblyOrMacroParamSignature(params) =>
                params.foreach(_.variable match {
                  case M6809RegisterVariable(M6809Register.A, _) =>
                    result = result.copy(a = Important)
                  case M6809RegisterVariable(M6809Register.B, _) =>
                    result = result.copy(b = Important)
                  case M6809RegisterVariable(M6809Register.D, _) =>
                    result = result.copy(a = Important, b = Important)
                  case M6809RegisterVariable(M6809Register.U, _) =>
                    result = result.copy(u = Important)
                  case M6809RegisterVariable(M6809Register.X, _) =>
                    result = result.copy(x = Important)
                  case M6809RegisterVariable(M6809Register.Y, _) =>
                    result = result.copy(y = Important)
                  case _ =>
                })
              case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 1 =>
                result = result.copy(b = Important)
              case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 2 =>
                result = result.copy(a = Important, b = Important)
              case _ =>
            }
            if (readsA(fun.name)) result = result.copy(a = Important)
            if (readsB(fun.name)) result = result.copy(b = Important)
            if (readsX(fun.name)) result = result.copy(x = Important)
            if (readsY(fun.name)) result = result.copy(y = Important)
            currentImportance = result.copy(
              a = if (niceFunctionProperties(DoesntChangeA -> fun.name)) currentImportance.a ~ result.a else result.a,
              b = if (niceFunctionProperties(DoesntChangeB -> fun.name)) currentImportance.b ~ result.b else result.b,
              x = if (niceFunctionProperties(DoesntChangeX -> fun.name)) currentImportance.x ~ result.x else result.x,
              y = if (niceFunctionProperties(DoesntChangeY -> fun.name)) currentImportance.y ~ result.y else result.y,
              u = if (niceFunctionProperties(DoesntChangeU -> fun.name)) currentImportance.u ~ result.u else result.u,
              cf = if (niceFunctionProperties(DoesntChangeCF -> fun.name)) currentImportance.cf ~ result.cf else result.cf,
            )

          case MLine0(opcode, addrMode, _) =>
            if (MOpcode.ChangesC(opcode)) currentImportance = currentImportance.copy(cf = Unimportant)
            if (MOpcode.ChangesN(opcode)) currentImportance = currentImportance.copy(nf = Unimportant)
            if (MOpcode.ChangesH(opcode)) currentImportance = currentImportance.copy(hf = Unimportant)
            if (MOpcode.ChangesZ(opcode)) currentImportance = currentImportance.copy(zf = Unimportant)
            if (MOpcode.ReadsC(opcode)) currentImportance = currentImportance.copy(cf = Important)
            if (MOpcode.ReadsH(opcode)) currentImportance = currentImportance.copy(hf = Important)
            if (MOpcode.ReadsV(opcode)) currentImportance = currentImportance.copy(vf = Important)
            if (MOpcode.ReadsZ(opcode)) currentImportance = currentImportance.copy(zf = Important)
            if (MOpcode.ReadsN(opcode)) currentImportance = currentImportance.copy(nf = Important)
            if  (currentLine.changesRegister(M6809Register.A)) currentImportance = currentImportance.copy(a = Unimportant)
            if  (currentLine.changesRegister(M6809Register.B)) currentImportance = currentImportance.copy(b = Unimportant)
            if  (currentLine.changesRegister(M6809Register.X)) currentImportance = currentImportance.copy(x = Unimportant)
            if  (currentLine.changesRegister(M6809Register.Y)) currentImportance = currentImportance.copy(y = Unimportant)
            if  (currentLine.changesRegister(M6809Register.U)) currentImportance = currentImportance.copy(u = Unimportant)
            if  (currentLine.readsRegister(M6809Register.A)) currentImportance = currentImportance.copy(a = Important)
            if  (currentLine.readsRegister(M6809Register.B)) currentImportance = currentImportance.copy(b = Important)
            if  (currentLine.readsRegister(M6809Register.X)) currentImportance = currentImportance.copy(x = Important)
            if  (currentLine.readsRegister(M6809Register.Y)) currentImportance = currentImportance.copy(y = Important)
            if  (currentLine.readsRegister(M6809Register.U)) currentImportance = currentImportance.copy(u = Important)
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
