package millfork.assembly.opt

import millfork.assembly.{AssemblyLine, OpcodeClasses, State}
import millfork.env._
import millfork.node.Register

import scala.collection.immutable

/**
  * @author Karol Stasiak
  */

sealed trait Importance {
  def ~(that: Importance) = (this, that) match {
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
                         x: Importance = UnknownImportance,
                         y: Importance = UnknownImportance,
                         n: Importance = UnknownImportance,
                         z: Importance = UnknownImportance,
                         v: Importance = UnknownImportance,
                         c: Importance = UnknownImportance,
                         d: Importance = UnknownImportance,
                        ) {
  override def toString: String = s"A=$a,X=$x,Y=$y,Z=$z,N=$n,C=$c,V=$v,D=$d"

  def ~(that: CpuImportance) = new CpuImportance(
    a = this.a ~ that.a,
    x = this.x ~ that.x,
    y = this.y ~ that.y,
    z = this.z ~ that.z,
    n = this.n ~ that.n,
    c = this.c ~ that.c,
    v = this.v ~ that.v,
    d = this.d ~ that.d,
  )

  def isUnimportant(state: State.Value): Boolean = state match {
    case State.A => a == Unimportant
    case State.X => x == Unimportant
    case State.Y => y == Unimportant
    case State.Z => z == Unimportant
    case State.N => n == Unimportant
    case State.C => c == Unimportant
    case State.V => v == Unimportant
    case State.D => d == Unimportant
  }
}

object ReverseFlowAnalyzer {
  //noinspection RedundantNewCaseClass
  def analyze(f: NormalFunction, code: List[AssemblyLine]): List[CpuImportance] = {
    val importanceArray = Array.fill[CpuImportance](code.length)(new CpuImportance())
    val codeArray = code.toArray
    val initialStatus = new CpuStatus(d = SingleStatus(false))

    var changed = true
    val finalImportance = new CpuImportance(a = Important, x = Important, y = Important, c = Important, v = Important, d = Important, z = Important, n = Important)
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
        codeArray(i) match {
          case AssemblyLine(opcode, Relative, MemoryAddressConstant(Label(l)), _) if OpcodeClasses.ShortBranching(opcode) =>
            val L = l
            val labelIndex = codeArray.indexWhere {
              case AssemblyLine(LABEL, _, MemoryAddressConstant(Label(L)), _) => true
              case _ => false
            }
            currentImportance = if (labelIndex < 0) finalImportance else importanceArray(labelIndex) ~ currentImportance
          case _ =>
        }
        codeArray(i) match {
          case AssemblyLine(JSR, _, MemoryAddressConstant(fun:FunctionInMemory), _) =>
            var result = new CpuImportance().copy(d = Important)
            fun.params match {
              case AssemblyParamSignature(params) =>
                params.foreach(_.variable match {
                  case RegisterVariable(Register.A, _) =>
                    result = result.copy(a = Important)
                  case RegisterVariable(Register.X, _) =>
                    result = result.copy(x = Important)
                  case RegisterVariable(Register.Y, _) =>
                    result = result.copy(y = Important)
                  case RegisterVariable(Register.AX | Register.XA, _) =>
                    result = result.copy(a = Important, x = Important)
                  case RegisterVariable(Register.YA | Register.YA, _) =>
                    result = result.copy(a = Important, y = Important)
                  case RegisterVariable(Register.XY | Register.YX, _) =>
                    result = result.copy(x = Important, y=Important)
                  case _ =>
                })
              case _ =>
            }
            currentImportance = result
          case AssemblyLine(JSR, _, _, _) =>
            currentImportance = finalImportance
          case AssemblyLine(JMP, Absolute, MemoryAddressConstant(Label(l)), _) =>
            val L = l
            val labelIndex = codeArray.indexWhere {
              case AssemblyLine(LABEL, _, MemoryAddressConstant(Label(L)), _) => true
              case _ => false
            }
            currentImportance = if (labelIndex < 0) finalImportance else importanceArray(labelIndex)
          case AssemblyLine(JMP, Indirect, _, _) =>
            currentImportance = finalImportance
          case AssemblyLine(BNE | BEQ, _, _, _) =>
            currentImportance = currentImportance.copy(z = Important)
          case AssemblyLine(BMI | BPL, _, _, _) =>
            currentImportance = currentImportance.copy(n = Important)
          case AssemblyLine(SED | CLD, _, _, _) =>
            currentImportance = currentImportance.copy(d = Unimportant)
          case AssemblyLine(RTS, _, _, _) =>
            currentImportance = finalImportance
          case AssemblyLine(DISCARD_XF, _, _, _) =>
            currentImportance = currentImportance.copy(x = Unimportant, n = Unimportant, z = Unimportant, c = Unimportant, v = Unimportant)
          case AssemblyLine(DISCARD_YF, _, _, _) =>
            currentImportance = currentImportance.copy(y = Unimportant, n = Unimportant, z = Unimportant, c = Unimportant, v = Unimportant)
          case AssemblyLine(DISCARD_AF, _, _, _) =>
            currentImportance = currentImportance.copy(a = Unimportant, n = Unimportant, z = Unimportant, c = Unimportant, v = Unimportant)
          case AssemblyLine(opcode, addrMode, _, _) =>
            if (OpcodeClasses.ChangesC(opcode)) currentImportance = currentImportance.copy(c = Unimportant)
            if (OpcodeClasses.ChangesV(opcode)) currentImportance = currentImportance.copy(v = Unimportant)
            if (OpcodeClasses.ChangesNAndZ(opcode)) currentImportance = currentImportance.copy(n = Unimportant, z = Unimportant)
            if (OpcodeClasses.OverwritesA(opcode)) currentImportance = currentImportance.copy(a = Unimportant)
            if (OpcodeClasses.OverwritesX(opcode)) currentImportance = currentImportance.copy(x = Unimportant)
            if (OpcodeClasses.OverwritesY(opcode)) currentImportance = currentImportance.copy(y = Unimportant)
            if (OpcodeClasses.ReadsC(opcode)) currentImportance = currentImportance.copy(c = Important)
            if (OpcodeClasses.ReadsD(opcode)) currentImportance = currentImportance.copy(d = Important)
            if (OpcodeClasses.ReadsV(opcode)) currentImportance = currentImportance.copy(v = Important)
            if (OpcodeClasses.ReadsXAlways(opcode)) currentImportance = currentImportance.copy(x = Important)
            if (OpcodeClasses.ReadsYAlways(opcode)) currentImportance = currentImportance.copy(y = Important)
            if (OpcodeClasses.ReadsAAlways(opcode)) currentImportance = currentImportance.copy(a = Important)
            if (OpcodeClasses.ReadsAIfImplied(opcode) && addrMode == Implied) currentImportance = currentImportance.copy(a = Important)
            if (addrMode == AbsoluteX || addrMode == IndexedX || addrMode == ZeroPageX) currentImportance = currentImportance.copy(x = Important)
            if (addrMode == AbsoluteY || addrMode == IndexedY || addrMode == ZeroPageY) currentImportance = currentImportance.copy(y = Important)
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
