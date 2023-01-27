package millfork.assembly.z80.opt

import millfork.assembly.{AssemblyOptimization, Elidability, OptimizationContext}
import millfork.assembly.z80._
import millfork.env.{AssemblyOrMacroParam, AssemblyOrMacroParamSignature, FunctionInMemory, MemoryAddressConstant, NormalFunction, NormalParamSignature, NumericConstant, ParamSignature, ZRegisterVariable}
import millfork.error.Logger
import millfork.node.ZRegister

/**
  * @author Karol Stasiak
  */
object ChangeRegisterPairPreferringDE extends ChangeRegisterPair(true)

object ChangeRegisterPairPreferringBC extends ChangeRegisterPair(false)

class ChangeRegisterPair(preferBC2DE: Boolean) extends AssemblyOptimization[ZLine] {

  import millfork.node.ZRegister._

  override def minimumRequiredLines: Int = 3

  case class Loaded(b: Boolean = false, c: Boolean = false, d: Boolean = false, e: Boolean = false) {
    def load(register: ZRegister.Value): Loaded = register match {
      case B => copy(b = true, d = false, e = false)
      case C => copy(c = true, d = false, e = false)
      case BC => Loaded(b = true, c = true)
      case D => copy(d = true, b = false, c = false)
      case E => copy(e = true, b = false, c = false)
      case DE => Loaded(d = true, e = true)
      case _ => this
    }

    def hasDE: Boolean = d && e

    def hasBC: Boolean = b && c
  }

  object PairDirection extends Enumeration {
    val BC2DE, DE2BC = Value
  }

  import PairDirection._
  import millfork.assembly.z80.ZOpcode._

  type PairDirection = PairDirection.Value

  override def name = "Changing registers pairs"

  override def optimize(f: NormalFunction, code: List[ZLine], optimizationContext: OptimizationContext): List[ZLine] = {
    if (f.params.isInstanceOf[AssemblyOrMacroParamSignature]) return code
    val usesBC = code.exists(l => (l.readsRegister(BC) || l.changesRegister(BC)) && l.opcode != CALL)
    val usesDE = code.exists(l => (l.readsRegister(DE) || l.changesRegister(DE)) && l.opcode != CALL)
    val canDE2BC = f.returnType.size < 3 && canOptimize(code, DE2BC, Loaded()) && canOptimize2(code, DE2BC) && (f.params match {
      case NormalParamSignature(List(p)) => p.typ.size < 3
      case _ => true
    })
    val canBC2DE = canOptimize(code, BC2DE, Loaded()) && canOptimize2(code, BC2DE)
    implicit val log: Logger = optimizationContext.log
    (canDE2BC, canBC2DE) match {
      case (false, false) => code
      case (true, false) =>
        if (!usesDE) code else {
          log.debug("Changing register pair from DE to BC")
          val changed = switchDE2BC(code)
          traceDiff(code, changed)
          changed
        }
      case (false, true) =>
        if (!usesBC) code else {
          log.debug("Changing register pair from BC to DE")
          val changed = switchBC2DE(code)
          traceDiff(code, changed)
          changed
        }
      case (true, true) =>
        if (preferBC2DE) {
          if (!usesBC) code else {
            log.debug("Changing register pair from BC to DE (arbitrarily)")
            val changed = switchBC2DE(code)
            traceDiff(code, changed)
            changed
          }
        } else {
          if (!usesDE) code else {
            log.debug("Changing register pair from DE to BC (arbitrarily)")
            val changed = switchDE2BC(code)
            traceDiff(code, changed)
            changed
          }
        }
    }
  }

  private def traceDiff(original: List[ZLine], changed: List[ZLine])(implicit log: Logger): Unit = {
    if (log.traceEnabled) {
      original.zip(changed).foreach {
        case (o, n) =>
          if (o.registers == n.registers && o.opcode == n.opcode) log.trace(n.toString)
          else log.trace(f"$o%-30s →  $n%s")
      }
    }
  }

  private def readsBCorDE(params: ParamSignature): Boolean = {
    import ZRegister._
    params match {
      case AssemblyOrMacroParamSignature(ps) => ps.exists {
        case AssemblyOrMacroParam(_, ZRegisterVariable(B | C | D | E | BC | DE, _), _) => true
        case _ => false
      }
      case NormalParamSignature(List(p)) => p.typ.size == 3 || p.typ.size == 4
      case _ => false
    }
  }

  private def canOptimize(code: List[ZLine], dir: PairDirection, loaded: Loaded): Boolean = code match {
    case ZLine0(CALL, _, _) :: xs => false // TODO
    case ZLine0(JP, _, MemoryAddressConstant(f: FunctionInMemory)) :: xs if readsBCorDE(f.params) => false
    case ZLine0(JP, _, NumericConstant(_, _)) :: xs => false
    case ZLine0(LD_16, TwoRegisters(r, IMM_16), _) :: xs => canOptimize(xs, dir, loaded = loaded.load(r))
    case ZLine0(LD,
    TwoRegisters(B, C) |
    TwoRegisters(B, E) |
    TwoRegisters(C, B) |
    TwoRegisters(C, D) |
    TwoRegisters(D, E) |
    TwoRegisters(D, C) |
    TwoRegisters(E, D) |
    TwoRegisters(E, B), _) :: xs => false
    case ZLine0(DSUB, _, _) :: xs => if (loaded.hasBC && dir == DE2BC) canOptimize(xs, dir, loaded) else false
    case ZLine0(LD, TwoRegisters(r@(B|C|D|E), _), _) :: xs => canOptimize(xs, dir, loaded = loaded.load(r))
    case ZLine0(LD, TwoRegistersOffset(r@(B|C|D|E), _, _), _) :: xs => canOptimize(xs, dir, loaded = loaded.load(r))
    case ZLine0(LABEL | CALL | JR | JP, _, _) :: xs => canOptimize(xs, dir, loaded = Loaded())
    case ZLine0(EX_DE_HL | LHLX | SHLX | LD_DEHL | LD_DESP, _, _) :: xs => if (loaded.hasDE && dir == BC2DE) canOptimize(xs, dir, loaded) else false
    case ZLine0(DJNZ, _, _) :: xs => if (loaded.b && dir == DE2BC) canOptimize(xs, dir, loaded) else false
    case x :: xs if !loaded.b && (x.readsRegister(B) || x.changesRegister(B)) => false
    case x :: xs if !loaded.c && (x.readsRegister(C) || x.changesRegister(C)) => false
    case x :: xs if !loaded.d && (x.readsRegister(D) || x.changesRegister(D)) => false
    case x :: xs if !loaded.e && (x.readsRegister(E) || x.changesRegister(E)) => false
    case x :: xs => canOptimize(xs, dir, loaded)
    case _ => true
  }

  private def canOptimize2(code: List[ZLine], dir: PairDirection): Boolean = code match {
    case ZLine0(CALL, _, _) :: xs => canOptimize2(xs, dir)
    case x :: xs if dir == DE2BC && (x.readsRegister(DE) || x.changesRegister(DE)) =>
      (x.elidability == Elidability.Elidable || x.elidability == Elidability.Volatile) && canOptimize2(xs, dir)
    case x :: xs if dir == BC2DE && (x.readsRegister(BC) || x.changesRegister(BC)) =>
      (x.elidability == Elidability.Elidable || x.elidability == Elidability.Volatile) && canOptimize2(xs, dir)
    case x :: xs => canOptimize2(xs, dir)
    case Nil => true
  }

  private def switchBC2DE(code: List[ZLine]): List[ZLine] = {
    code match {
      case (x@ZLine0(_, OneRegister(B), _)) :: xs =>
        x.copy(registers = OneRegister(D)) :: switchBC2DE(xs)
      case (x@ZLine0(_, OneRegister(C), _)) :: xs =>
        x.copy(registers = OneRegister(E)) :: switchBC2DE(xs)
      case (x@ZLine0(_, OneRegister(BC), _)) :: xs =>
        x.copy(registers = OneRegister(DE)) :: switchBC2DE(xs)

      case (x@ZLine0(LD, TwoRegisters(B, D), _)) :: xs =>
        switchBC2DE(xs)
      case (x@ZLine0(LD, TwoRegisters(C, E), _)) :: xs =>
        switchBC2DE(xs)
      case (x@ZLine0(LD, TwoRegisters(D, B), _)) :: xs =>
        switchBC2DE(xs)
      case (x@ZLine0(LD, TwoRegisters(E, C), _)) :: xs =>
        switchBC2DE(xs)

      case (x@ZLine0(_, TwoRegisters(BC, r), _)) :: xs =>
        x.copy(registers = TwoRegisters(DE, r)) :: switchBC2DE(xs)
      case (x@ZLine0(_, TwoRegisters(r, BC), _)) :: xs =>
        x.copy(registers = TwoRegisters(r, DE)) :: switchBC2DE(xs)

      case (x@ZLine0(_, TwoRegisters(B, r), _)) :: xs =>
        x.copy(registers = TwoRegisters(D, r)) :: switchBC2DE(xs)
      case (x@ZLine0(_, TwoRegisters(r, B), _)) :: xs =>
        x.copy(registers = TwoRegisters(r, D)) :: switchBC2DE(xs)

      case (x@ZLine0(_, TwoRegisters(C, r), _)) :: xs =>
        x.copy(registers = TwoRegisters(E, r)) :: switchBC2DE(xs)
      case (x@ZLine0(_, TwoRegisters(r, C), _)) :: xs =>
        x.copy(registers = TwoRegisters(r, E)) :: switchBC2DE(xs)

      case (x@ZLine0(_, TwoRegistersOffset(B, r, o), _)) :: xs =>
        x.copy(registers = TwoRegistersOffset(D, r, o)) :: switchBC2DE(xs)
      case (x@ZLine0(_, TwoRegistersOffset(r, B, o), _)) :: xs =>
        x.copy(registers = TwoRegistersOffset(r, D, o)) :: switchBC2DE(xs)

      case (x@ZLine0(_, TwoRegistersOffset(C, r, o), _)) :: xs =>
        x.copy(registers = TwoRegistersOffset(E, r, o)) :: switchBC2DE(xs)
      case (x@ZLine0(_, TwoRegistersOffset(r, C, o), _)) :: xs =>
        x.copy(registers = TwoRegistersOffset(r, E, o)) :: switchBC2DE(xs)

      case x :: xs => x :: switchBC2DE(xs)
      case Nil => Nil
    }
  }

  private def switchDE2BC(code: List[ZLine]): List[ZLine] = {
    code match {
      case (x@ZLine0(_, OneRegister(D), _)) :: xs =>
        x.copy(registers = OneRegister(B)) :: switchDE2BC(xs)
      case (x@ZLine0(_, OneRegister(E), _)) :: xs =>
        x.copy(registers = OneRegister(C)) :: switchDE2BC(xs)
      case (x@ZLine0(_, OneRegister(BC), _)) :: xs =>
        x.copy(registers = OneRegister(DE)) :: switchDE2BC(xs)

      case (x@ZLine0(LD, TwoRegisters(B, D), _)) :: xs =>
        switchBC2DE(xs)
      case (x@ZLine0(LD, TwoRegisters(C, E), _)) :: xs =>
        switchBC2DE(xs)
      case (x@ZLine0(LD, TwoRegisters(D, B), _)) :: xs =>
        switchDE2BC(xs)
      case (x@ZLine0(LD, TwoRegisters(E, C), _)) :: xs =>
        switchDE2BC(xs)

      case (x@ZLine0(_, TwoRegisters(DE, r), _)) :: xs =>
        x.copy(registers = TwoRegisters(BC, r)) :: switchDE2BC(xs)
      case (x@ZLine0(_, TwoRegisters(r, DE), _)) :: xs =>
        x.copy(registers = TwoRegisters(r, BC)) :: switchDE2BC(xs)

      case (x@ZLine0(_, TwoRegisters(D, r), _)) :: xs =>
        x.copy(registers = TwoRegisters(B, r)) :: switchDE2BC(xs)
      case (x@ZLine0(_, TwoRegisters(r, D), _)) :: xs =>
        x.copy(registers = TwoRegisters(r, B)) :: switchDE2BC(xs)

      case (x@ZLine0(_, TwoRegisters(E, r), _)) :: xs =>
        x.copy(registers = TwoRegisters(C, r)) :: switchDE2BC(xs)
      case (x@ZLine0(_, TwoRegisters(r, E), _)) :: xs =>
        x.copy(registers = TwoRegisters(r, C)) :: switchDE2BC(xs)

      case (x@ZLine0(_, TwoRegistersOffset(D, r, o), _)) :: xs =>
        x.copy(registers = TwoRegistersOffset(B, r, o)) :: switchDE2BC(xs)
      case (x@ZLine0(_, TwoRegistersOffset(r, D, o), _)) :: xs =>
        x.copy(registers = TwoRegistersOffset(r, B, o)) :: switchDE2BC(xs)

      case (x@ZLine0(_, TwoRegistersOffset(E, r, o), _)) :: xs =>
        x.copy(registers = TwoRegistersOffset(C, r, o)) :: switchDE2BC(xs)
      case (x@ZLine0(_, TwoRegistersOffset(r, E, o), _)) :: xs =>
        x.copy(registers = TwoRegistersOffset(r, C, o)) :: switchDE2BC(xs)

      case x :: xs => x :: switchBC2DE(xs)
      case Nil => Nil
    }
  }
}
