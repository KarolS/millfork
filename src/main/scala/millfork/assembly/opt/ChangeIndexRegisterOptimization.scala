package millfork.assembly.opt

import millfork.CompilationOptions
import millfork.assembly.{AssemblyLine, OpcodeClasses}
import millfork.env.NormalFunction
import millfork.error.ErrorReporting

/**
  * @author Karol Stasiak
  */

object ChangeIndexRegisterOptimizationPreferringX2Y extends ChangeIndexRegisterOptimization(true)
object ChangeIndexRegisterOptimizationPreferringY2X extends ChangeIndexRegisterOptimization(false)

class ChangeIndexRegisterOptimization(preferX2Y: Boolean) extends AssemblyOptimization {

  object IndexReg extends Enumeration {
    val X, Y = Value
  }

  object IndexDirection extends Enumeration {
    val X2Y, Y2X = Value
  }

  import IndexReg._
  import IndexDirection._
  import millfork.assembly.AddrMode._
  import millfork.assembly.Opcode._

  type IndexReg = IndexReg.Value
  type IndexDirection = IndexDirection.Value

  override def name = "Changing index registers"

  override def optimize(f: NormalFunction, code: List[AssemblyLine], options: CompilationOptions): List[AssemblyLine] = {
    val usesX = code.exists(l =>
      OpcodeClasses.ReadsXAlways(l.opcode) ||
        OpcodeClasses.ReadsYAlways(l.opcode) ||
        OpcodeClasses.ChangesX(l.opcode) ||
        OpcodeClasses.ChangesY(l.opcode) ||
        Set(AbsoluteX, AbsoluteY, ZeroPageY, ZeroPageX, IndexedX, IndexedY)(l.addrMode)
    )
    val usesY = code.exists(l =>
      OpcodeClasses.ReadsXAlways(l.opcode) ||
        OpcodeClasses.ReadsYAlways(l.opcode) ||
        OpcodeClasses.ChangesX(l.opcode) ||
        OpcodeClasses.ChangesY(l.opcode) ||
        Set(AbsoluteX, AbsoluteY, ZeroPageY, ZeroPageX, IndexedX, IndexedY)(l.addrMode)
    )
    if (!usesX && !usesY) {
      return code
    }
    val canX2Y = f.returnType.size <= 1 && canOptimize(code, X2Y, None)
    val canY2X = canOptimize(code, Y2X, None)
    (canX2Y, canY2X) match {
      case (false, false) => code
      case (true, false) =>
        if (!usesX) code else {
          ErrorReporting.debug("Changing index register from X to Y")
          val changed = switchX2Y(code)
          traceDiff(code, changed)
          changed
        }
      case (false, true) =>
        if (!usesY) code else {
          ErrorReporting.debug("Changing index register from Y to X")
          val changed = switchY2X(code)
          traceDiff(code, changed)
          changed
        }
      case (true, true) =>
        if (preferX2Y) {
          if (!usesX) code else {
            ErrorReporting.debug("Changing index register from X to Y (arbitrarily)")
            val changed = switchX2Y(code)
            traceDiff(code, changed)
            changed
          }
        } else {
          if (!usesY) code else {
            ErrorReporting.debug("Changing index register from Y to X (arbitrarily)")
            val changed = switchY2X(code)
            traceDiff(code, changed)
            changed
          }
        }
    }
  }

  private def traceDiff(original: List[AssemblyLine], changed: List[AssemblyLine]): Unit = {
    if (ErrorReporting.traceEnabled) {
      original.zip(changed).foreach{
        case (o, n) =>
          if (o.addrMode == n.addrMode && o.opcode == n.opcode) ErrorReporting.trace(n.toString)
          else ErrorReporting.trace(f"$o%-30s →  $n%s")
      }
    }
  }

  //noinspection OptionEqualsSome
  private def canOptimize(code: List[AssemblyLine], dir: IndexDirection, loaded: Option[IndexReg]): Boolean = code match {
    case AssemblyLine(_, AbsoluteY, _, _) :: xs if loaded != Some(Y) => false
    case AssemblyLine(_, ZeroPageY, _, _) :: xs if loaded != Some(Y) => false
    case AssemblyLine(_, IndexedY, _, _) :: xs if dir == Y2X || loaded != Some(Y) => false
    case AssemblyLine(_, AbsoluteX, _, _) :: xs if loaded != Some(X) => false
    case AssemblyLine(_, ZeroPageX, _, _) :: xs if loaded != Some(X) => false
    case AssemblyLine(_, IndexedX, _, _) :: xs if dir == X2Y || loaded != Some(X) => false
    case AssemblyLine(_, AbsoluteIndexedX, _, _) :: xs if dir == X2Y => false

      // using a wrong index register for one instruction is fine
    case AssemblyLine(LDY | TAY, _, _, _) :: AssemblyLine(_, IndexedY, _, _) :: xs if dir == Y2X =>
      canOptimize(xs, dir, None)
    case AssemblyLine(LDX | TAX, _, _, _) :: AssemblyLine(_, IndexedX, _, _) :: xs if dir == X2Y =>
      canOptimize(xs, dir, None)
    case AssemblyLine(LDX | TAX, _, _, _) :: AssemblyLine(INC | DEC | ASL | ROL | ROR | LSR | STZ, AbsoluteX | ZeroPageX, _, _) :: xs if dir == X2Y =>
      canOptimize(xs, dir, None)
    case AssemblyLine(LDY | TAY, _, _, _) :: AssemblyLine(INY | DEY, _, _, _) :: AssemblyLine(_, IndexedY, _, _) :: xs if dir == Y2X =>
      canOptimize(xs, dir, None)
    case AssemblyLine(LDX | TAX, _, _, _) :: AssemblyLine(INX | DEX, _, _, _) :: AssemblyLine(_, IndexedX, _, _) :: xs if dir == X2Y =>
      canOptimize(xs, dir, None)
    case AssemblyLine(LDX | TAX, _, _, _) :: AssemblyLine(INX | DEX, _, _, _) :: AssemblyLine(INC | DEC | ASL | ROL | ROR | LSR | STZ, AbsoluteX | ZeroPageX, _, _) :: xs if dir == X2Y =>
      canOptimize(xs, dir, None)

    case AssemblyLine(INC | DEC | ASL | ROL | ROR | LSR | STZ, AbsoluteX | ZeroPageX, _, _) :: xs if dir == X2Y => false

    case AssemblyLine(LAX, _, _, _) :: xs => false
    case AssemblyLine(JSR, _, _, _) :: xs => false // TODO
    case AssemblyLine(JMP, _, _, _) :: xs => canOptimize(xs, dir, None)
    case AssemblyLine(op, _, _, _) :: xs if OpcodeClasses.ShortBranching(op) => canOptimize(xs, dir, None)
    case AssemblyLine(RTS, _, _, _) :: xs => canOptimize(xs, dir, None)
    case AssemblyLine(LABEL, _, _, _) :: xs => canOptimize(xs, dir, None)
    case AssemblyLine(DISCARD_XF, _, _, _) :: xs => canOptimize(xs, dir, loaded.filter(_ != X))
    case AssemblyLine(DISCARD_YF, _, _, _) :: xs => canOptimize(xs, dir, loaded.filter(_ != Y))
    case AssemblyLine(_, DoesNotExist, _, _) :: xs => canOptimize(xs, dir, loaded)

    case AssemblyLine(TAX | LDX | PLX, _, _, e) :: xs =>
      (e || dir == Y2X) && canOptimize(xs, dir, Some(X))
    case AssemblyLine(TAY | LDY | PLY, _, _, e) :: xs =>
      (e || dir == X2Y) && canOptimize(xs, dir, Some(Y))
    case AssemblyLine(TXA | STX | PHX | CPX | INX | DEX, _, _, e) :: xs =>
      (e || dir == Y2X) && loaded == Some(X) && canOptimize(xs, dir, Some(X))
    case AssemblyLine(TYA | STY | PHY | CPY | INY | DEY, _, _, e) :: xs =>
      (e || dir == X2Y) && loaded == Some(Y) && canOptimize(xs, dir, Some(Y))

    case AssemblyLine(SAX | TXS | SBX, _, _, _) :: xs => dir == Y2X && loaded == Some(X) && canOptimize(xs, dir, Some(X))
    case AssemblyLine(TSX, _, _, _) :: xs => dir == Y2X && loaded != Some(Y) && canOptimize(xs, dir, Some(X))

    case _ :: xs => canOptimize(xs, dir, loaded)

    case Nil => true
  }

  private def switchX2Y(code: List[AssemblyLine]): List[AssemblyLine] = code match {
    case (a@AssemblyLine(LDX | TAX, _, _, _))
      :: (b@AssemblyLine(INC | DEC | ASL | ROL | ROR | LSR | STZ, AbsoluteX | ZeroPageX, _, _))
      :: xs => a :: b :: switchX2Y(xs)
    case (a@AssemblyLine(LDX | TAX, _, _, _))
      :: (b@AssemblyLine(_, IndexedX, _, _))
      :: xs => a :: b :: switchX2Y(xs)
    case (a@AssemblyLine(LDX | TAX, _, _, _))
      :: (i@AssemblyLine(INX | DEX, _, _, _))
      :: (b@AssemblyLine(INC | DEC | ASL | ROL | ROR | LSR | STZ, AbsoluteX | ZeroPageX, _, _))
      :: xs => a :: i :: b :: switchX2Y(xs)
    case (a@AssemblyLine(LDX | TAX, _, _, _))
      :: (i@AssemblyLine(INX | DEX, _, _, _))
      :: (b@AssemblyLine(_, IndexedX, _, _))
      :: xs => a :: i :: b :: switchX2Y(xs)
    case (x@AssemblyLine(TAX, _, _, _)) :: xs => x.copy(opcode = TAY) :: switchX2Y(xs)
    case (x@AssemblyLine(TXA, _, _, _)) :: xs => x.copy(opcode = TYA) :: switchX2Y(xs)
    case (x@AssemblyLine(STX, _, _, _)) :: xs => x.copy(opcode = STY) :: switchX2Y(xs)
    case (x@AssemblyLine(LDX, _, _, _)) :: xs => x.copy(opcode = LDY) :: switchX2Y(xs)
    case (x@AssemblyLine(INX, _, _, _)) :: xs => x.copy(opcode = INY) :: switchX2Y(xs)
    case (x@AssemblyLine(DEX, _, _, _)) :: xs => x.copy(opcode = DEY) :: switchX2Y(xs)
    case (x@AssemblyLine(CPX, _, _, _)) :: xs => x.copy(opcode = CPY) :: switchX2Y(xs)

    case AssemblyLine(LAX, _, _, _) :: xs => ErrorReporting.fatal("Unexpected LAX")
    case AssemblyLine(TXS, _, _, _) :: xs => ErrorReporting.fatal("Unexpected TXS")
    case AssemblyLine(TSX, _, _, _) :: xs => ErrorReporting.fatal("Unexpected TSX")
    case AssemblyLine(SBX, _, _, _) :: xs => ErrorReporting.fatal("Unexpected SBX")
    case AssemblyLine(SAX, _, _, _) :: xs => ErrorReporting.fatal("Unexpected SAX")

    case (x@AssemblyLine(_, AbsoluteX, _, _)) :: xs => x.copy(addrMode = AbsoluteY) :: switchX2Y(xs)
    case (x@AssemblyLine(_, ZeroPageX, _, _)) :: xs => x.copy(addrMode = ZeroPageY) :: switchX2Y(xs)
    case (x@AssemblyLine(_, IndexedX, _, _)) :: xs => ErrorReporting.fatal("Unexpected IndexedX")

    case x::xs => x :: switchX2Y(xs)
    case Nil => Nil
  }

  private def switchY2X(code: List[AssemblyLine]): List[AssemblyLine] = code match {
    case AssemblyLine(LDY | TAY, _, _, _)
      :: AssemblyLine(_, IndexedY, _, _)
      :: xs => code.take(2) ++ switchY2X(xs)
    case AssemblyLine(LDY | TAY, _, _, _)
      :: (i@AssemblyLine(INY | DEY, _, _, _))
      :: AssemblyLine(_, IndexedY, _, _)
      :: xs => code.take(3) ++ switchY2X(xs)
    case (x@AssemblyLine(TAY, _, _, _)) :: xs => x.copy(opcode = TAX) :: switchY2X(xs)
    case (x@AssemblyLine(TYA, _, _, _)) :: xs => x.copy(opcode = TXA) :: switchY2X(xs)
    case (x@AssemblyLine(STY, _, _, _)) :: xs => x.copy(opcode = STX) :: switchY2X(xs)
    case (x@AssemblyLine(LDY, _, _, _)) :: xs => x.copy(opcode = LDX) :: switchY2X(xs)
    case (x@AssemblyLine(INY, _, _, _)) :: xs => x.copy(opcode = INX) :: switchY2X(xs)
    case (x@AssemblyLine(DEY, _, _, _)) :: xs => x.copy(opcode = DEX) :: switchY2X(xs)
    case (x@AssemblyLine(CPY, _, _, _)) :: xs => x.copy(opcode = CPX) :: switchY2X(xs)

    case (x@AssemblyLine(_, AbsoluteY, _, _)) :: xs => x.copy(addrMode = AbsoluteX) :: switchY2X(xs)
    case (x@AssemblyLine(_, ZeroPageY, _, _)) :: xs => x.copy(addrMode = ZeroPageX) :: switchY2X(xs)
    case AssemblyLine(_, IndexedY, _, _) :: xs => ErrorReporting.fatal("Unexpected IndexedY")

    case x::xs => x :: switchY2X(xs)
    case Nil => Nil
  }
}
