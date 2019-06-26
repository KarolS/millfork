package millfork.assembly.mos.opt

import millfork.assembly.mos.{AssemblyLine, AssemblyLine0, OpcodeClasses}
import millfork.assembly.{AssemblyOptimization, Elidability, OptimizationContext}
import millfork.env.NormalFunction
import millfork.error.Logger

import scala.util.control.TailCalls.{TailRec, done, tailcall}

/**
  * @author Karol Stasiak
  */

object ChangeIndexRegisterOptimizationPreferringX2Y extends ChangeIndexRegisterOptimization(true)
object ChangeIndexRegisterOptimizationPreferringY2X extends ChangeIndexRegisterOptimization(false)

class ChangeIndexRegisterOptimization(preferX2Y: Boolean) extends AssemblyOptimization[AssemblyLine] {

  object IndexReg extends Enumeration {
    val X, Y = Value
  }

  object IndexDirection extends Enumeration {
    val X2Y, Y2X = Value
  }

  import IndexReg._
  import IndexDirection._
  import millfork.assembly.mos.AddrMode._
  import millfork.assembly.mos.Opcode._

  type IndexReg = IndexReg.Value
  type IndexDirection = IndexDirection.Value

  override def name = "Changing index registers"

  override def optimize(f: NormalFunction, code: List[AssemblyLine], optimizationContext: OptimizationContext): List[AssemblyLine] = {
    val usesX = code.exists(l =>
      OpcodeClasses.ReadsXAlways(l.opcode) ||
        OpcodeClasses.ReadsYAlways(l.opcode) ||
        OpcodeClasses.ChangesX(l.opcode) ||
        OpcodeClasses.ChangesY(l.opcode) ||
        Set(AbsoluteX, AbsoluteY, ZeroPageY, ZeroPageX, IndexedX, IndexedY, LongIndexedY, IndexedSY)(l.addrMode)
    )
    val usesY = code.exists(l =>
      OpcodeClasses.ReadsXAlways(l.opcode) ||
        OpcodeClasses.ReadsYAlways(l.opcode) ||
        OpcodeClasses.ChangesX(l.opcode) ||
        OpcodeClasses.ChangesY(l.opcode) ||
        Set(AbsoluteX, AbsoluteY, ZeroPageY, ZeroPageX, IndexedX, IndexedY, LongIndexedY, IndexedSY)(l.addrMode)
    )
    if (!usesX && !usesY) {
      return code
    }
    val canX2Y = f.returnType.size <= 1 && canOptimize(code, X2Y, None)
    val canY2X = canOptimize(code, Y2X, None)
    implicit val log: Logger = optimizationContext.log
    (canX2Y, canY2X) match {
      case (false, false) => code
      case (true, false) =>
        if (!usesX) code else {
          log.debug("Changing index register from X to Y")
          val changed = switchX2Y(code).result
          traceDiff(code, changed)
          changed
        }
      case (false, true) =>
        if (!usesY) code else {
          log.debug("Changing index register from Y to X")
          val changed = switchY2X(code).result
          traceDiff(code, changed)
          changed
        }
      case (true, true) =>
        if (preferX2Y) {
          if (!usesX) code else {
            log.debug("Changing index register from X to Y (arbitrarily)")
            val changed = switchX2Y(code).result
            traceDiff(code, changed)
            changed
          }
        } else {
          if (!usesY) code else {
            log.debug("Changing index register from Y to X (arbitrarily)")
            val changed = switchY2X(code).result
            traceDiff(code, changed)
            changed
          }
        }
    }
  }

  private def traceDiff(original: List[AssemblyLine], changed: List[AssemblyLine])(implicit log: Logger): Unit = {
    if (log.traceEnabled) {
      original.zip(changed).foreach{
        case (o, n) =>
          if (o.addrMode == n.addrMode && o.opcode == n.opcode) log.trace(n.toString)
          else log.trace(f"$o%-30s →  $n%s")
      }
    }
  }

  //noinspection OptionEqualsSome
  private def canOptimize(code: List[AssemblyLine], dir: IndexDirection, loaded: Option[IndexReg]): Boolean = code match {

    case AssemblyLine0(INC | DEC | ASL | ROL | ROR | LSR | STZ | LDZ | BIT, AbsoluteX | ZeroPageX, _) :: xs if dir == X2Y => false
    case AssemblyLine0(LDY | STY, AbsoluteX | ZeroPageX, _) :: xs => false
    case AssemblyLine0(LDX | STX, AbsoluteY | ZeroPageY, _) :: xs => false

    case AssemblyLine0(_, AbsoluteY, _) :: xs if loaded != Some(Y) => false
    case AssemblyLine0(_, ZeroPageY, _) :: xs if loaded != Some(Y) => false
    case AssemblyLine0(_, IndexedY, _) :: xs if dir == Y2X || loaded != Some(Y) => false
    case AssemblyLine0(_, LongIndexedY, _) :: xs if dir == Y2X || loaded != Some(Y) => false
    case AssemblyLine0(_, IndexedSY, _) :: xs if dir == Y2X || loaded != Some(Y) => false
    case AssemblyLine0(_, AbsoluteX, _) :: xs if loaded != Some(X) => false
    case AssemblyLine0(_, LongAbsoluteX, _) :: xs if loaded != Some(X) => false
    case AssemblyLine0(_, ZeroPageX, _) :: xs if loaded != Some(X) => false
    case AssemblyLine0(_, IndexedX, _) :: xs if dir == X2Y || loaded != Some(X) => false
    case AssemblyLine0(_, AbsoluteIndexedX, _) :: xs if dir == X2Y => false
    case AssemblyLine0(SHX | SHY | AHX | TAS | LAS, _, _) :: xs => false
    case AssemblyLine(TXY, _, _, e, _) :: xs => (e == Elidability.Elidable || e == Elidability.Volatile) && loaded == Some(X) && canOptimize(xs, dir, Some(Y))
    case AssemblyLine(TYX, _, _, e, _) :: xs => (e == Elidability.Elidable || e == Elidability.Volatile) && loaded == Some(Y) && canOptimize(xs, dir, Some(X))

      // using a wrong index register for one instruction is fine
    case AssemblyLine0(LDY | TAY, _, _) :: AssemblyLine0(_, IndexedY, _) :: xs if dir == Y2X =>
      canOptimize(xs, dir, None)
    case AssemblyLine0(LDX | TAX, _, _) :: AssemblyLine0(_, IndexedX, _) :: xs if dir == X2Y =>
      canOptimize(xs, dir, None)
    case AssemblyLine0(LDX | TAX, _, _) :: AssemblyLine0(INC | DEC | ASL | ROL | ROR | LSR | STZ, AbsoluteX | ZeroPageX, _) :: xs if dir == X2Y =>
      canOptimize(xs, dir, None)
    case AssemblyLine0(LDY | TAY, _, _) :: AssemblyLine0(INY | DEY, _, _) :: AssemblyLine0(_, IndexedY, _) :: xs if dir == Y2X =>
      canOptimize(xs, dir, None)
    case AssemblyLine0(LDX | TAX, _, _) :: AssemblyLine0(INX | DEX, _, _) :: AssemblyLine0(_, IndexedX, _) :: xs if dir == X2Y =>
      canOptimize(xs, dir, None)
    case AssemblyLine0(LDX | TAX, _, _) :: AssemblyLine0(INX | DEX, _, _) :: AssemblyLine0(INC | DEC | ASL | ROL | ROR | LSR | STZ, AbsoluteX | ZeroPageX, _) :: xs if dir == X2Y =>
      canOptimize(xs, dir, None)

    case AssemblyLine0(LAX, _, _) :: xs => false
    case AssemblyLine0(JSR | BSR, _, _) :: xs => false // TODO
    case AssemblyLine0(JMP, Absolute, _) :: xs => canOptimize(xs, dir, None) // TODO
    case AssemblyLine0(op, _, _) :: xs if OpcodeClasses.ShortBranching(op) => canOptimize(xs, dir, None)
    case AssemblyLine0(RTS | RTL | BRA | BRL, _, _) :: xs => canOptimize(xs, dir, None)
    case AssemblyLine0(LABEL, _, _) :: xs => canOptimize(xs, dir, None)
    case AssemblyLine0(DISCARD_XF, _, _) :: xs => canOptimize(xs, dir, loaded.filter(_ != X))
    case AssemblyLine0(DISCARD_YF, _, _) :: xs => canOptimize(xs, dir, loaded.filter(_ != Y))
    case AssemblyLine0(_, DoesNotExist, _) :: xs => canOptimize(xs, dir, loaded)

    case AssemblyLine(TAX | LDX | PLX, _, _, e, _) :: xs =>
      (e == Elidability.Elidable || e == Elidability.Volatile || dir == Y2X) && canOptimize(xs, dir, Some(X))
    case AssemblyLine(TAY | LDY | PLY, _, _, e, _) :: xs =>
      (e == Elidability.Elidable || e == Elidability.Volatile || dir == X2Y) && canOptimize(xs, dir, Some(Y))
    case AssemblyLine(TXA | STX | PHX | CPX | INX | DEX | HuSAX, _, _, e, _) :: xs =>
      (e == Elidability.Elidable || e == Elidability.Volatile || dir == Y2X) && loaded == Some(X) && canOptimize(xs, dir, Some(X))
    case AssemblyLine(TYA | STY | PHY | CPY | INY | DEY | SAY, _, _, e, _) :: xs =>
      (e == Elidability.Elidable || e == Elidability.Volatile || dir == X2Y) && loaded == Some(Y) && canOptimize(xs, dir, Some(Y))

    case AssemblyLine0(SAX | TXS | SBX, _, _) :: xs => dir == Y2X && loaded == Some(X) && canOptimize(xs, dir, Some(X))
    case AssemblyLine0(TSX, _, _) :: xs => dir == Y2X && loaded != Some(Y) && canOptimize(xs, dir, Some(X))

    case _ :: xs => canOptimize(xs, dir, loaded)

    case Nil => true
  }

  private def switchX2Y(code: List[AssemblyLine])(implicit log: Logger): TailRec[List[AssemblyLine]] = code match {
    case (a@AssemblyLine0(LDX | TAX, _, _))
      :: (b@AssemblyLine0(INC | DEC | ASL | ROL | ROR | LSR | LDY | STY | STZ, AbsoluteX | ZeroPageX, _))
      :: xs => tailcall(switchX2Y(xs)).map(a :: b :: _)
    case (a@AssemblyLine0(LDX | TAX, _, _))
      :: (b@AssemblyLine0(_, IndexedX, _))
      :: xs => tailcall(switchX2Y(xs)).map(a :: b :: _)
    case (a@AssemblyLine0(LDX | TAX, _, _))
      :: (i@AssemblyLine0(INX | DEX, _, _))
      :: (b@AssemblyLine0(INC | DEC | ASL | ROL | ROR | LSR | LDY | STY | STZ, AbsoluteX | ZeroPageX, _))
      :: xs => tailcall(switchX2Y(xs)).map(a :: i :: b :: _)
    case (a@AssemblyLine0(LDX | TAX, _, _))
      :: (i@AssemblyLine0(INX | DEX, _, _))
      :: (b@AssemblyLine0(_, IndexedX, _))
      :: xs => tailcall(switchX2Y(xs)).map(a :: i :: b :: _)
    case (x@AssemblyLine0(TAX, _, _)) :: xs => tailcall(switchX2Y(xs)).map(x.copy(opcode = TAY) :: _)
    case (x@AssemblyLine0(TXA, _, _)) :: xs => tailcall(switchX2Y(xs)).map(x.copy(opcode = TYA) :: _)
    case (x@AssemblyLine0(TXY | TYX, _, _)) :: xs => tailcall(switchX2Y(xs)).map(x.copy(opcode = TYX) :: _) // keep the transfer for the flags
    case (x@AssemblyLine0(STX, _, _)) :: xs => tailcall(switchX2Y(xs)).map(x.copy(opcode = STY) :: _)
    case (x@AssemblyLine0(LDX, _, _)) :: xs => tailcall(switchX2Y(xs)).map(x.copy(opcode = LDY) :: _)
    case (x@AssemblyLine0(INX, _, _)) :: xs => tailcall(switchX2Y(xs)).map(x.copy(opcode = INY) :: _)
    case (x@AssemblyLine0(DEX, _, _)) :: xs => tailcall(switchX2Y(xs)).map(x.copy(opcode = DEY) :: _)
    case (x@AssemblyLine0(CPX, _, _)) :: xs => tailcall(switchX2Y(xs)).map(x.copy(opcode = CPY) :: _)
    case (x@AssemblyLine0(PHX, _, _)) :: xs => tailcall(switchX2Y(xs)).map(x.copy(opcode = PHY) :: _)
    case (x@AssemblyLine0(PLX, _, _)) :: xs => tailcall(switchX2Y(xs)).map(x.copy(opcode = PLY) :: _)
    case (x@AssemblyLine0(HuSAX, _, _)) :: xs => tailcall(switchX2Y(xs)).map(x.copy(opcode = SAY) :: _)

    case AssemblyLine0(LAX, _, _) :: xs => log.fatal("Unexpected LAX")
    case AssemblyLine0(TXS, _, _) :: xs => log.fatal("Unexpected TXS")
    case AssemblyLine0(TSX, _, _) :: xs => log.fatal("Unexpected TSX")
    case AssemblyLine0(SBX, _, _) :: xs => log.fatal("Unexpected SBX")
    case AssemblyLine0(SAX, _, _) :: xs => log.fatal("Unexpected SAX")
    case AssemblyLine0(SXY, _, _) :: xs => log.fatal("Unexpected SXY")

    case (x@AssemblyLine0(_, AbsoluteX, _)) :: xs => tailcall(switchX2Y(xs)).map(x.copy(addrMode = AbsoluteY) :: _)
    case (x@AssemblyLine0(_, ZeroPageX, _)) :: xs => tailcall(switchX2Y(xs)).map(x.copy(addrMode = ZeroPageY) :: _)
    case (x@AssemblyLine0(_, IndexedX, _)) :: xs => log.fatal("Unexpected IndexedX")

    case x::xs => tailcall(switchX2Y(xs)).map(x :: _)
    case Nil => done(Nil)
  }

  private def switchY2X(code: List[AssemblyLine])(implicit log: Logger): TailRec[List[AssemblyLine]] = code match {
    case AssemblyLine0(LDY | TAY, _, _)
      :: AssemblyLine0(_, IndexedY, _)
      :: xs => tailcall(switchY2X(xs)).map(code.take(2) ++ _)
    case AssemblyLine0(LDY | TAY, _, _)
      :: (i@AssemblyLine0(INY | DEY, _, _))
      :: AssemblyLine0(_, IndexedY, _)
      :: xs => tailcall(switchY2X(xs)).map(code.take(3) ++ _)
    case (x@AssemblyLine0(TAY, _, _)) :: xs => tailcall(switchY2X(xs)).map(x.copy(opcode = TAX) :: _)
    case (x@AssemblyLine0(TYA, _, _)) :: xs => tailcall(switchY2X(xs)).map(x.copy(opcode = TXA) :: _)
    case (x@AssemblyLine0(TYX | TXY, _, _)) :: xs => tailcall(switchY2X(xs)).map(x.copy(opcode = TXY) :: _) // keep the transfer for the flags
    case (x@AssemblyLine0(STY, _, _)) :: xs => tailcall(switchY2X(xs)).map(x.copy(opcode = STX) :: _)
    case (x@AssemblyLine0(LDY, _, _)) :: xs => tailcall(switchY2X(xs)).map(x.copy(opcode = LDX) :: _)
    case (x@AssemblyLine0(INY, _, _)) :: xs => tailcall(switchY2X(xs)).map(x.copy(opcode = INX) :: _)
    case (x@AssemblyLine0(DEY, _, _)) :: xs => tailcall(switchY2X(xs)).map(x.copy(opcode = DEX) :: _)
    case (x@AssemblyLine0(CPY, _, _)) :: xs => tailcall(switchY2X(xs)).map(x.copy(opcode = CPX) :: _)
    case (x@AssemblyLine0(PHY, _, _)) :: xs => tailcall(switchY2X(xs)).map(x.copy(opcode = PHX) :: _)
    case (x@AssemblyLine0(PLY, _, _)) :: xs => tailcall(switchY2X(xs)).map(x.copy(opcode = PLX) :: _)
    case (x@AssemblyLine0(SAY, _, _)) :: xs => tailcall(switchY2X(xs)).map(x.copy(opcode = HuSAX) :: _)
    case AssemblyLine0(SXY, _, _) :: xs => log.fatal("Unexpected SXY")

    case (x@AssemblyLine0(_, AbsoluteY, _)) :: xs => tailcall(switchY2X(xs)).map(x.copy(addrMode = AbsoluteX) :: _)
    case (x@AssemblyLine0(_, ZeroPageY, _)) :: xs => tailcall(switchY2X(xs)).map(x.copy(addrMode = ZeroPageX) :: _)
    case AssemblyLine0(_, IndexedY, _) :: xs => log.fatal("Unexpected IndexedY")
    case AssemblyLine0(_, LongIndexedY, _) :: xs => log.fatal("Unexpected LongIndexedY")
    case AssemblyLine0(_, IndexedSY, _) :: xs => log.fatal("Unexpected IndexedSY")

    case x::xs => tailcall(switchY2X(xs)).map(x :: _)
    case Nil => done(Nil)
  }
}
