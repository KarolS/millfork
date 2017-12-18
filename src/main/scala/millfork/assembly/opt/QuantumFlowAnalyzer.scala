package millfork.assembly.opt

import millfork.assembly.{AssemblyLine, OpcodeClasses}
import millfork.env.{Label, MemoryAddressConstant, NormalFunction, NumericConstant}

import scala.collection.immutable.BitSet

/**
  * @author Karol Stasiak
  */

object QCpuStatus {
  val InitialStatus = QCpuStatus((for {
    c <- Seq(true, false)
    v <- Seq(true, false)
    n <- Seq(true, false)
    z <- Seq(true, false)
  } yield QFlagStatus(c = c, d = false, v = v, n = n, z = z) -> QRegStatus(a = QRegStatus.AllValues, x = QRegStatus.AllValues, y = QRegStatus.AllValues, equal = RegEquality.NoEquality)).toMap)

  val UnknownStatus = QCpuStatus((for {
    c <- Seq(true, false)
    v <- Seq(true, false)
    n <- Seq(true, false)
    z <- Seq(true, false)
  } yield QFlagStatus(c = c, d = false, v = v, n = n, z = z) -> QRegStatus(a = QRegStatus.AllValues, x = QRegStatus.AllValues, y = QRegStatus.AllValues, equal = RegEquality.UnknownEquality)).toMap)

  def gather(l: List[(QFlagStatus, QRegStatus)]) =
    QCpuStatus(l.groupBy(_._1).
      map { case (k, vs) => k -> vs.map(_._2).reduce(_ ++ _) }.
      filterNot(_._2.isEmpty))
}

case class QCpuStatus(data: Map[QFlagStatus, QRegStatus]) {
  def collapse: CpuStatus = {
    val registers = data.values.reduce(_ ++ _)

    def bitset(b: BitSet): Status[Int] = if (b.size == 1) SingleStatus(b.head) else AnyStatus()

    def flag(f: QFlagStatus => Boolean): Status[Boolean] =
      if (data.keys.forall(k => f(k))) SingleStatus(true)
      else if (data.keys.forall(k => !f(k))) SingleStatus(false)
      else AnyStatus()

    CpuStatus(
      a = bitset(registers.a),
      x = bitset(registers.x),
      y = bitset(registers.y),
      c = flag(_.c),
      d = flag(_.d),
      v = flag(_.v),
      z = flag(_.z),
      n = flag(_.n),
    )
  }

  def changeFlagUnconditionally(f: QFlagStatus => QFlagStatus): QCpuStatus = {
    QCpuStatus.gather(data.toList.map { case (k, v) => f(k) -> v })
  }

  def changeFlagsInAnUnknownWay(f: QFlagStatus => QFlagStatus, g: QFlagStatus => QFlagStatus): QCpuStatus = {
    QCpuStatus.gather(data.toList.flatMap { case (k, v) => List(f(k) -> v, g(k) -> v) })
  }

  def changeFlagsInAnUnknownWay(f: QFlagStatus => QFlagStatus, g: QFlagStatus => QFlagStatus, h: QFlagStatus => QFlagStatus): QCpuStatus = {
    QCpuStatus.gather(data.toList.flatMap { case (k, v) => List(f(k) -> v, g(k) -> v, h(k) -> v) })
  }

  def mapRegisters(f: QRegStatus => QRegStatus): QCpuStatus = {
    QCpuStatus(data.map { case (k, v) => k -> f(v) })
  }

  def mapRegisters(f: (QFlagStatus, QRegStatus) => QRegStatus): QCpuStatus = {
    QCpuStatus(data.map { case (k, v) => k -> f(k, v) })
  }

  def flatMap(f: (QFlagStatus, QRegStatus) => List[(QFlagStatus, QRegStatus)]): QCpuStatus = {
    QCpuStatus.gather(data.toList.flatMap { case (k, v) => f(k, v) })
  }

  def changeNZFromA: QCpuStatus = {
    QCpuStatus.gather(data.toList.flatMap { case (k, v) =>
      List(
        k.copy(n = false, z = false) -> v.whereA(i => i.toByte > 0),
        k.copy(n = true, z = false) -> v.whereA(i => i.toByte < 0),
        k.copy(n = false, z = true) -> v.whereA(i => i.toByte == 0))
    })
  }

  def changeNZFromX: QCpuStatus = {
    QCpuStatus.gather(data.toList.flatMap { case (k, v) =>
      List(
        k.copy(n = false, z = false) -> v.whereX(i => i.toByte > 0),
        k.copy(n = true, z = false) -> v.whereX(i => i.toByte < 0),
        k.copy(n = false, z = true) -> v.whereX(i => i.toByte == 0))
    })
  }

  def changeNZFromY: QCpuStatus = {
    QCpuStatus.gather(data.toList.flatMap { case (k, v) =>
      List(
        k.copy(n = false, z = false) -> v.whereY(i => i.toByte > 0),
        k.copy(n = true, z = false) -> v.whereY(i => i.toByte < 0),
        k.copy(n = false, z = true) -> v.whereY(i => i.toByte == 0))
    })
  }

  def ~(that: QCpuStatus): QCpuStatus = QCpuStatus.gather(this.data.toList ++ that.data.toList)
}

object QRegStatus {
  val NoValues: BitSet = BitSet.empty
  val AllValues: BitSet = BitSet.fromBitMask(Array(-1L, -1L, -1L, -1L))

}

object RegEquality extends Enumeration {
  val NoEquality, AX, AY, XY, AXY, UnknownEquality = Value

  def or(a: Value, b: Value) = {
    (a, b) match {
      case (UnknownEquality, _) => b
      case (_, UnknownEquality) => a
      case (NoEquality, _) => NoEquality
      case (_, NoEquality) => NoEquality
      case (_, _) if a == b => a
      case (AXY, _) => b
      case (_, AXY) => a
      case _ => NoEquality
    }
  }

  def afterTransfer(a: Value, b: Value) = {
    (a, b) match {
      case (UnknownEquality, _) => b
      case (_, UnknownEquality) => a
      case (NoEquality, _) => b
      case (_, NoEquality) => a
      case (_, _) if a == b => a
      case _ => AXY
    }
  }
}

case class QRegStatus(a: BitSet, x: BitSet, y: BitSet, equal: RegEquality.Value) {
  def isEmpty: Boolean = a.isEmpty || x.isEmpty || y.isEmpty

  def ++(that: QRegStatus) = QRegStatus(
    a = a ++ that.a,
    x = x ++ that.x,
    y = y ++ that.y,
    equal = RegEquality.or(equal, that.equal))

  def afterTransfer(transfer: RegEquality.Value): QRegStatus =
    copy(equal = RegEquality.afterTransfer(equal, transfer))

  def changeA(f: Int => Long): QRegStatus = {
    val newA = a.map(i => f(i).toInt & 0xff)
    val newEqual = equal match {
      case RegEquality.XY => RegEquality.XY
      case RegEquality.AXY => RegEquality.XY
      case _ => RegEquality.NoEquality
    }
    QRegStatus(newA, x, y, newEqual)
  }

  def changeX(f: Int => Long): QRegStatus = {
    val newA = a.map(i => f(i).toInt & 0xff)
    val newEqual = equal match {
      case RegEquality.XY => RegEquality.XY
      case RegEquality.AXY => RegEquality.XY
      case _ => RegEquality.NoEquality
    }
    QRegStatus(newA, x, y, newEqual)
  }

  def changeY(f: Int => Long): QRegStatus = {
    val newA = a.map(i => f(i).toInt & 0xff)
    val newEqual = equal match {
      case RegEquality.XY => RegEquality.XY
      case RegEquality.AXY => RegEquality.XY
      case _ => RegEquality.NoEquality
    }
    QRegStatus(newA, x, y, newEqual)
  }

  def whereA(f: Int => Boolean): QRegStatus =
    equal match {
      case RegEquality.AXY =>
        copy(a = a.filter(f), x = x.filter(f), y = y.filter(f))
      case RegEquality.AY =>
        copy(a = a.filter(f), y = y.filter(f))
      case RegEquality.AX =>
        copy(a = a.filter(f), x = x.filter(f))
      case _ =>
        copy(a = a.filter(f))
    }

  def whereX(f: Int => Boolean): QRegStatus =
    equal match {
      case RegEquality.AXY =>
        copy(a = a.filter(f), x = x.filter(f), y = y.filter(f))
      case RegEquality.XY =>
        copy(x = x.filter(f), y = y.filter(f))
      case RegEquality.AX =>
        copy(a = a.filter(f), x = x.filter(f))
      case _ =>
        copy(x = x.filter(f))
    }

  def whereY(f: Int => Boolean): QRegStatus =
    equal match {
      case RegEquality.AXY =>
        copy(a = a.filter(f), x = x.filter(f), y = y.filter(f))
      case RegEquality.AY =>
        copy(a = a.filter(f), y = y.filter(f))
      case RegEquality.XY =>
        copy(x = x.filter(f), y = y.filter(f))
      case _ =>
        copy(y = y.filter(f))
    }
}

case class QFlagStatus(c: Boolean, d: Boolean, v: Boolean, z: Boolean, n: Boolean)

object QuantumFlowAnalyzer {
  private def loBit(b: Boolean) = if (b) 1 else 0

  private def hiBit(b: Boolean) = if (b) 0x80 else 0

  //noinspection RedundantNewCaseClass
  def analyze(f: NormalFunction, code: List[AssemblyLine]): List[QCpuStatus] = {
    val flagArray = Array.fill[QCpuStatus](code.length)(QCpuStatus.UnknownStatus)
    val codeArray = code.toArray

    var changed = true
    while (changed) {
      changed = false
      var currentStatus: QCpuStatus = if (f.interrupt) QCpuStatus.UnknownStatus else QCpuStatus.UnknownStatus
      for (i <- codeArray.indices) {
        import millfork.assembly.Opcode._
        import millfork.assembly.AddrMode._
        if (flagArray(i) != currentStatus) {
          changed = true
          flagArray(i) = currentStatus
        }
        codeArray(i) match {
          case AssemblyLine(LABEL, _, MemoryAddressConstant(Label(l)), _) =>
            val L = l
            currentStatus = codeArray.indices.flatMap(j => codeArray(j) match {
              case AssemblyLine(_, _, MemoryAddressConstant(Label(L)), _) => Some(flagArray(j))
              case _ => None
            }).fold(QCpuStatus.UnknownStatus)(_ ~ _)

          case AssemblyLine(BCC, _, _, _) =>
            currentStatus = currentStatus.changeFlagUnconditionally(f => f.copy(c = true))
          case AssemblyLine(BCS, _, _, _) =>
            currentStatus = currentStatus.changeFlagUnconditionally(f => f.copy(c = false))
          case AssemblyLine(BVS, _, _, _) =>
            currentStatus = currentStatus.changeFlagUnconditionally(f => f.copy(v = false))
          case AssemblyLine(BVC, _, _, _) =>
            currentStatus = currentStatus.changeFlagUnconditionally(f => f.copy(v = true))
          case AssemblyLine(BMI, _, _, _) =>
            currentStatus = currentStatus.changeFlagUnconditionally(f => f.copy(n = false))
          case AssemblyLine(BPL, _, _, _) =>
            currentStatus = currentStatus.changeFlagUnconditionally(f => f.copy(n = true))
          case AssemblyLine(BEQ, _, _, _) =>
            currentStatus = currentStatus.changeFlagUnconditionally(f => f.copy(z = false))
          case AssemblyLine(BNE, _, _, _) =>
            currentStatus = currentStatus.changeFlagUnconditionally(f => f.copy(z = true))

          case AssemblyLine(SED, _, _, _) =>
            currentStatus = currentStatus.changeFlagUnconditionally(f => f.copy(d = true))
          case AssemblyLine(SEC, _, _, _) =>
            currentStatus = currentStatus.changeFlagUnconditionally(f => f.copy(c = true))
          case AssemblyLine(CLD, _, _, _) =>
            currentStatus = currentStatus.changeFlagUnconditionally(f => f.copy(d = false))
          case AssemblyLine(CLC, _, _, _) =>
            currentStatus = currentStatus.changeFlagUnconditionally(f => f.copy(c = false))
          case AssemblyLine(CLV, _, _, _) =>
            currentStatus = currentStatus.changeFlagUnconditionally(f => f.copy(v = false))

          case AssemblyLine(JSR, _, _, _) =>
            currentStatus = QCpuStatus.InitialStatus

          case AssemblyLine(LDX, Immediate, NumericConstant(n, _), _) =>
            currentStatus = currentStatus.mapRegisters(r => r.changeX(_ => n)).changeNZFromX
          case AssemblyLine(LDY, Immediate, NumericConstant(n, _), _) =>
            currentStatus = currentStatus.mapRegisters(r => r.changeY(_ => n)).changeNZFromY
          case AssemblyLine(LDA, Immediate, NumericConstant(n, _), _) =>
            currentStatus = currentStatus.mapRegisters(r => r.changeA(_ => n)).changeNZFromA
          case AssemblyLine(LAX, Immediate, NumericConstant(n, _), _) =>
            currentStatus = currentStatus.mapRegisters(r => r.changeA(_ => n).changeX(_ => n).afterTransfer(RegEquality.AX)).changeNZFromA

          case AssemblyLine(EOR, Immediate, NumericConstant(n, _), _) =>
            currentStatus = currentStatus.mapRegisters(r => r.changeA(_ ^ n)).changeNZFromA
          case AssemblyLine(AND, Immediate, NumericConstant(n, _), _) =>
            currentStatus = currentStatus.mapRegisters(r => r.changeA(_ & n)).changeNZFromA
          case AssemblyLine(ANC, Immediate, NumericConstant(n, _), _) =>
            currentStatus = currentStatus.mapRegisters(r => r.changeA(_ & n)).changeNZFromA.changeFlagUnconditionally(f => f.copy(c = f.z))
          case AssemblyLine(ORA, Immediate, NumericConstant(n, _), _) =>
            currentStatus = currentStatus.mapRegisters(r => r.changeA(_ | n)).changeNZFromA

          case AssemblyLine(INX, Implied, _, _) =>
            currentStatus = currentStatus.mapRegisters(r => r.changeX(_ + 1)).changeNZFromX
          case AssemblyLine(DEX, Implied, _, _) =>
            currentStatus = currentStatus.mapRegisters(r => r.changeX(_ - 1)).changeNZFromX
          case AssemblyLine(INY, Implied, _, _) =>
            currentStatus = currentStatus.mapRegisters(r => r.changeY(_ - 1)).changeNZFromY
          case AssemblyLine(DEY, Implied, _, _) =>
            currentStatus = currentStatus.mapRegisters(r => r.changeY(_ - 1)).changeNZFromY
          case AssemblyLine(INC, Implied, _, _) =>
            currentStatus = currentStatus.mapRegisters(r => r.changeA(_ - 1)).changeNZFromA
          case AssemblyLine(DEC, Implied, _, _) =>
            currentStatus = currentStatus.mapRegisters(r => r.changeA(_ - 1)).changeNZFromA
          case AssemblyLine(TAX, _, _, _) =>
            currentStatus = currentStatus.mapRegisters(r => r.copy(x = r.a).afterTransfer(RegEquality.AX)).changeNZFromX
          case AssemblyLine(TXA, _, _, _) =>
            currentStatus = currentStatus.mapRegisters(r => r.copy(a = r.x).afterTransfer(RegEquality.AX)).changeNZFromA
          case AssemblyLine(TAY, _, _, _) =>
            currentStatus = currentStatus.mapRegisters(r => r.copy(y = r.a).afterTransfer(RegEquality.AY)).changeNZFromY
          case AssemblyLine(TYA, _, _, _) =>
            currentStatus = currentStatus.mapRegisters(r => r.copy(a = r.y).afterTransfer(RegEquality.AY)).changeNZFromA

          case AssemblyLine(ROL, Implied, _, _) =>
            currentStatus = currentStatus.flatMap((f, r) => List(
              f.copy(c = true) -> r.whereA(a => (a & 0x80) != 0).changeA(a => a * 2 + loBit(f.c)),
              f.copy(c = false) -> r.whereA(a => (a & 0x80) == 0).changeA(a => a * 2 + loBit(f.c)),
            )).changeNZFromA
          case AssemblyLine(ROR, Implied, _, _) =>
            currentStatus = currentStatus.flatMap((f, r) => List(
              f.copy(c = true) -> r.whereA(a => (a & 1) != 0).changeA(a => (a >>> 2) & 0x7f | hiBit(f.c)),
              f.copy(c = false) -> r.whereA(a => (a & 1) == 0).changeA(a => (a >>> 2) & 0x7f | hiBit(f.c)),
            )).changeNZFromA
          case AssemblyLine(ASL, Implied, _, _) =>
            currentStatus = currentStatus.flatMap((f, r) => List(
              f.copy(c = true) -> r.whereA(a => (a & 0x80) != 0).changeA(a => a * 2),
              f.copy(c = false) -> r.whereA(a => (a & 0x80) == 0).changeA(a => a * 2),
            )).changeNZFromA
          case AssemblyLine(LSR, Implied, _, _) =>
            currentStatus = currentStatus.flatMap((f, r) => List(
              f.copy(c = true) -> r.whereA(a => (a & 1) != 0).changeA(a => (a >>> 2) & 0x7f),
              f.copy(c = false) -> r.whereA(a => (a & 1) == 0).changeA(a => (a >>> 2) & 0x7f),
            )).changeNZFromA
          case AssemblyLine(ALR, Immediate, NumericConstant(n, _), _) =>
            currentStatus = currentStatus.flatMap((f, r) => List(
              f.copy(c = true) -> r.whereA(a => (a & n & 1) != 0).changeA(a => ((a & n) >>> 2) & 0x7f),
              f.copy(c = false) -> r.whereA(a => (a & n & 1) == 0).changeA(a => ((a & n) >>> 2) & 0x7f),
            )).changeNZFromA
          case AssemblyLine(ADC, Immediate, NumericConstant(nn, _), _) =>
            val n = nn & 0xff
            currentStatus = currentStatus.flatMap((f, r) =>
              if (f.d) {
                val regs = r.copy(a = QRegStatus.AllValues).changeA(_.toLong)
                List(
                  f.copy(c = false, v = false) -> regs,
                  f.copy(c = true, v = false) -> regs,
                  f.copy(c = false, v = true) -> regs,
                  f.copy(c = true, v = true) -> regs,
                )
              } else {
                if (f.c) {
                  val regs = r.changeA(_ + n + 1)
                  List(
                    f.copy(c = false, v = false) -> regs.whereA(_ >= n),
                    f.copy(c = true, v = false) -> regs.whereA(_ < n),
                    f.copy(c = false, v = true) -> regs.whereA(_ >= n),
                    f.copy(c = true, v = true) -> regs.whereA(_ < n),
                  )
                } else {
                  val regs = r.changeA(_ + n)
                  List(
                    f.copy(c = false, v = false) -> regs.whereA(_ > n),
                    f.copy(c = true, v = false) -> regs.whereA(_ <= n),
                    f.copy(c = false, v = true) -> regs.whereA(_ > n),
                    f.copy(c = true, v = true) -> regs.whereA(_ <= n),
                  )
                }
              }
            ).changeNZFromA
          case AssemblyLine(SBC, Immediate, NumericConstant(n, _), _) =>
            currentStatus = currentStatus.flatMap((f, r) =>
              if (f.d) {
                val regs = r.copy(a = QRegStatus.AllValues).changeA(_.toLong)
                // TODO: guess the carry flag correctly
                List(
                  f.copy(c = false, v = false) -> regs,
                  f.copy(c = true, v = false) -> regs,
                  f.copy(c = false, v = true) -> regs,
                  f.copy(c = true, v = true) -> regs,
                )
              } else {
                val regs = if (f.c) r.changeA(_ - n) else r.changeA(_ - n - 1)
                List(
                  f.copy(c = false, v = false) -> regs,
                  f.copy(c = true, v = false) -> regs,
                  f.copy(c = false, v = true) -> regs,
                  f.copy(c = true, v = true) -> regs,
                )
              }
            ).changeNZFromA

          case AssemblyLine(opcode, addrMode, parameter, _) =>
            if (OpcodeClasses.ChangesX(opcode)) currentStatus = currentStatus.mapRegisters(r => r.copy(x = QRegStatus.AllValues))
            if (OpcodeClasses.ChangesY(opcode)) currentStatus = currentStatus.mapRegisters(r => r.copy(y = QRegStatus.AllValues))
            if (OpcodeClasses.ChangesAAlways(opcode)) currentStatus = currentStatus.mapRegisters(r => r.copy(a = QRegStatus.AllValues))
            if (addrMode == Implied && OpcodeClasses.ChangesAIfImplied(opcode)) currentStatus = currentStatus.mapRegisters(r => r.copy(a = QRegStatus.AllValues))
            if (OpcodeClasses.ChangesNAndZ(opcode)) currentStatus = currentStatus.changeFlagsInAnUnknownWay(
              _.copy(n = false, z = false),
              _.copy(n = true, z = false),
              _.copy(n = false, z = true))
            if (OpcodeClasses.ChangesC(opcode)) currentStatus = currentStatus.changeFlagsInAnUnknownWay(_.copy(c = false), _.copy(c = true))
            if (OpcodeClasses.ChangesV(opcode)) currentStatus = currentStatus.changeFlagsInAnUnknownWay(_.copy(v = false), _.copy(v = true))
            if (opcode == CMP || opcode == CPX || opcode == CPY) {
              if (addrMode == Immediate) parameter match {
                case NumericConstant(0, _) => currentStatus = currentStatus.changeFlagUnconditionally(_.copy(c = true))
                case _ => ()
              }
            }
        }
      }
      //            flagArray.zip(codeArray).foreach{
      //              case (fl, y) => if (y.isPrintable) println(f"$fl%-32s $y%-32s")
      //            }
      //            println("---------------------")
    }

    flagArray.toList
  }
}
