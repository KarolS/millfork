package millfork.compiler.mos

import millfork.CompilationFlag
import millfork.assembly.mos.AddrMode._
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos._
import millfork.assembly._
import millfork.compiler.{BranchSpec, CompilationContext}
import millfork.env.{NumericConstant, RegisterVariable, Type, _}
import millfork.error.ErrorReporting
import millfork.node.{Expression, MosRegister, _}

/**
  * @author Karol Stasiak
  */
object DecimalBuiltIns {
  def compileByteShiftLeft(ctx: CompilationContext, l: Expression, r: Expression, rotate: Boolean): List[AssemblyLine] = {
    ctx.env.eval(r) match {
      case Some(NumericConstant(0, _)) =>
        Nil
      case Some(NumericConstant(v, _)) =>
        val addition = BuiltIns.compileAddition(ctx, List.fill(1 << v)(false -> l), decimal = true)
        if (rotate) addition.filterNot(_.opcode == CLC) else addition
      case _ =>
        ErrorReporting.error("Cannot shift by a non-constant amount", r.position)
        Nil
    }
  }

  def compileByteShiftRight(ctx: CompilationContext, l: Expression, r: Expression, rotate: Boolean): List[AssemblyLine] = {
    val b = ctx.env.get[Type]("byte")
    MosExpressionCompiler.compile(ctx, l, Some((b, RegisterVariable(MosRegister.A, b))), BranchSpec.None) ++ (ctx.env.eval(r) match {
      case Some(NumericConstant(0, _)) =>
        Nil
      case Some(NumericConstant(v, _)) =>
        List.fill(v.toInt) {
          shiftOrRotateAccumulatorRight(ctx, rotate, preserveCarry = false)
        }.flatten
      case _ =>
        ErrorReporting.error("Cannot shift by a non-constant amount", r.position)
        Nil
    })
  }

  private def shiftOrRotateAccumulatorRight(ctx: CompilationContext, rotate: Boolean, preserveCarry: Boolean) = {
    val skipHiDigit = MosCompiler.nextLabel("ds")
    val skipLoDigit = MosCompiler.nextLabel("ds")
    val cmos = ctx.options.flags(CompilationFlag.EmitCmosOpcodes)
    if (preserveCarry) {
      val constantLabel = MosCompiler.nextLabel("c8")
      val bit = if (cmos) {
        AssemblyLine.immediate(BIT, 8)
      } else {
        AssemblyLine.absolute(BIT, Label(constantLabel))
      }
      List(
        if (rotate) AssemblyLine.implied(ROR) else AssemblyLine.implied(LSR),
        AssemblyLine(LABEL, DoesNotExist, Label(constantLabel).toAddress, elidable = cmos),
        AssemblyLine(PHP, Implied, Constant.Zero, elidable = cmos),
        AssemblyLine.relative(BPL, skipHiDigit),
        AssemblyLine.implied(SEC),
        AssemblyLine.immediate(SBC, 0x30),
        AssemblyLine.label(skipHiDigit),
        bit,
        AssemblyLine.relative(BEQ, skipLoDigit),
        AssemblyLine.implied(SEC),
        AssemblyLine.immediate(SBC, 0x3),
        AssemblyLine.label(skipLoDigit),
        AssemblyLine.implied(PLP))
    } else {
      val bit = if (cmos) {
        AssemblyLine.immediate(BIT, 8)
      } else {
        AssemblyLine.absolute(BIT, ctx.env.get[ThingInMemory]("__constant8"))
      }
      List(
        if (rotate) AssemblyLine.implied(ROR) else AssemblyLine.implied(LSR),
        AssemblyLine.relative(BPL, skipHiDigit),
        AssemblyLine.implied(SEC),
        AssemblyLine.immediate(SBC, 0x30),
        AssemblyLine.label(skipHiDigit),
        bit,
        AssemblyLine.relative(BEQ, skipLoDigit),
        AssemblyLine.implied(SEC),
        AssemblyLine.immediate(SBC, 0x3),
        AssemblyLine.label(skipLoDigit))
    }
  }

  def compileInPlaceLongShiftLeft(ctx: CompilationContext, l: LhsExpression, r: Expression): List[AssemblyLine] = {
    ctx.env.eval(r) match {
      case Some(NumericConstant(0, _)) =>
        Nil
      case Some(NumericConstant(v, _)) =>
        List.fill(v.toInt)(BuiltIns.compileInPlaceWordOrLongAddition(ctx, l, l, decimal = true, subtract = false)).flatten
      case _ =>
        ErrorReporting.error("Cannot shift by a non-constant amount", r.position)
        Nil
    }
  }

  def compileInPlaceLongShiftRight(ctx: CompilationContext, l: LhsExpression, r: Expression): List[AssemblyLine] = {
    val targetBytes: List[List[AssemblyLine]] = l match {
      case v: VariableExpression =>
        val variable = ctx.env.get[Variable](v.name)
        List.tabulate(variable.typ.size) { i => AssemblyLine.variable(ctx, STA, variable, i) }
      case SeparateBytesExpression(h: VariableExpression, l: VariableExpression) =>
        val lv = ctx.env.get[Variable](l.name)
        val hv = ctx.env.get[Variable](h.name)
        List(
          AssemblyLine.variable(ctx, STA, lv),
          AssemblyLine.variable(ctx, STA, hv))
    }
    ctx.env.eval(r) match {
      case Some(NumericConstant(v, _)) =>
        val size = targetBytes.length
        List.fill(v.toInt) {
          List.tabulate(size) { i =>
            BuiltIns.staTo(LDA, targetBytes(size - 1 - i)) ++
              shiftOrRotateAccumulatorRight(ctx, rotate = i != 0, preserveCarry = i != size - 1) ++
              targetBytes(size - 1 - i)
          }.flatten
        }.flatten
      case _ =>
        ErrorReporting.error("Cannot shift by a non-constant amount", r.position)
        Nil
    }
  }

  def compileInPlaceByteMultiplication(ctx: CompilationContext, l: LhsExpression, r: Expression): List[AssemblyLine] = {
    val multiplier = ctx.env.eval(r) match {
      case Some(NumericConstant(v, _)) =>
        if (v.&(0xf0) > 0x90 || v.&(0xf) > 9)
          ErrorReporting.error("Invalid decimal constant", r.position)
        (v.&(0xf0).>>(4) * 10 + v.&(0xf)).toInt
      case _ =>
        ErrorReporting.error("Cannot multiply by a non-constant amount", r.position)
        return Nil
    }
    val fullStorage = MosExpressionCompiler.compileByteStorage(ctx, MosRegister.A, l)
    val sta = fullStorage.last
    if (sta.opcode != STA) ???
    val fullLoad = fullStorage.init :+ sta.copy(opcode = LDA)
    val transferToStash = sta.addrMode match {
      case AbsoluteX | AbsoluteIndexedX | ZeroPageX | IndexedX => AssemblyLine.implied(TAY)
      case _ => AssemblyLine.implied(TAX)
    }
    val transferToAccumulator = sta.addrMode match {
      case AbsoluteX | AbsoluteIndexedX | ZeroPageX | IndexedX => AssemblyLine.implied(TYA)
      case _ => AssemblyLine.implied(TXA)
    }

    def add1 = List(transferToAccumulator, AssemblyLine.implied(CLC), sta.copy(opcode = ADC), sta)
    def times7 = List(
      AssemblyLine.implied(ASL), AssemblyLine.implied(ASL),
      AssemblyLine.implied(ASL), AssemblyLine.implied(ASL),
      AssemblyLine.implied(SEC), sta.copy(opcode = SBC),
      AssemblyLine.implied(SEC), sta.copy(opcode = SBC),
      AssemblyLine.implied(SEC), sta.copy(opcode = SBC),
      sta)
    def times8 = List(
      AssemblyLine.implied(ASL), AssemblyLine.implied(ASL),
      AssemblyLine.implied(ASL), AssemblyLine.implied(ASL),
      AssemblyLine.implied(SEC), sta.copy(opcode = SBC),
      AssemblyLine.implied(SEC), sta.copy(opcode = SBC),
      sta)
    def times9 = List(
      AssemblyLine.implied(ASL), AssemblyLine.implied(ASL), AssemblyLine.implied(ASL), AssemblyLine.implied(ASL),
      AssemblyLine.implied(SEC), sta.copy(opcode = SBC),
      sta)

    val execute = multiplier match {
      case 0 => List(AssemblyLine.immediate(LDA, 0), sta)
      case 1 => Nil
      case x =>
        val ways = sta.addrMode match {
          case Absolute | AbsoluteX | AbsoluteY | AbsoluteIndexedX | Indirect =>
            waysForLongAddrModes
          case _ =>
            waysForShortAddrModes
        }
        ways(x).flatMap {
          case 1 => add1
          case -7 => times7
          case q if q < 9 => List.fill(q - 1)(List(AssemblyLine.implied(CLC), sta.copy(opcode = ADC))).flatten :+ sta
          case 8 => times8
          case 9 => times9
          case q => List(AssemblyLine.implied(ASL), AssemblyLine.implied(ASL), AssemblyLine.implied(ASL), AssemblyLine.implied(ASL)) ++
            List.fill(q - 10)(List(AssemblyLine.implied(CLC), sta.copy(opcode = ADC))).flatten :+ sta
        }
    }
    if (execute.contains(transferToAccumulator)) {
      AssemblyLine.implied(SED) :: (fullLoad ++ List(transferToStash) ++ execute :+ AssemblyLine.implied(CLD))
    } else {
      AssemblyLine.implied(SED) :: (fullLoad ++ execute :+ AssemblyLine.implied(CLD))
    }
  }

  private lazy val waysForShortAddrModes: Map[Int, List[Int]] = Map(
    2 -> List(2), 3 -> List(3), 4 -> List(2,2), 5 -> List(5), 6 -> List(3,2), 7 -> List(7), 8 -> List(8), 9 -> List(9), 10 -> List(10),
    11 -> List(11), 12 -> List(12), 13 -> List(13), 14 -> List(14), 15 -> List(3,5), 16 -> List(8,2), 17 -> List(8,2,1), 18 -> List(2,9), 19 -> List(2,9,1), 20 -> List(2,10),
    21 -> List(2,10,1), 22 -> List(11,2), 23 -> List(11,2,1), 24 -> List(12,2), 25 -> List(5,5), 26 -> List(2,13), 27 -> List(3,9), 28 -> List(3,9,1), 29 -> List(3,9,1,1), 30 -> List(3,10),
    31 -> List(3,10,1), 32 -> List(8,2,2), 33 -> List(11,3), 34 -> List(11,3,1), 35 -> List(7,5), 36 -> List(2,2,9), 37 -> List(2,2,9,1), 38 -> List(2,9,1,2), 39 -> List(3,13), 40 -> List(2,2,10),
    41 -> List(2,2,10,1), 42 -> List(2,10,1,2), 43 -> List(2,10,1,2,1), 44 -> List(11,2,2), 45 -> List(9,5), 46 -> List(11,2,1,2), 47 -> List(11,2,1,2,1), 48 -> List(12,2,2), 49 -> List(7,7), 50 -> List(10,5),
    51 -> List(10,5,1), 52 -> List(2,2,13), 53 -> List(2,2,13,1), 54 -> List(3,2,9), 55 -> List(11,5), 56 -> List(8,7), 57 -> List(2,9,1,3), 58 -> List(3,9,1,1,2), 59 -> List(3,9,1,1,2,1), 60 -> List(3,2,10),
    61 -> List(3,2,10,1), 62 -> List(3,10,1,2), 63 -> List(7,9), 64 -> List(8,8), 65 -> List(13,5), 66 -> List(11,3,2), 67 -> List(11,3,2,1), 68 -> List(11,3,1,2), 69 -> List(11,2,1,3), 70 -> List(7,10),
    71 -> List(7,10,1), 72 -> List(8,9), 73 -> List(8,9,1), 74 -> List(2,2,9,1,2), 75 -> List(3,5,5), 76 -> List(2,9,1,2,2), 77 -> List(11,7), 78 -> List(3,2,13), 79 -> List(3,2,13,1), 80 -> List(8,10),
    81 -> List(9,9), 82 -> List(9,9,1), 83 -> List(9,9,1,1), 84 -> List(7,12), 85 -> List(7,12,1), 86 -> List(2,10,1,2,1,2), 87 -> List(3,9,1,1,3), 88 -> List(8,11), 89 -> List(8,11,1), 90 -> List(9,10),
    91 -> List(9,10,1), 92 -> List(9,10,1,1), 93 -> List(3,10,1,3), 94 -> List(3,10,1,3,1), 95 -> List(2,9,1,5), 96 -> List(8,12), 97 -> List(8,12,1), 98 -> List(7,14), 99 -> List(11,9),
  )
  private lazy val waysForLongAddrModes: Map[Int, List[Int]] = Map(
    2 -> List(2), 3 -> List(3), 4 -> List(2,2), 5 -> List(5), 6 -> List(3,2), 7 -> List(-7), 8 -> List(8), 9 -> List(9), 10 -> List(10),
    11 -> List(11), 12 -> List(12), 13 -> List(13), 14 -> List(14), 15 -> List(15), 16 -> List(8,2), 17 -> List(8,2,1), 18 -> List(2,9), 19 -> List(2,9,1), 20 -> List(2,10),
    21 -> List(2,10,1), 22 -> List(11,2), 23 -> List(11,2,1), 24 -> List(12,2), 25 -> List(12,2,1), 26 -> List(2,13), 27 -> List(3,9), 28 -> List(3,9,1), 29 -> List(3,9,1,1), 30 -> List(3,10),
    31 -> List(3,10,1), 32 -> List(8,2,2), 33 -> List(11,3), 34 -> List(11,3,1), 35 -> List(-7,5), 36 -> List(2,2,9), 37 -> List(2,2,9,1), 38 -> List(2,9,1,2), 39 -> List(3,13), 40 -> List(2,2,10),
    41 -> List(2,2,10,1), 42 -> List(2,10,1,2), 43 -> List(2,10,1,2,1), 44 -> List(11,2,2), 45 -> List(9,5), 46 -> List(11,2,1,2), 47 -> List(11,2,1,2,1), 48 -> List(12,2,2), 49 -> List(12,2,2,1), 50 -> List(10,5),
    51 -> List(10,5,1), 52 -> List(2,2,13), 53 -> List(2,2,13,1), 54 -> List(3,2,9), 55 -> List(11,5), 56 -> List(8,-7), 57 -> List(2,9,1,3), 58 -> List(3,9,1,1,2), 59 -> List(3,9,1,1,2,1), 60 -> List(3,2,10),
    61 -> List(3,2,10,1), 62 -> List(3,10,1,2), 63 -> List(9,-7), 64 -> List(8,8), 65 -> List(13,5), 66 -> List(11,3,2), 67 -> List(11,3,2,1), 68 -> List(11,3,1,2), 69 -> List(11,2,1,3), 70 -> List(-7,10),
    71 -> List(-7,10,1), 72 -> List(8,9), 73 -> List(8,9,1), 74 -> List(2,2,9,1,2), 75 -> List(12,2,1,3), 76 -> List(2,9,1,2,2), 77 -> List(11,-7), 78 -> List(3,2,13), 79 -> List(3,2,13,1), 80 -> List(8,10),
    81 -> List(9,9), 82 -> List(9,9,1), 83 -> List(9,9,1,1), 84 -> List(12,-7), 85 -> List(12,-7,1), 86 -> List(2,10,1,2,1,2), 87 -> List(3,9,1,1,3), 88 -> List(8,11), 89 -> List(8,11,1), 90 -> List(9,10),
    91 -> List(9,10,1), 92 -> List(9,10,1,1), 93 -> List(3,10,1,3), 94 -> List(3,10,1,3,1), 95 -> List(2,9,1,5), 96 -> List(8,12), 97 -> List(8,12,1), 98 -> List(14,-7), 99 -> List(11,9),
  )

  // The following functions are used to generate the tables above:

  private def multiplyCosts(addrMode: AddrMode.Value) = {
    val hiBytes = addrMode match {
      case Absolute | AbsoluteX | AbsoluteY | AbsoluteIndexedX | Indirect => 1
      case _ => 0
    }
    // TODO: make those costs smarter.
    // Ideally, the following features should be considered:
    // * NMOS vs CMOS (for timings)
    // * compiling for speed vs compiling for size
    // Currently, only the size is taken account of.
    Map(
      1 -> (6 + 2 * hiBytes),
      2 -> (5 + 2 * hiBytes),
      3 -> (8 + 3 * hiBytes),
      5 -> (11 + 5 * hiBytes),
      7 -> (14 + 7 * hiBytes),
      -7 -> (15 + 4 * hiBytes), // alternative algorithm of multiplying by 7
      8 -> (12 + 3 * hiBytes),
      9 -> (9 + 2 * hiBytes),
      10 -> (6 + 1 * hiBytes),
      11 -> (9 + 2 * hiBytes),
      12 -> (12 + 3 * hiBytes),
      13 -> (15 + 4 * hiBytes),
      14 -> (18 + 5 * hiBytes),
      15 -> (21 + 6 * hiBytes),
      16 -> (24 + 7 * hiBytes),
      17 -> (27 + 8 * hiBytes),
      18 -> (30 + 9 * hiBytes),
      19 -> (33 + 10 * hiBytes)
    )
  }

  private def findWay(target: Int, costs: Map[Int, Int]): List[Int] = {
    def recurse(acc: Int, depthLeft: Int, costAndTrace: (Int, List[Int])): Option[(Int, List[Int])] = {
      if (acc == target) return Some(costAndTrace)
      if (acc > target) return None
      if (depthLeft == 0) return None
      val (costSoFar, trace) = costAndTrace
      val results = costs.flatMap {
        case (key, keyCost) =>
          if (key == 1) {
            recurse(1 + acc, depthLeft - 1, (costSoFar + keyCost, key :: trace))
          } else {
            recurse(key.abs * acc, depthLeft - 1, (costSoFar + keyCost, key :: trace))
          }
      }
      if (results.isEmpty) return None
      Some(results.minBy(_._1))
    }

    recurse(1, 6, 0 -> Nil).get._2.reverse
  }

  def main(args: Array[String]): Unit = {
    val shortCosts = multiplyCosts(ZeroPageY)
    val longCosts = multiplyCosts(AbsoluteX)
    for (i <- 2 to 99) {
      if (waysForLongAddrModes(i) != waysForShortAddrModes(i)) {
        println(i)
        val l = waysForLongAddrModes(i)
        val s = waysForShortAddrModes(i)
        val longCost = l.map(longCosts).sum
        val shortCost = s.map(shortCosts).sum
        val longCostIfUsedShortWay = s.map(longCosts).sum
        val shortCostIfUsedLongWay = l.map(shortCosts).sum
        println(s"For long addr:  $l (size: $longCost); the other would have size $longCostIfUsedShortWay")
        println(s"For short addr: $s (size: $shortCost); the other would have size $shortCostIfUsedLongWay")
      }
    }
    println((2 to 99).map(waysForLongAddrModes).map(_.length).max)
    println((2 to 99).map(waysForShortAddrModes).map(_.length).max)
    for (i <- 2 to 99) {
      print(s"$i -> List(${findWay(i, shortCosts).mkString(",")}), ")
      if (i % 10 == 0) println()
    }
    println()
    println()
    for (i <- 2 to 99) {
      print(s"$i -> List(${findWay(i, longCosts).mkString(",")}), ")
      if (i % 10 == 0) println()
    }
  }
}
