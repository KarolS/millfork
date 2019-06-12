package millfork.compiler.z80

import millfork.CompilationFlag
import millfork.assembly.z80._
import millfork.compiler.CompilationContext
import millfork.env.NumericConstant
import millfork.error.ConsoleLogger
import millfork.node.{Expression, LhsExpression, ZRegister}

import scala.collection.mutable.ListBuffer

/**
  * @author Karol Stasiak
  */
object Z80DecimalBuiltIns {

  def compileByteShiftLeft(ctx: CompilationContext, r: Expression): List[ZLine] = {
    import millfork.assembly.z80.ZOpcode._
    ctx.env.eval(r) match {
      case Some(NumericConstant(0, _)) =>
        Nil
      case Some(NumericConstant(v, _)) if v < 0 =>
        ctx.log.error("Cannot shift by a negative amount", r.position)
        Nil
      case Some(NumericConstant(v, _)) =>
        List.fill(v.toInt)(List(ZLine.register(ADD, ZRegister.A), ZLine.implied(DAA))).flatten
      case _ =>
        ctx.log.error("Cannot shift by a non-constant amount", r.position)
        Nil
    }
  }

  def compileWordShiftLeft(ctx: CompilationContext, r: Expression): List[ZLine] = {
    import millfork.assembly.z80.ZOpcode._
    import ZRegister._
    ctx.env.eval(r) match {
      case Some(NumericConstant(0, _)) =>
        Nil
      case Some(NumericConstant(v, _)) if v < 0 =>
        ctx.log.error("Cannot shift by a negative amount", r.position)
        Nil
      case Some(NumericConstant(v, _)) =>
        List.fill(v.toInt)(List(
          ZLine.ld8(A, L),
          ZLine.register(ADD, ZRegister.A),
          ZLine.implied(DAA),
          ZLine.ld8(L, A),
          ZLine.ld8(A, H),
          ZLine.register(ADC, ZRegister.A),
          ZLine.implied(DAA),
          ZLine.ld8(H, A)
        )).flatten
      case _ =>
        ctx.log.error("Cannot shift by a non-constant amount", r.position)
        Nil
    }
  }

  def compileInPlaceShiftLeft(ctx: CompilationContext, l: LhsExpression, r: Expression, size: Int): List[ZLine] = {
    import millfork.assembly.z80.ZOpcode._
    ctx.env.eval(r) match {
      case Some(NumericConstant(0, _)) =>
        Nil
      case Some(NumericConstant(v, _)) if v < 0 =>
        ctx.log.error("Cannot shift by a negative amount", r.position)
        Nil
      case Some(NumericConstant(v, _)) =>
        List.fill(v.toInt)(ZBuiltIns.performLongInPlace(ctx, l, l, ADD, ADC, size, decimal = true)).flatten
      case _ =>
        ctx.log.error("Cannot shift by a non-constant amount", r.position)
        Nil
    }
  }

  def compileShiftARight(ctx: CompilationContext, clearCarry: Boolean, preserveCarry: Boolean, output: List[ZLine]): List[ZLine] = {
    import millfork.assembly.z80.ZOpcode._
    import ZRegister._
    val skipHiDigit = ctx.nextLabel("ds")
    val skipLoDigit = ctx.nextLabel("ds")
    val result = ListBuffer[ZLine]()
    if (clearCarry) {
      result += ZLine.register(OR, A)
    }
    if (ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) {
      // Z80 and Sharp
      if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
        result += ZLine.register(RR, A)
      } else {
        result += ZLine.implied(RRA)
      }
      if (preserveCarry) {
        result += ZLine.register(PUSH, AF)
      }
      if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
        result += ZLine.jump(skipHiDigit, IfFlagClear(ZFlag.S))
      } else {
        result += ZLine.register(BIT7, A)
        result += ZLine.jumpR(ctx, skipHiDigit, IfFlagSet(ZFlag.Z))
      }
      result += ZLine.imm8(SUB, 0x30)
      result += ZLine.label(skipHiDigit)
      result += ZLine.register(BIT3, A)
      result += ZLine.jumpR(ctx, skipLoDigit, IfFlagSet(ZFlag.Z))
      result += ZLine.imm8(SUB, 3)
      result += ZLine.label(skipLoDigit)
    } else {
      // Intel 8080
      result += ZLine.implied(RRA)
      if (preserveCarry) {
        result += ZLine.register(PUSH, AF)
      }
      result += ZLine.register(OR, A)
      result += ZLine.jump(skipHiDigit, IfFlagClear(ZFlag.S))
      result += ZLine.imm8(SUB, 0x30)
      result += ZLine.label(skipHiDigit)
      result += ZLine.ld8(E, A)
      result += ZLine.imm8(AND, 8)
      result += ZLine.ld8(A, E)
      result += ZLine.jump(skipLoDigit, IfFlagSet(ZFlag.Z))
      result += ZLine.imm8(SUB, 3)
      result += ZLine.label(skipLoDigit)
    }
    if (output == Nil && preserveCarry) throw new IllegalArgumentException
    result ++= output
    if (preserveCarry) {
      result += ZLine.register(POP, AF)
    }
    result.toList
  }

  def compileByteShiftRight(ctx: CompilationContext, l: Option[Expression], r: Expression): List[ZLine] = {
    val left = l match {
      case Some(e) => Z80ExpressionCompiler.compileToA(ctx, e)
      case _ => Nil
    }
    ctx.env.eval(r) match {
      case Some(NumericConstant(0, _)) => left
      case Some(NumericConstant(n, _)) =>
        left ++ List.fill(n.toInt)(compileShiftARight(ctx, clearCarry = true, preserveCarry = false, Nil)).flatten
      case _ =>
        val right = Z80ExpressionCompiler.compile8BitTo(ctx, r, ZRegister.B)
        val load = if (!left.exists(_.changesRegister(ZRegister.B))) {
          right ++ left
        } else if (!right.exists(_.changesRegister(ZRegister.A))) {
          left ++ right
        } else {
          right ++ Z80ExpressionCompiler.stashBCIfChanged(ctx, left)
        }
        val label = ctx.nextLabel("ds")
        load ++ List(ZLine.label(label)) ++
          compileShiftARight(ctx, clearCarry = true, preserveCarry = false, Nil) ++ ZLine.djnz(ctx, label)
    }
  }

  def compileWordShiftRight(ctx: CompilationContext, l: Expression, r: Expression): List[ZLine] = {
    val left = Z80ExpressionCompiler.compileToHL(ctx, l)
    ctx.env.eval(r) match {
      case Some(NumericConstant(0, _)) => left
      case Some(NumericConstant(n, _)) =>
        left ++ List.fill(n.toInt) {
          List(ZLine.ld8(ZRegister.A, ZRegister.H)) ++
            compileShiftARight(ctx, clearCarry = true, preserveCarry = true, List(ZLine.ld8(ZRegister.H, ZRegister.A))) ++
            List(ZLine.ld8(ZRegister.A, ZRegister.L)) ++
            compileShiftARight(ctx, clearCarry = false, preserveCarry = false, List(ZLine.ld8(ZRegister.L, ZRegister.A)))
        }.flatten
      case _ =>
        val right = Z80ExpressionCompiler.compile8BitTo(ctx, r, ZRegister.B)
        val load = if (!left.exists(_.changesRegister(ZRegister.B))) {
          right ++ left
        } else if (!right.exists(_.changesRegister(ZRegister.HL))) {
          left ++ right
        } else {
          left ++ Z80ExpressionCompiler.stashHLIfChanged(ctx, right)
        }
        val label = ctx.nextLabel("ds")
        load ++
          List(ZLine.label(label), ZLine.ld8(ZRegister.A, ZRegister.H)) ++
          compileShiftARight(ctx, clearCarry = true, preserveCarry = true, Nil) ++
          List(ZLine.ld8(ZRegister.H, ZRegister.A), ZLine.ld8(ZRegister.A, ZRegister.L)) ++
          compileShiftARight(ctx, clearCarry = false, preserveCarry = false, Nil) ++
          List(ZLine.ld8(ZRegister.L, ZRegister.A)) ++ ZLine.djnz(ctx, label)
    }
  }

  def compileInPlaceShiftRight(ctx: CompilationContext, l: LhsExpression, r: Expression, size: Int): List[ZLine] = {
    val loads = Z80ExpressionCompiler.compileByteReads(ctx, l, size, ZExpressionTarget.HL)
    val stores = Z80ExpressionCompiler.compileByteStores(ctx, l, size, includeStep = false)
    ctx.env.eval(r) match {
      case Some(NumericConstant(0, _)) => loads.flatten
      case Some(NumericConstant(n, _)) =>
        List.fill(n.toInt)(loads.zip(stores).zipWithIndex.reverse.flatMap {
          case ((ld, st), i) =>
            ld ++ compileShiftARight(ctx, i == size - 1, i != 0, st)
        }).flatten
      case _ =>
        val right = Z80ExpressionCompiler.compile8BitTo(ctx, r, ZRegister.B)
        val label = ctx.nextLabel("ds")
        right ++ List(ZLine.label(label)) ++
          loads.zip(stores).zipWithIndex.reverse.flatMap {
            case ((ld, st), i) =>
              Z80ExpressionCompiler.stashBCIfChanged(ctx, ld) ++ compileShiftARight(ctx, i == size - 1, i != 0, st)
          } ++ ZLine.djnz(ctx, label)
    }
  }


  def compileInPlaceByteMultiplication(ctx: CompilationContext, r: Expression): List[ZLine] = {
    val multiplier = ctx.env.eval(r) match {
      case Some(NumericConstant(v, _)) =>
        if (v.&(0xf0) > 0x90 || v.&(0xf) > 9)
          ctx.log.error("Invalid decimal constant", r.position)
        (v.&(0xf0).>>(4) * 10 + v.&(0xf)).toInt
      case _ =>
        ctx.log.error("Cannot multiply by a non-constant amount", r.position)
        return Nil
    }
    import millfork.assembly.z80.ZOpcode._
    import ZRegister._

    multiplier match {
      case 0 => List(ZLine.ldImm8(A, 0))
      case 1 => Nil
      case x =>
        val add1 = List(ZLine.register(ADD, D), ZLine.implied(DAA), ZLine.ld8(E, A))
        val times10 = List(ZLine.implied(RLA), ZLine.implied(RLA), ZLine.implied(RLA), ZLine.implied(RLA), ZLine.imm8(AND, 0xf0))
        // TODO: rethink this:
        val ways = if (ctx.options.flag(CompilationFlag.OptimizeForSpeed)) waysOptimizedForCycles else waysOptimizedForBytes
        ZLine.ld8(D, A) :: ZLine.ld8(E, A) :: ways(x).flatMap {
          case 1 => add1
          case q if q < 10 => List.fill(q - 1)(List(ZLine.register(ADD, E), ZLine.implied(DAA))).flatten :+ ZLine.ld8(E, A)
          case q if q >= 10 => times10 ++ List.fill(q - 10)(List(ZLine.register(ADD, E), ZLine.implied(DAA))).flatten :+ ZLine.ld8(E, A)
        }
    }
  }

  private lazy val waysOptimizedForCycles: Map[Int, List[Int]] = Map(
    2 -> List(2), 3 -> List(3), 4 -> List(2,2), 5 -> List(2,2,1), 6 -> List(3,2), 7 -> List(3,2,1), 8 -> List(2,2,2), 9 -> List(3,3), 10 -> List(10),
    11 -> List(11), 12 -> List(12), 13 -> List(13), 14 -> List(14), 15 -> List(3,5), 16 -> List(2,2,2,2), 17 -> List(2,2,2,2,1), 18 -> List(3,3,2), 19 -> List(3,3,2,1), 20 -> List(2,10),
    21 -> List(2,10,1), 22 -> List(11,2), 23 -> List(11,2,1), 24 -> List(12,2), 25 -> List(12,2,1), 26 -> List(2,13), 27 -> List(3,3,3), 28 -> List(2,14), 29 -> List(2,14,1), 30 -> List(3,10),
    31 -> List(3,10,1), 32 -> List(2,2,2,2,2), 33 -> List(11,3), 34 -> List(11,3,1), 35 -> List(11,3,1,1), 36 -> List(3,12), 37 -> List(3,12,1), 38 -> List(3,3,2,1,2), 39 -> List(3,13), 40 -> List(2,2,10),
    41 -> List(2,2,10,1), 42 -> List(2,10,1,2), 43 -> List(2,10,1,2,1), 44 -> List(11,2,2), 45 -> List(11,2,2,1), 46 -> List(11,2,1,2), 47 -> List(11,2,1,2,1), 48 -> List(12,2,2), 49 -> List(12,2,2,1), 50 -> List(2,2,1,10),
    51 -> List(2,2,1,10,1), 52 -> List(2,2,13), 53 -> List(2,2,13,1), 54 -> List(3,3,3,2), 55 -> List(11,5), 56 -> List(11,5,1), 57 -> List(3,3,2,1,3), 58 -> List(2,14,1,2), 59 -> List(2,14,1,2,1), 60 -> List(3,2,10),
    61 -> List(3,2,10,1), 62 -> List(3,10,1,2), 63 -> List(2,10,1,3), 64 -> List(2,2,2,2,2,2), 65 -> List(2,2,1,13), 66 -> List(11,3,2), 67 -> List(11,3,2,1), 68 -> List(11,3,1,2), 69 -> List(11,2,1,3), 70 -> List(3,2,1,10),
    71 -> List(3,2,1,10,1), 72 -> List(3,12,2), 73 -> List(3,12,2,1), 74 -> List(3,12,1,2), 75 -> List(12,2,1,3), 76 -> List(3,3,2,1,2,2), 77 -> List(3,2,1,11), 78 -> List(3,2,13), 79 -> List(3,2,13,1), 80 -> List(2,2,2,10),
    81 -> List(2,2,2,10,1), 82 -> List(2,2,10,1,2), 83 -> List(2,2,10,1,2,1), 84 -> List(2,10,1,2,2), 85 -> List(2,10,1,2,2,1), 86 -> List(2,10,1,2,1,2), 87 -> List(2,10,1,2,1,2,1), 88 -> List(11,2,2,2), 89 -> List(11,2,2,2,1), 90 -> List(3,3,10),
    91 -> List(3,3,10,1), 92 -> List(11,2,1,2,2), 93 -> List(3,10,1,3), 94 -> List(3,10,1,3,1), 95 -> List(3,10,1,3,1,1), 96 -> List(12,2,2,2), 97 -> List(12,2,2,2,1), 98 -> List(12,2,2,1,2), 99 -> List(11,3,3),
  )

  private lazy val waysOptimizedForBytes: Map[Int, List[Int]] = Map(
    2 -> List(2), 3 -> List(3), 4 -> List(2,2), 5 -> List(2,2,1), 6 -> List(3,2), 7 -> List(3,2,1), 8 -> List(2,2,2), 9 -> List(3,3), 10 -> List(10),
    11 -> List(11), 12 -> List(12), 13 -> List(13), 14 -> List(3,2,1,2), 15 -> List(3,5), 16 -> List(2,2,2,2), 17 -> List(2,2,2,2,1), 18 -> List(3,3,2), 19 -> List(3,3,2,1), 20 -> List(2,10),
    21 -> List(2,10,1), 22 -> List(11,2), 23 -> List(11,2,1), 24 -> List(12,2), 25 -> List(12,2,1), 26 -> List(2,13), 27 -> List(3,3,3), 28 -> List(3,2,1,2,2), 29 -> List(3,2,1,2,2,1), 30 -> List(3,10),
    31 -> List(3,10,1), 32 -> List(2,2,2,2,2), 33 -> List(11,3), 34 -> List(11,3,1), 35 -> List(11,3,1,1), 36 -> List(3,12), 37 -> List(3,12,1), 38 -> List(3,3,2,1,2), 39 -> List(3,13), 40 -> List(2,2,10),
    41 -> List(2,2,10,1), 42 -> List(2,10,1,2), 43 -> List(2,10,1,2,1), 44 -> List(11,2,2), 45 -> List(11,2,2,1), 46 -> List(11,2,1,2), 47 -> List(11,2,1,2,1), 48 -> List(12,2,2), 49 -> List(12,2,2,1), 50 -> List(2,2,1,10),
    51 -> List(2,2,1,10,1), 52 -> List(2,2,13), 53 -> List(2,2,13,1), 54 -> List(3,3,3,2), 55 -> List(11,5), 56 -> List(3,2,1,2,2,2), 57 -> List(3,3,2,1,3), 58 -> List(3,2,1,2,2,1,2), 59 -> List(3,2,1,2,2,1,2,1), 60 -> List(3,2,10),
    61 -> List(3,2,10,1), 62 -> List(3,10,1,2), 63 -> List(2,10,1,3), 64 -> List(2,2,2,2,2,2), 65 -> List(2,2,2,2,2,2,1), 66 -> List(11,3,2), 67 -> List(11,3,2,1), 68 -> List(11,3,1,2), 69 -> List(11,2,1,3), 70 -> List(3,2,1,10),
    71 -> List(3,2,1,10,1), 72 -> List(3,12,2), 73 -> List(3,12,2,1), 74 -> List(3,12,1,2), 75 -> List(12,2,1,3), 76 -> List(3,3,2,1,2,2), 77 -> List(3,2,1,11), 78 -> List(3,2,13), 79 -> List(3,2,13,1), 80 -> List(2,2,2,10),
    81 -> List(2,2,2,10,1), 82 -> List(2,2,10,1,2), 83 -> List(2,2,10,1,2,1), 84 -> List(2,10,1,2,2), 85 -> List(2,10,1,2,2,1), 86 -> List(2,10,1,2,1,2), 87 -> List(2,10,1,2,1,2,1), 88 -> List(11,2,2,2), 89 -> List(11,2,2,2,1), 90 -> List(3,3,10),
    91 -> List(3,3,10,1), 92 -> List(11,2,1,2,2), 93 -> List(3,10,1,3), 94 -> List(3,10,1,3,1), 95 -> List(3,3,2,1,5), 96 -> List(12,2,2,2), 97 -> List(12,2,2,2,1), 98 -> List(12,2,2,1,2), 99 -> List(11,3,3),
  )

  private val multiplyCostsCycles = {
    Map(
      1 -> 12,
      2 -> 12,
      3 -> 20,
      5 -> 36,
      10 -> 17,
      11 -> 25,
      12 -> 33,
      13 -> 41,
      14 -> 49,
      15 -> 57,
      16 -> 65,
      17 -> 73,
      18 -> 81,
      19 -> 89
    )
  }

  private val multiplyCostsBytes = {
    Map(
      1 -> 3,
      2 -> 3,
      3 -> 5,
      5 -> 9,
      10 -> 7,
      11 -> 9,
      12 -> 11,
      13 -> 13,
      14 -> 15,
      15 -> 17,
      16 -> 19,
      17 -> 21,
      18 -> 23,
      19 -> 25
    )
  }

  private def findWay(target: Int, costs: Map[Int, Double]): List[Int] = {
    def recurse(acc: Int, depthLeft: Int, costAndTrace: (Double, List[Int])): Option[(Double, List[Int])] = {
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

    recurse(1, 9, 0.0 -> Nil).get._2.reverse
  }

  def main(args: Array[String]): Unit = {
//    println((2 to 99).map(ways).map(_.length).max)
    println()
    println()
    val multiplyCostsCycles2 = multiplyCostsCycles.map { case (k, v) => k -> (v + multiplyCostsBytes(k) / 2048.0) }
    val multiplyCostsBytes2 = multiplyCostsBytes.map { case (k, v) => k -> (v + multiplyCostsCycles(k) / 2048.0) }
    val mc = (2 to 99).map{i => i -> findWay(i, multiplyCostsCycles2)}.toMap
    val mb = (2 to 99).map{i => i -> findWay(i, multiplyCostsBytes2)}.toMap
    for (i <- 2 to 99) {
      print(s"$i -> List(${mc(i).mkString(",")}), ")
      if (i % 10 == 0) println()
    }
    println()
    println()
    for (i <- 2 to 99) {
      print(s"$i -> List(${mb(i).mkString(",")}), ")
      if (i % 10 == 0) println()
    }
    println()
    println()
    for (i <- 2 to 99) {
      if (mc(i) != mb(i)) {
        println(i)
        val c = mc(i)
        val b = mb(i)
        val cycleCostForC = c.map(multiplyCostsCycles).sum
        val cycleCostForB = b.map(multiplyCostsCycles).sum
        val byteCostForC = c.map(multiplyCostsBytes).sum
        val byteCostForB = b.map(multiplyCostsBytes).sum
        println(s"For cycle-optimized addr:  $c (cycles: $cycleCostForC); the other would have cycles $cycleCostForB")
        println(s"For size-optimized addr: $b (size: $byteCostForB); the other would have size $byteCostForC")
      }
    }
    def weight(i: Int): Double = 300.0 / (i * i)
    println("expected byte waste: ", (2 to 99).map(i => (mc(i).map(multiplyCostsBytes).sum - mb(i).map(multiplyCostsBytes).sum) * weight(i)).sum)
    println("expected cycles waste: ", (2 to 99).map(i => (mb(i).map(multiplyCostsCycles).sum - mc(i).map(multiplyCostsCycles).sum) * weight(i)).sum)
    println((2 to 99).map(mc).map(_.length).max)
    println((2 to 99).map(mb).map(_.length).max)
  }

}
