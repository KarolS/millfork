package millfork.compiler.z80

import millfork.CompilationFlag
import millfork.assembly.Elidability
import millfork.assembly.z80._
import millfork.compiler.CompilationContext
import millfork.env._
import millfork.node._
import millfork.assembly.z80.ZOpcode._

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object Z80BulkMemoryOperations {
  import Z80StatementCompiler.compileForStatement

  /**
    * Compiles loops like <code>for i,a,until,b { p[i] = q[i] }</code>
    */
  def compileMemcpy(ctx: CompilationContext, target: IndexedExpression, source: IndexedExpression, f: ForStatement): List[ZLine] = {
    val sourceOffset = removeVariableOnce(f.variable, source.index).getOrElse(return compileForStatement(ctx, f)._1)
    if (!sourceOffset.isPure) return compileForStatement(ctx, f)._1
    val sourceIndexExpression = sourceOffset #+# f.start
    val calculateSource = Z80ExpressionCompiler.calculateAddressToHL(ctx, IndexedExpression(source.name, sourceIndexExpression).pos(source.position), forWriting = false)
    compileMemoryBulk(ctx, target, f,
      useDEForTarget = true,
      preferDecreasing = false,
      _ => calculateSource -> Nil,
      next => List(
        ZLine.ld8(ZRegister.A, ZRegister.MEM_HL),
        ZLine.register(next, ZRegister.HL),
        ZLine.ld8(ZRegister.MEM_DE, ZRegister.A)
      ),
      decreasing => Some(if (decreasing) LDDR else LDIR)
    )
  }


  /**
    * Compiles loops like <code>for i,a,until,b { p[i] = a }</code>,
    * where <code>a</code> is an arbitrary expression independent of <code>i</code>
    */
  def compileMemset(ctx: CompilationContext, target: IndexedExpression, source: Expression, f: ForStatement): List[ZLine] = {
    val loadA = Z80ExpressionCompiler.stashHLIfChanged(ctx, Z80ExpressionCompiler.compileToA(ctx, source)) :+ ZLine.ld8(ZRegister.MEM_HL, ZRegister.A)

    def compileForZ80(targetOffset: Expression): List[ZLine] = {
      val targetIndexExpression = f.direction match {
        case ForDirection.DownTo => targetOffset #+# f.end
        case _ => targetOffset #+# f.start
      }
      val array = if (target.name != f.variable) target.name else "$0000"
      val calculateAddress = Z80ExpressionCompiler.calculateAddressToHL(ctx, IndexedExpression(array, targetIndexExpression).pos(targetIndexExpression.position), forWriting = true)
      val calculateSize = f.direction match {
        case ForDirection.DownTo =>
          Z80ExpressionCompiler.stashHLIfChanged(ctx, Z80ExpressionCompiler.compileToBC(ctx, f.start #-# f.end))
        case ForDirection.To | ForDirection.ParallelTo =>
          Z80ExpressionCompiler.stashHLIfChanged(ctx, Z80ExpressionCompiler.compileToBC(ctx, f.end #-# f.start))
        case ForDirection.Until | ForDirection.ParallelUntil =>
          Z80ExpressionCompiler.stashHLIfChanged(ctx, Z80ExpressionCompiler.compileToBC(ctx, f.end #-# f.start #-# 1))
      }
      val (incOp, ldOp) = f.direction match {
        case ForDirection.DownTo => DEC_16 -> LDDR
        case _ => INC_16 -> LDIR
      }
      val loadFirstValue = ctx.env.eval(source) match {
        case Some(c) => List(ZLine.ldImm8(ZRegister.MEM_HL, c))
        case _ => Z80ExpressionCompiler.stashBCIfChanged(ctx, loadA)
      }
      val loadDE = calculateAddress match {
        case List(ZLine0(ZOpcode.LD_16, TwoRegisters(ZRegister.HL, ZRegister.IMM_16), c)) =>
          if (incOp == DEC_16) List(ZLine.ldImm16(ZRegister.DE, (c - 1).quickSimplify))
          else List(ZLine.ldImm16(ZRegister.DE, (c + 1).quickSimplify))
        case _ => List(
          ZLine.ld8(ZRegister.D, ZRegister.H),
          ZLine.ld8(ZRegister.E, ZRegister.L),
          ZLine.register(incOp, ZRegister.DE))
      }
      calculateAddress ++ calculateSize ++ loadFirstValue ++ loadDE :+ ZLine.implied(ldOp)
    }

    if (ctx.options.flag(CompilationFlag.EmitZ80Opcodes)) {
      removeVariableOnce(f.variable, target.index) match {
        case Some(targetOffset) if targetOffset.isPure =>
          return compileForZ80(targetOffset)
        case _ =>
      }
      if (target.isPure && target.name == f.variable && !target.index.containsVariable(f.variable)) {
        return compileForZ80(target.index)
      }
    }

    compileMemoryBulk(ctx, target, f,
      useDEForTarget = false,
      preferDecreasing = false,
      _ => Nil -> Nil,
      _ => loadA,
      _ => None
    )
  }

  /**
    * Compiles loops like <code>for i,a,until,b { target[i] = z }</code>,
    * where <code>z</code> is an expression depending on <code>source[i]</code>
    */
  def compileMemtransform(ctx: CompilationContext, target: IndexedExpression, operator: String, source: Expression, f: ForStatement): List[ZLine] = {
    val c = determineExtraLoopRegister(ctx, f, source.containsVariable(f.variable))
    val load = buildMemtransformLoader(ctx, ZRegister.MEM_HL, f.variable, operator, source, c.loopRegister).getOrElse(return compileForStatement(ctx, f)._1)
    import scala.util.control.Breaks._
    breakable{
      return compileMemoryBulk(ctx, target, f,
        useDEForTarget = false,
        preferDecreasing = true,
        isSmall => if (isSmall) Nil -> c.initC else break,
        _ => load ++ c.nextC,
        _ => None
      )
    }
    compileForStatement(ctx, f)._1
  }

  /**
    * Compiles loops like <code>for i,a,until,b { target1[i] = x1 ; target2[i] = x2 }</code>,
    * where <code>x1</code> is an expression depending on <code>source1[i]</code>,
    * and <code>x2</code> is an expression depending on <code>source2[i]</code>
    */
  def compileMemtransform2(ctx: CompilationContext,
                           target1: IndexedExpression, operator1: String, source1: Expression,
                           target2: IndexedExpression, operator2: String, source2: Expression,
                           f: ForStatement): List[ZLine] = {
    import scala.util.control.Breaks._
    val c = determineExtraLoopRegister(ctx, f, source1.containsVariable(f.variable) || source2.containsVariable(f.variable))
    val target1Offset = removeVariableOnce(f.variable, target2.index).getOrElse(return compileForStatement(ctx, f)._1)
    val target2Offset = removeVariableOnce(f.variable, target2.index).getOrElse(return compileForStatement(ctx, f)._1)
    val target1IndexExpression = if (c.countDownDespiteSyntax) {
      target1Offset #+# f.end #-# 1
    } else {
      target1Offset #+# f.start
    }
    val target2IndexExpression = if (c.countDownDespiteSyntax) {
      target2Offset #+# f.end #-# 1
    } else {
      target2Offset #+# f.start
    }
    val fused = target1.name == target2.name && ((ctx.env.eval(target1Offset), ctx.env.eval(target2Offset)) match {
      case (Some(a), Some(b)) => a == b
      case _ => false
    })
    if (fused) {
      val load1 = buildMemtransformLoader(ctx, ZRegister.MEM_HL, f.variable, operator1, source1, c.loopRegister).getOrElse(return compileForStatement(ctx, f)._1)
      val load2 = buildMemtransformLoader(ctx, ZRegister.MEM_HL, f.variable, operator2, source2, c.loopRegister).getOrElse(return compileForStatement(ctx, f)._1)
      val loads = load1 ++ load2
      breakable{
        return compileMemoryBulk(ctx, target1, f,
          useDEForTarget = false,
          preferDecreasing = true,
          isSmall => if (isSmall) Nil -> c.initC else break,
          _ => loads ++ c.nextC,
          _ => None
        )
      }
    } else {
      val goodness1 = goodnessForHL(ctx, operator1, source1)
      val goodness2 = goodnessForHL(ctx, operator2, source2)
      val loads = if (goodness1 <= goodness2) {
        val load1 = buildMemtransformLoader(ctx, ZRegister.MEM_DE, f.variable, operator1, source1, c.loopRegister).getOrElse(return compileForStatement(ctx, f)._1)
        val load2 = buildMemtransformLoader(ctx, ZRegister.MEM_HL, f.variable, operator2, source2, c.loopRegister).getOrElse(return compileForStatement(ctx, f)._1)
        load1 ++ load2
      } else {
        val load1 = buildMemtransformLoader(ctx, ZRegister.MEM_HL, f.variable, operator1, source1, c.loopRegister).getOrElse(return compileForStatement(ctx, f)._1)
        val load2 = buildMemtransformLoader(ctx, ZRegister.MEM_DE, f.variable, operator2, source2, c.loopRegister).getOrElse(return compileForStatement(ctx, f)._1)
        load1 ++ load2
      }
      val targetForDE = if (goodness1 <= goodness2) target1 else target2
      val targetForHL = if (goodness1 <= goodness2) target2 else target1
      val targetForHLIndexExpression = if (goodness1 <= goodness2) target2IndexExpression else target1IndexExpression
      breakable{
        return compileMemoryBulk(ctx, targetForDE, f,
          useDEForTarget = true,
          preferDecreasing = true,
          isSmall => if (isSmall) {
            Z80ExpressionCompiler.calculateAddressToHL(ctx, IndexedExpression(targetForHL.name, targetForHLIndexExpression).pos(targetForHLIndexExpression.position), forWriting = false) -> c.initC
          } else break,
          next => loads ++ (c.nextC :+ ZLine.register(next, ZRegister.HL)),
          _ => None
        )
      }
    }
    compileForStatement(ctx, f)._1
  }

  private case class ExtraLoopRegister(loopRegister: ZRegister.Value, initC: List[ZLine], nextC: List[ZLine], countDownDespiteSyntax: Boolean)

  private def determineExtraLoopRegister(ctx: CompilationContext, f: ForStatement, readsLoopVariable: Boolean): ExtraLoopRegister = {
    val useC = readsLoopVariable && (f.direction match {
      case ForDirection.Until | ForDirection.To => true
      case ForDirection.DownTo => ctx.env.eval(f.end) match {
        case Some(NumericConstant(1, _)) => false
        case _ => true
      }
      case ForDirection.ParallelUntil | ForDirection.ParallelTo => ctx.env.eval(f.start) match {
        case Some(NumericConstant(1, _)) => false
        case _ => true
      }
    })
    val loopRegister = if (useC) ZRegister.C else ZRegister.B
    val countDown = f.direction == ForDirection.ParallelTo || f.direction == ForDirection.ParallelUntil || f.direction == ForDirection.DownTo
    val countDownDespiteSyntax = f.direction == ForDirection.ParallelTo || f.direction == ForDirection.ParallelUntil
    val initC = if (useC) Z80ExpressionCompiler.compile8BitTo(ctx, f.direction match {
      case ForDirection.ParallelTo => f.end
      case ForDirection.ParallelUntil => f.end #-# 1
      case _ => f.start
    }, ZRegister.C) else Nil
    val nextC = if (useC) List(ZLine.register(if (countDown) DEC else INC, ZRegister.C)) else Nil
    ExtraLoopRegister(loopRegister, initC, nextC, countDownDespiteSyntax)
  }

  private def goodnessForHL(ctx: CompilationContext, operator: String, source: Expression): Int = operator match {
    case "<<=" | ">>=" =>
      if (ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) ctx.env.eval(source) match {
        case Some(NumericConstant(n, _)) =>
          if (ctx.options.flag(CompilationFlag.OptimizeForSize)) {
            2
          } else {
            // SLA (HL) = 15 cycles
            // SLA A = 8 cycles
            // LD A,(HL) + LD (HL),A = 14 cycles
            (14 - 7 * (n max 0).toInt).max(0)
          }
        case _ => 0
      } else 0
    case "+=" | "-=" => ctx.env.eval(source) match {
      case Some(NumericConstant(1, _)) =>
        if (ctx.options.flag(CompilationFlag.OptimizeForSize)) {
          2
        } else {
          // INC (HL) = 11 cycles
          // INC A = 4 cycles
          // LD A,(HL) + LD (HL),A = 14 cycles
          7
        }
      case Some(NumericConstant(2, _)) =>
        if (ctx.options.flag(CompilationFlag.OptimizeForSize)) {
          2
        } else {
          // INC (HL) = 11 cycles
          // ADD # = 7 cycles
          // LD A,(HL) + LD (HL),A = 14 cycles
          0
        }
      case _ => 0
    }
    case "=" => ctx.env.eval(source) match {
      case Some(_) =>
        if (ctx.options.flag(CompilationFlag.OptimizeForSize)) {
          1
        } else {
          // LD (HL),# = 11 cycles
          // LD A,# = 4 cycles
          // LD (HL),A = 7 cycles
          // so we don't save cycles, but we do save one byte
          1
        }
      case _ => 0
    }
    case _ => 0
  }

  private def buildMemtransformLoader(ctx: CompilationContext, element: ZRegister.Value, loopVariable: String, operator: String, source: Expression, loopRegister: ZRegister.Value): Option[List[ZLine]] = {
    val env = ctx.env
    if (operator == "=") {
      source match {
        case VariableExpression(n) if n == loopVariable =>
          if (element == ZRegister.MEM_HL) Some(List(ZLine.ld8(ZRegister.MEM_HL, loopRegister)))
          else Some(List(ZLine.ld8(ZRegister.A, loopRegister), ZLine.ld8(element, ZRegister.A)))
        case _ => env.eval(source) map { c =>
          if (element == ZRegister.MEM_HL) List(ZLine.ldImm8(ZRegister.MEM_HL, c))
          else List(ZLine.ldImm8(ZRegister.A, c), ZLine.ld8(element, ZRegister.A))
        }
      }
    } else {
      val (operation, daa, shift) = operator match {
        case "+=" => (ZOpcode.ADD, false, false)
        case "+'=" => (ZOpcode.ADD, true, false)
        case "-=" => (ZOpcode.SUB, false, false)
        case "-'=" => (ZOpcode.SUB, true, false)
        case "|=" => (ZOpcode.OR, false, false)
        case "&=" => (ZOpcode.AND, false, false)
        case "^=" => (ZOpcode.XOR, false, false)
        case ">>=" => (ZOpcode.SRL, false, true)
        case "<<=" => (ZOpcode.SLA, false, true)
        case "<<'=" => (ZOpcode.SLA, true, true)
        case _ => return None
      }
      if (shift) {
        env.eval(source) match {
          case Some(NumericConstant(n, _)) =>
            if (n <= 0) Some(Nil) else {
              operation match {
                case SLA =>
                  val builder = mutable.ListBuffer[ZLine]()
                  builder += ZLine.ld8(ZRegister.A, element)
                  for (_ <- 0 until n.toInt) {
                    builder += ZLine.register(ADD, ZRegister.A)
                    if (daa) builder += ZLine.implied(DAA)
                  }
                  builder += ZLine.ld8(element, ZRegister.A)
                  Some(builder.toList)
                case SRL =>
                  if (ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) {
                    if (element == ZRegister.MEM_HL && n <= 2) {
                      Some(List.fill(n.toInt)(ZLine.register(SRL, ZRegister.MEM_HL)))
                    } else {
                      val builder = mutable.ListBuffer[ZLine]()
                      builder += ZLine.ld8(ZRegister.A, element)
                      for (_ <- 0 until n.toInt) {
                        builder += ZLine.register(SRL, ZRegister.A)
                      }
                      builder += ZLine.ld8(element, ZRegister.A)
                      Some(builder.toList)
                    }
                  } else {
                    val builder = mutable.ListBuffer[ZLine]()
                    builder += ZLine.ld8(ZRegister.A, element)
                    // TODO: tricks with AND?
                    for (_ <- 0 until n.toInt) {
                      builder += ZLine.register(OR, ZRegister.A)
                      builder += ZLine.implied(RRA)
                    }
                    builder += ZLine.ld8(element, ZRegister.A)
                    Some(builder.toList)
                  }
                case _ => throw new IllegalStateException()
              }
            }
          case _ => return None
        }
      } else {
        val mod = source match {
          case VariableExpression(n) if n == loopVariable =>
            List(ZLine.register(operation, loopRegister))
          case _ => env.eval(source) match {
            case Some(NumericConstant(1, _)) if operator == "+=" && element == ZRegister.MEM_HL =>
              return Some(List(ZLine.register(INC, ZRegister.MEM_HL)))
            case Some(NumericConstant(1, _)) if operator == "-=" && element == ZRegister.MEM_HL =>
              return Some(List(ZLine.register(DEC, ZRegister.MEM_HL)))
            case Some(NumericConstant(2, _)) if operator == "+=" && element == ZRegister.MEM_HL && ctx.options.flag(CompilationFlag.OptimizeForSize) =>
              return Some(List(ZLine.register(INC, ZRegister.MEM_HL), ZLine.register(INC, ZRegister.MEM_HL)))
            case Some(NumericConstant(2, _)) if operator == "-=" && element == ZRegister.MEM_HL && ctx.options.flag(CompilationFlag.OptimizeForSize) =>
              return Some(List(ZLine.register(DEC, ZRegister.MEM_HL), ZLine.register(DEC, ZRegister.MEM_HL)))
            case Some(c) =>
              if (daa) {
                List(ZLine.imm8(operation, c), ZLine.implied(DAA))
              } else {
                List(ZLine.imm8(operation, c))
              }
            case _ => return None
          }
        }
        Some(
          ZLine.ld8(ZRegister.A, element) :: (mod :+ ZLine.ld8(element, ZRegister.A))
        )
      }
    }
  }

  /**
    * Compiles a tight memcpy-like loop.
    * The result looks like this (assuming Intel 8080 and a small iteration count:
    * <br> calculate byte count to BC
    * <br> calculate target address to HL/DE
    * <br> perform extraAddressCalculations._1 (protected from clobbering BC and DE, not HL)
    * <br> perform extraAddressCalculations._2 (protected from clobbering HL and DE, not BC)
    * <br> (here B or BC contains iteration count)
    * <br> .me_label
    * <br> loadA(INC/DEC) // do stuff, including modifying the target array!
    * <br> INC/DEC HL/DE
    * <br> DJNZ .me_label
    *
    * <p>The entire loop at the end may be replaced with a single instruction on Z80
    *
    * @param ctx                      compilation context
    * @param target                   target indexed expression
    * @param f                        original for statement
    * @param useDEForTarget           use DE instead of HL for target
    * @param extraAddressCalculations extra calculations to perform before the loop, before and after POP BC (parameter: is count small)
    * @param loadA                    byte value calculation (parameter: INC_16 or DEC_16); if you need to store to the target array, do it here
    * @param z80Bulk                  Z80 opcode for faster operation (parameter: is decreasing)
    * @return
    */
  def compileMemoryBulk(ctx: CompilationContext,
                        target: IndexedExpression,
                        f: ForStatement,
                        useDEForTarget: Boolean,
                        preferDecreasing: Boolean,
                        extraAddressCalculations: Boolean => (List[ZLine], List[ZLine]),
                        loadA: ZOpcode.Value => List[ZLine],
                        z80Bulk: Boolean => Option[ZOpcode.Value]): List[ZLine] = {
    val targetOffset = removeVariableOnce(f.variable, target.index).getOrElse(return compileForStatement(ctx, f)._1)
    if (!targetOffset.isPure) return compileForStatement(ctx, f)._1
    val indexVariableSize = ctx.env.get[Variable](f.variable).typ.size
    val wrapper = createForLoopPreconditioningIfStatement(ctx, f)
    val decreasingDespiteSyntax = preferDecreasing && (f.direction == ForDirection.ParallelTo || f.direction == ForDirection.ParallelUntil)
    val decreasing = f.direction == ForDirection.DownTo || decreasingDespiteSyntax
    val plusOne = f.direction == ForDirection.To || f.direction == ForDirection.DownTo || f.direction == ForDirection.ParallelTo
    val byteCountExpression =
      if (f.direction == ForDirection.DownTo) f.start #+# 1 #-# f.end
      else if (plusOne) f.end #+# 1 #-# f.start
      else f.end #-# f.start
    val targetIndexExpression = if (decreasingDespiteSyntax) {
      targetOffset #+# f.end #-# 1
    } else {
      targetOffset #+# f.start
    }
    val ldr = z80Bulk(decreasing)
    val smallCount = indexVariableSize == 1 && (ldr.isEmpty || !ctx.options.flag(CompilationFlag.EmitZ80Opcodes))
    val calculateByteCount = if (smallCount) {
      Z80ExpressionCompiler.compile8BitTo(ctx, byteCountExpression, ZRegister.B)
    } else {
      Z80ExpressionCompiler.compileToHL(ctx, byteCountExpression) ++
        List(ZLine.ld8(ZRegister.B, ZRegister.H), ZLine.ld8(ZRegister.C, ZRegister.L))
    }
    val next = if (decreasing) DEC_16 else INC_16
    val calculateSourceValue = loadA(next)
    val calculateTargetAddress = Z80ExpressionCompiler.calculateAddressToHL(ctx, IndexedExpression(target.name, targetIndexExpression).pos(targetIndexExpression.position), forWriting = true)
    val extraInitializationPair = extraAddressCalculations(smallCount)
    // TODO: figure the optimal compilation order
    val loading = if (useDEForTarget) {
      calculateByteCount ++
        Z80ExpressionCompiler.stashBCIfChanged(ctx, calculateTargetAddress ++ List(ZLine.ld8(ZRegister.D, ZRegister.H), ZLine.ld8(ZRegister.E, ZRegister.L))) ++
        Z80ExpressionCompiler.stashBCIfChanged(ctx, Z80ExpressionCompiler.stashDEIfChanged(ctx, extraInitializationPair._1)) ++
        Z80ExpressionCompiler.stashHLIfChanged(ctx, Z80ExpressionCompiler.stashDEIfChanged(ctx, extraInitializationPair._2))
    } else {
      calculateByteCount ++
        Z80ExpressionCompiler.stashBCIfChanged(ctx, calculateTargetAddress) ++
        Z80ExpressionCompiler.stashBCIfChanged(ctx, Z80ExpressionCompiler.stashHLIfChanged(ctx, extraInitializationPair._1)) ++
        Z80ExpressionCompiler.stashHLIfChanged(ctx, extraInitializationPair._2)
    }

    val label = ctx.nextLabel("me")
    val body = if (ldr.isDefined && ctx.options.flag(CompilationFlag.EmitZ80Opcodes)) {
      List(ZLine.implied(ldr.get))
    } else {
      ZLine.label(label) :: calculateSourceValue ++ (if (smallCount) {
          ZLine.register(next, if (useDEForTarget) ZRegister.DE else ZRegister.HL) :: ZLine.djnz(ctx, label)
      } else {
        List(
          ZLine.register(next, if (useDEForTarget) ZRegister.DE else ZRegister.HL),
          ZLine.register(DEC_16, ZRegister.BC), // <-- Z flag is set here?
          ZLine.ld8(ZRegister.A, ZRegister.C),
          ZLine.register(OR, ZRegister.B),
          ZLine.jump(label, IfFlagSet(ZFlag.Z))
        )
      })
    }
    wrapper.flatMap(l => if (l.opcode == NOP) loading ++ body else List(l))
  }

  private def createForLoopPreconditioningIfStatement(ctx: CompilationContext, f: ForStatement): List[ZLine] = {
    val operator = f.direction match {
      case ForDirection.To | ForDirection.ParallelTo => "<="
      case ForDirection.DownTo => ">="
      case ForDirection.Until | ForDirection.ParallelUntil => "<"
    }
    Z80StatementCompiler.compile(ctx, IfStatement(
      FunctionCallExpression(operator, List(f.start, f.end)),
      List(Z80AssemblyStatement(ZOpcode.NOP, NoRegisters, None, LiteralExpression(0, 1), elidability = Elidability.Fixed)),
      Nil))._1
  }

  private def removeVariableOnce(variable: String, expr: Expression): Option[Expression] = {
    expr match {
      case VariableExpression(i) => if (i == variable) Some(LiteralExpression(0, 1)) else None
      case SumExpression(exprs, false) =>
        if (exprs.count(_._2.containsVariable(variable)) == 1) {
          Some(SumExpression(exprs.map {
            case (false, e) => false -> (if (e.containsVariable(variable)) removeVariableOnce(variable, e).getOrElse(return None) else e)
            case (true, e) => if (e.containsVariable(variable)) return None else true -> e
          }, decimal = false))
        } else None
      case _ => None
    }
  }

}
