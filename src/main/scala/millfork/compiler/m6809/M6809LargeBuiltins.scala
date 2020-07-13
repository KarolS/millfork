package millfork.compiler.m6809

import millfork.assembly.m6809.{Immediate, MLine, MLine0, MOpcode, TwoRegisters}
import millfork.compiler.{AbstractExpressionCompiler, BranchIfFalse, BranchIfTrue, BranchSpec, ComparisonType, CompilationContext, NoBranching}
import millfork.node.{Expression, LhsExpression, M6809Register}
import millfork.assembly.m6809.MOpcode._
import millfork.env.{Constant, NumericConstant}

import scala.collection.GenTraversableOnce
import scala.collection.mutable.ListBuffer

/**
  * @author Karol Stasiak
  */
object M6809LargeBuiltins {

  def compileShiftInPlace(ctx: CompilationContext, size: Int, target: LhsExpression, shiftValue: Expression, left: Boolean): List[MLine] = {
    val (targetAddr, targetInit): (Option[Constant], List[MLine]) = M6809ExpressionCompiler.compileAddressToX(ctx, target) match {
      case List(MLine(LDX, Immediate, addr, _, _)) => Some(addr) -> Nil
      case xs => None -> xs
    }
    val oneShift = if (left) {
      targetAddr match {
        case Some(addr) =>
          List.tabulate(size)(i => MLine.absolute(if (i == 0) ASL else ROL, addr + (size - 1 - i)))
        case None =>
          List.tabulate(size)(i => MLine.indexedX(if (i == 0) ASL else ROL, size - 1 - i))
      }
    } else {
      targetAddr match {
        case Some(addr) =>
          List.tabulate(size)(i => MLine.absolute(if (i == 0) LSR else ROR, addr + i))
        case None =>
          List.tabulate(size)(i => MLine.indexedX(if (i == 0) LSR else ROR, i))
      }
    }
    targetInit ++ (M6809ExpressionCompiler.compileToB(ctx, shiftValue) match {
      case List(MLine0(LDB, Immediate, NumericConstant(n, _))) if n >= 0 && (n - 1) * size < 4 =>
        List.fill(n.toInt)(oneShift).flatten
      case xs@List(MLine0(LDB, Immediate, c)) if c.isProvablyGreaterOrEqualThan(1) =>
        val label = ctx.nextLabel("sr")
        xs ++ List(MLine.label(label)) ++ oneShift ++ List(MLine.inherentB(DEC), MLine.shortBranch(BNE, label))
      case xs =>
        val loop = ctx.nextLabel("sr")
        val skip = ctx.nextLabel("ss")
        M6809ExpressionCompiler.stashXIfNeeded(ctx, xs) ++ List(
          MLine.label(loop),
          MLine.inherentB(DEC),
          MLine.immediate(CMPB, -1),
          MLine.shortBranch(BEQ, skip)) ++ oneShift ++ List(
          MLine.shortBranch(BRA, loop),
          MLine.label(skip)
        )
    })
  }

  def storeLarge(ctx: CompilationContext, target: LhsExpression, source: Expression): List[MLine] = {
    val byteCount = AbstractExpressionCompiler.getExpressionType(ctx, target).size
    val byteLoads = M6809ExpressionCompiler.compileToByteReads(ctx, source, byteCount)
    val (targetAddr, targetInit): (Option[Constant], List[MLine]) = M6809ExpressionCompiler.compileAddressToX(ctx, target) match {
      case List(MLine(LDX, Immediate, addr, _, _)) => Some(addr) -> Nil
      case xs => None -> xs
    }
    // TODO: use loop for very large structures
    // TODO: use D to speed up certain loads
    targetInit ++ (targetAddr match {
      case Some(addr) =>
        byteLoads.zipWithIndex.flatMap { case (loadByte, ix) =>
          loadByte :+ MLine.absolute(STB, addr + (byteCount - 1 - ix))
        }
      case None =>
        byteLoads.zipWithIndex.flatMap { case (loadByte, ix) =>
          M6809ExpressionCompiler.stashXIfNeeded(ctx, loadByte) :+ MLine.indexedX(STB, byteCount - 1 - ix)
        }
    })
  }

  def convertToLda(ldb: List[MLine]): List[MLine] = {
    ldb match {
      case List(l@MLine0(LDB, _, _)) => List(l.copy(opcode = LDA))
      case List(l@MLine0(TFR, TwoRegisters(M6809Register.A, M6809Register.B), _)) => List(l.copy(addrMode = TwoRegisters(M6809Register.B, M6809Register.A)))
      case _ => MLine.exg(M6809Register.A, M6809Register.B) :: (ldb :+ MLine.exg(M6809Register.A, M6809Register.B))
    }
  }

  def isSingleLdb(code: List[MLine]): Boolean = code.length == 1 && code.head.opcode == LDB

  def replaceLdb(code: List[MLine], op: MOpcode.Value): List[MLine] = {
    if (code.last.opcode != LDB) ???
    code.init :+ code.last.copy(opcode = op)
  }

  def compileInc(ctx: CompilationContext, target: LhsExpression): List[MLine] = {
    val sizeInBytes = AbstractExpressionCompiler.getExpressionType(ctx, target).size
    val result = new ListBuffer[MLine]()
    val targetAddr: Option[Constant] = M6809ExpressionCompiler.compileAddressToX(ctx, target) match {
      case List(MLine(LDX, Immediate, addr, _, _)) => Some(addr)
      case xs =>
        result ++= xs
        None
    }
    val skipLabel = ctx.nextLabel("in")
    for (i <- 0 until sizeInBytes) {
      targetAddr match {
        case Some(addr) =>
          result += MLine.absolute(INC, addr + (sizeInBytes - 1 - i))
        case None =>
          result += MLine.indexedX(INC, sizeInBytes - 1 - i)
      }
      if (i != sizeInBytes - 1) {
        result += MLine.shortBranch(BNE, skipLabel)
      }
    }
    result += MLine.label(skipLabel)
    result.toList
  }

  def modifyInPlaceViaX(ctx: CompilationContext, target: LhsExpression, argument: Expression, opcode: MOpcode.Value): List[MLine] = {
    if (opcode == ADDB) ctx.env.eval(argument) match {
      case Some(NumericConstant(1, _)) =>
        return compileInc(ctx, target)
      case _ =>
    }
    val sizeInBytes = AbstractExpressionCompiler.getExpressionType(ctx, target).size
    val byteLoads = M6809ExpressionCompiler.compileToByteReads(ctx, argument, sizeInBytes)
    val needsAPreserved = byteLoads.tails.toList.tail.map(_.exists(_.exists(_.readsRegister(M6809Register.A))))
    val result = new ListBuffer[MLine]()
    val targetAddr: Option[Constant] = M6809ExpressionCompiler.compileAddressToX(ctx, target) match {
      case List(MLine(LDX, Immediate, addr, _, _)) => Some(addr)
      case List(MLine(LEAX, _, _, _, _)) => None
      case xs =>
        result ++= xs
        None
    }
    var firstNonzeroByte = 0
    for (i <- 0 until sizeInBytes) {
      val ldb = byteLoads(i)
      val magicConstant =
        ldb match {
          case List(MLine0(LDB, Immediate, NumericConstant(x, _))) => Some(x.toInt & 0xff)
          case _ => None
        }
      (opcode, magicConstant) match {

        case (ORB | EORB, Some(0)) =>
          // nothing
        case (ANDB, Some(0xff)) =>
          // nothing
        case (EORB, Some(0xff)) =>
          targetAddr match {
            case Some(addr) =>
              result += MLine.absolute(COM, addr + (sizeInBytes - 1 - i))
            case None =>
              result += MLine.indexedX(COM, sizeInBytes - 1 - i)
          }

        case (ADDB | SUBB | ADDA | SUBA, Some(0)) if i == firstNonzeroByte =>
           firstNonzeroByte = i + 1

        case (SUBA, _) =>
          targetAddr match {
            case Some(addr) =>
              val stashCC = i != firstNonzeroByte
              if (stashCC) result += MLine.pp(PSHS, M6809Register.CC)
              result ++= ldb
              if (needsAPreserved(i)) {
                // TODO: optimize?
                result += MLine.pp(PSHS, M6809Register.A)
              }
              result += MLine.pp(PSHS, M6809Register.B)
              result += MLine.immediate(LDA, if (i == firstNonzeroByte) 0x9a else 0x99)
              result += MLine.accessAndPullS(SUBA)
              if (needsAPreserved(i)) {
                // TODO: optimize?
                result += MLine.pp(PULS, M6809Register.B)
              }
              if (stashCC) result += MLine.pp(PULS, M6809Register.CC)
              result += MLine.absolute(if (i == firstNonzeroByte) ADDA else ADCA, addr + (sizeInBytes - 1 - i))
              result += MLine.inherent(DAA)
              result += MLine.absolute(STA, addr + (sizeInBytes - 1 - i))
              if (needsAPreserved(i)) {
                // TODO: optimize?
                result += MLine.tfr(M6809Register.B, M6809Register.A)
              }
            case None =>
              result ++= M6809ExpressionCompiler.stashXIfNeeded(ctx, ldb)
              result += MLine.pp(PSHS, M6809Register.B)
              result += MLine.immediate(LDA, if (i == firstNonzeroByte) 0x9a else 0x99)
              result += MLine.accessAndPullS(SUBA)
              result += MLine.indexedX(if (i == firstNonzeroByte) ADDA else ADCA, sizeInBytes - 1 - i)
              result += MLine.inherent(DAA)
              result += MLine.indexedX(STA, sizeInBytes - 1 - i)
          }

        case (ADDA, _) if i == firstNonzeroByte =>
          val lda = convertToLda(ldb)
          targetAddr match {
            case Some(addr) =>
              result ++= lda
              result += MLine.absolute(ADDA, addr + (sizeInBytes - 1 - i))
              result += MLine.inherent(DAA)
              result += MLine.absolute(STA, addr + (sizeInBytes - 1 - i))
            case None =>
              result ++= M6809ExpressionCompiler.stashXIfNeeded(ctx, lda)
              result += MLine.indexedX(ADDA, sizeInBytes - 1 - i)
              result += MLine.inherent(DAA)
              result += MLine.indexedX(STA, sizeInBytes - 1 - i)
          }

        case (ADDA, _) if i != firstNonzeroByte =>
          val lda = convertToLda(ldb)
          targetAddr match {
            case Some(addr) =>
              result ++= M6809ExpressionCompiler.stashCarryIfNeeded(ctx, lda)
              result += MLine.absolute(ADCA, addr + (sizeInBytes - 1 - i))
              result += MLine.inherent(DAA)
              result += MLine.absolute(STA, addr + (sizeInBytes - 1 - i))
            case None =>
              result ++= M6809ExpressionCompiler.stashCarryIfNeeded(ctx, M6809ExpressionCompiler.stashXIfNeeded(ctx, lda))
              result += MLine.indexedX(ADCA, sizeInBytes - 1 - i)
              result += MLine.inherent(DAA)
              result += MLine.indexedX(STA, sizeInBytes - 1 - i)
          }

        case (SUBB, _) if i == firstNonzeroByte =>
          targetAddr match {
            case Some(addr) =>
              result ++=  ldb
              result += MLine.pp(PSHS, M6809Register.B)
              result += MLine.absolute(LDB, addr + (sizeInBytes - 1 - i))
              result += MLine.accessAndPullS(SUBB)
              result += MLine.absolute(STB, addr + (sizeInBytes - 1 - i))
            case None =>
              result ++= M6809ExpressionCompiler.stashXIfNeeded(ctx, ldb)
              result += MLine.pp(PSHS, M6809Register.B)
              result += MLine.indexedX(LDB, sizeInBytes - 1 - i)
              result += MLine.accessAndPullS(SUBB)
              result += MLine.indexedX(STB, sizeInBytes - 1 - i)
          }

        case (SUBB, _) if i != firstNonzeroByte =>
          targetAddr match {
            case Some(addr) =>
              result ++= M6809ExpressionCompiler.stashCarryIfNeeded(ctx, ldb)
              result += MLine.pp(PSHS, M6809Register.B)
              result += MLine.absolute(LDB, addr + (sizeInBytes - 1 - i))
              result += MLine.accessAndPullS(SBCB)
              result += MLine.absolute(STB, addr + (sizeInBytes - 1 - i))
            case None =>
              result ++= M6809ExpressionCompiler.stashCarryIfNeeded(ctx, M6809ExpressionCompiler.stashXIfNeeded(ctx, ldb))
              result += MLine.pp(PSHS, M6809Register.B)
              result += MLine.indexedX(LDB, sizeInBytes - 1 - i)
              result += MLine.accessAndPullS(SBCB)
              result += MLine.indexedX(STB, sizeInBytes - 1 - i)
          }
        case (ADDB, _) if i != firstNonzeroByte =>
          targetAddr match {
            case Some(addr) =>
              result ++= M6809ExpressionCompiler.stashCarryIfNeeded(ctx, ldb)
              result += MLine.absolute(ADCB, addr + (sizeInBytes - 1 - i))
              result += MLine.absolute(STB, addr + (sizeInBytes - 1 - i))
            case None =>
              result ++= M6809ExpressionCompiler.stashCarryIfNeeded(ctx, M6809ExpressionCompiler.stashXIfNeeded(ctx, ldb))
              result += MLine.indexedX(ADCB, sizeInBytes - 1 - i)
              result += MLine.indexedX(STB, sizeInBytes - 1 - i)
          }
        case _ =>
          targetAddr match {
            case Some(addr) =>
              result ++= ldb
              result += MLine.absolute(opcode, addr + (sizeInBytes - 1 - i))
              result += MLine.absolute(STB, addr + (sizeInBytes - 1 - i))
            case None =>
              result ++= M6809ExpressionCompiler.stashXIfNeeded(ctx, ldb)
              result += MLine.indexedX(opcode, sizeInBytes - 1 - i)
              result += MLine.indexedX(STB, sizeInBytes - 1 - i)
          }
      }
    }
    result.toList
  }

  def compileComparison(ctx: CompilationContext, typ: ComparisonType.Value, l: Expression, r: Expression, branches: BranchSpec): List[MLine] = {
    typ match {
      case ComparisonType.GreaterSigned | ComparisonType.GreaterUnsigned | ComparisonType.LessOrEqualSigned | ComparisonType.LessOrEqualUnsigned =>
        return compileComparison(ctx, ComparisonType.flip(typ), r, l, branches)
      case _ =>
    }
    val targetLabel = branches match {
      case BranchIfTrue(label) => Some(label)
      case BranchIfFalse(label) => return compileComparison(ctx, ComparisonType.negate(typ), l, r, BranchIfTrue(label))
      case NoBranching => None
    }
    val ltype = AbstractExpressionCompiler.getExpressionType(ctx, l)
    val rtype = AbstractExpressionCompiler.getExpressionType(ctx, r)
    val size = ltype.size max rtype.size
    val lReads = M6809ExpressionCompiler.compileToByteReads(ctx, l, size)
    val rReads = M6809ExpressionCompiler.compileToByteReads(ctx, r, size)
    if (!lReads.forall(isSingleLdb) && !rReads.forall(isSingleLdb)) {
      ctx.log.error("Too complex comparison", l.position.orElse(r.position))
      return Nil
    }
    val result = ListBuffer[MLine]()
    typ match {
      case ComparisonType.Equal =>
        val skipLabel = ctx.nextLabel("co")
        for(i <- 0 until size) {
          (lReads(i), rReads(i)) match {
            case (List(MLine0(LDB, Immediate, NumericConstant(a, _))), List(MLine0(LDB, Immediate, NumericConstant(b, _)))) if a == b =>
              if (a != b) {
                targetLabel.foreach(l => result += MLine.shortBranch(BRA, skipLabel))
              }
            case (l, r) =>
              if (isSingleLdb(l)) {
                result ++= r ++ replaceLdb(l, CMPB)
              } else if (isSingleLdb(r)) {
                result ++= l ++ replaceLdb(r, CMPB)
              } else ???
          }
          if (i != size - 1) {
            targetLabel.foreach(l => result += MLine.longBranch(BNE, skipLabel))
          }
        }
        targetLabel.foreach(l => result += MLine.longBranch(BEQ, l))
        result += MLine.label(skipLabel)
      case ComparisonType.NotEqual =>
        for(i <- 0 until size) {
          (lReads(i), rReads(i)) match {
            case (List(MLine0(LDB, Immediate, NumericConstant(a, _))), List(MLine0(LDB, Immediate, NumericConstant(b, _))))  =>
              if (a != b) {
                targetLabel.foreach(l => result += MLine.longBranch(BRA, l))
              }
            case (l, r) =>
              if (isSingleLdb(l)) {
                result ++= r ++ replaceLdb(l, CMPB)
              } else if (isSingleLdb(r)) {
                result ++= l ++ replaceLdb(r, CMPB)
              } else ???
          }
          if (result.nonEmpty) {
            targetLabel.foreach(l => result += MLine.longBranch(BNE, l))
          }
        }
      case _ =>
        for(i <- 0 until size) {
          val l = lReads(i)
          val r = rReads(i)
          if (isSingleLdb(r)) {
            result ++= M6809ExpressionCompiler.stashCarryIfNeeded(ctx, l)
            result ++= replaceLdb(r, if (i == 0) SUBB else SBCB)
          } else {
            result ++= M6809ExpressionCompiler.stashCarryIfNeeded(ctx, r)
            result += MLine.pp(PSHS, M6809Register.B)
            result ++= M6809ExpressionCompiler.stashCarryIfNeeded(ctx, l)
            result += MLine.accessAndPullS(if (i == 0) SUBB else SBCB)
          }
        }
        val j = typ match {
          case ComparisonType.GreaterOrEqualSigned => MOpcode.BGE
          case ComparisonType.GreaterOrEqualUnsigned => MOpcode.BCC
          case ComparisonType.LessSigned => MOpcode.BLT
          case ComparisonType.LessUnsigned => MOpcode.BCS
        }
        targetLabel.foreach(l => result += MLine.longBranch(j, l))
    }
    result.toList

  }

}
