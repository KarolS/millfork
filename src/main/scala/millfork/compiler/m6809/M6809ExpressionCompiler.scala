package millfork.compiler.m6809

import millfork.CompilationFlag
import millfork.assembly.m6809.{DAccumulatorIndexed, Immediate, Indexed, InherentB, MLine, MLine0, MOpcode, RegisterSet, TwoRegisters}
import millfork.compiler.{AbstractExpressionCompiler, BranchIfFalse, BranchIfTrue, BranchSpec, ComparisonType, CompilationContext, NoBranching}
import millfork.node.{DerefExpression, Expression, FunctionCallExpression, GeneratedConstantExpression, IndexedExpression, LhsExpression, LiteralExpression, M6809Register, SeparateBytesExpression, SumExpression, VariableExpression}
import millfork.assembly.m6809.MOpcode._
import millfork.env.{AssemblyOrMacroParamSignature, BuiltInBooleanType, Constant, ConstantBooleanType, ConstantPointy, ExternFunction, FatBooleanType, FlagBooleanType, FunctionInMemory, FunctionPointerType, Label, M6809RegisterVariable, MacroFunction, MathOperator, MemoryAddressConstant, MemoryVariable, NonFatalCompilationException, NormalFunction, NormalParamSignature, NumericConstant, StackVariablePointy, ThingInMemory, Type, Variable, VariableInMemory, VariablePointy}

import scala.collection.GenTraversableOnce

/**
  * @author Karol Stasiak
  */
object MExpressionTarget extends Enumeration {
  val A, B, D, X, Y, U, NOTHING = Value

  def toLd(r: Value): MOpcode.Value = r match {
    case A => LDA
    case B => LDB
    case D => LDD
    case X => LDX
    case Y => LDY
    case U => LDU
    case _ => ???
  }

  def toLea(r: Value): MOpcode.Value = r match {
    case X => LEAX
    case Y => LEAY
    case U => LEAU
    case _ => ???
  }

  def size(r: Value): Int = r match {
    case A | B => 1
    case D | X | Y | U => 2
    case NOTHING => 0
  }

}

import MExpressionTarget.toLd

object M6809ExpressionCompiler extends AbstractExpressionCompiler[MLine] {

  def compile(ctx: CompilationContext, expr: Expression, target: MExpressionTarget.Value, branches: BranchSpec = BranchSpec.None): List[MLine] = try {
    val env = ctx.env
    val exprType = getExpressionType(ctx, expr)
    val targetSize = MExpressionTarget.size(target)
    if (branches != NoBranching) {
      (exprType, branches) match {
        case (FatBooleanType, _) =>
          return compile(ctx, FunctionCallExpression("!=", List(expr, LiteralExpression(0, 1))), target, branches)
        case (ConstantBooleanType(_, false), BranchIfTrue(_)) | (ConstantBooleanType(_, true), BranchIfFalse(_))=>
          return compile(ctx, expr, target, NoBranching)
        case (ConstantBooleanType(_, true), BranchIfTrue(x)) =>
          return compile(ctx, expr, target, NoBranching) :+ MLine.longBranch(BRA, x)
        case (ConstantBooleanType(_, false), BranchIfFalse(x)) =>
          return compile(ctx, expr, target, NoBranching) :+ MLine.longBranch(BRA, x)
        case _ => ()
      }
    }
    env.eval(expr) match {
      case Some(c) =>
        return target match {
          case MExpressionTarget.NOTHING => Nil
          case _ => List(MLine.immediate(toLd(target), c))
        }
      case None =>
    }
    expr match {
      case VariableExpression(name) =>
        val variable = env.get[Variable](name)
        exprType.size match {
          case 1 =>
            targetSize match {
              case 0 => Nil
              case 1 => List(MLine.variable(ctx, toLd(target), variable))
              case 2 => List(MLine.variable(ctx, LDB, variable)) ++ zeroextendB(ctx, target, exprType.isSigned)
            }
          case 2 =>
            targetSize match {
              case 0 => Nil
              case 2 => List(MLine.variable(ctx, toLd(target), variable))
            }
        }
      case LiteralExpression(c, _) =>
        target match {
          case MExpressionTarget.NOTHING => Nil
          case _ => List(MLine.immediate(MExpressionTarget.toLd(target), NumericConstant(c, MExpressionTarget.size(target))))
        }
      case DerefExpression(inner, offset, _) =>
        compileToX(ctx, inner) :+ MLine(toLd(target), Indexed(M6809Register.X, indirect = false), NumericConstant(offset, 2))
      case IndexedExpression(name, index) =>
        env.getPointy(name) match {
          case c: ConstantPointy =>
            val (variableIndex, constIndex) = env.evalVariableAndConstantSubParts(index)
            val constantOffset = (c.value + constIndex).quickSimplify
            variableIndex match {
              case (Some(ix)) =>
                compileToX(ctx, ix) ++ (
                  targetSize match {
                    case 0 => Nil
                    case 1 => List(MLine.indexedX(toLd(target), constantOffset))
                    case 2 => List(MLine.indexedX(LDB, constantOffset)) ++ zeroextendB(ctx, target, exprType.isSigned)
                  })
              case None =>
                targetSize match {
                  case 0 => Nil
                  case 1 => List(MLine.absolute(toLd(target), constantOffset))
                  case 2 => List(MLine.absolute(LDB, constantOffset)) ++ zeroextendB(ctx, target, exprType.isSigned)
                }
            }
          case v:VariablePointy =>
            val (prepareIndex, offset): (List[MLine], Constant) = ctx.env.eval(index) match {
              case Some(ix) => List(MLine.absolute(LDX, v.addr)) -> (ix * v.elementType.size).quickSimplify
              case _ =>
                v.indexType.size match {
                  case 1 | 2 =>
                    (compileToD(ctx, index) ++ List(
                      MLine.absolute(LDX, v.addr),
                      MLine(LEAX, DAccumulatorIndexed(M6809Register.X, indirect = false), Constant.Zero)
                    )) -> Constant.Zero
                }
            }
            prepareIndex ++ (targetSize match {
              case 0 => Nil
              case 1 => List(MLine.indexedX(toLd(target), offset))
              case 2 => List(MLine.indexedX(LDB, offset)) ++ zeroextendB(ctx, target, exprType.isSigned)
            })
          case v:StackVariablePointy =>
            ctx.env.eval(index) match {
              case Some(ix) => List(MLine.variablestack(ctx, LDX, v.offset), MLine.indexedX(LDB, ix * v.elementType.size))
            }
        }
      case e@SumExpression(expressions, decimal) =>
        getArithmeticParamMaxSize(ctx, expressions.map(_._2)) match {
          case 1 => M6809Buitins.compileByteSum(ctx, e, fromScratch = true) ++ targetifyB(ctx, target, isSigned = false)
          case 2 => M6809Buitins.compileWordSum(ctx, e, fromScratch = true) ++ targetifyD(ctx, target)
        }
      case SeparateBytesExpression(hi, lo) =>
        val h = compile(ctx, hi, MExpressionTarget.A)
        val l = compile(ctx, lo, MExpressionTarget.B)
        (if (l.exists(_.changesRegister(M6809Register.A))) {
          if (h.exists(_.changesRegister(M6809Register.B))) h ++ stashAIfNeeded(ctx, l) else l ++ h
        } else h ++ l) ++ targetifyD(ctx, target)
      case fce@FunctionCallExpression(functionName, params) =>
        functionName match {
          case "not" =>
            assertBool(ctx, "not", params, 1)
            compile(ctx, params.head, target, branches.flip)
          case "nonet" =>
            if (params.length != 1) {
              ctx.log.error("Invalid number of parameters", fce.position)
              Nil
            } else {
              ctx.env.eval(params.head) match {
                case Some(c) =>
                  target match {
                    case MExpressionTarget.NOTHING => Nil
                    case _ => List(MLine.immediate(toLd(target), c))
                  }
                case _ =>
                  compileToB(ctx, params.head) ++ List(MLine.immediate(LDA, 0), MLine.inherentA(ROL)) ++ targetifyD(ctx, target)
              }
            }
          case "hi" =>
            if (params.length != 1) {
              ctx.log.error("Invalid number of parameters", fce.position)
              Nil
            } else {
              compileToD(ctx, params.head) ++ targetifyA(ctx, target, isSigned = false)
            }
          case "lo" =>
            if (params.length != 1) {
              ctx.log.error("Invalid number of parameters", fce.position)
              Nil
            } else {
              compileToD(ctx, params.head) ++ targetifyB(ctx, target, isSigned = false)
            }
          case "call" =>
            params match {
              case List(fp) =>
                getExpressionType(ctx, fp) match {
                  case FunctionPointerType(_, _, _, _, Some(v)) if (v.name == "void") =>
                    compileToX(ctx, fp) :+ MLine.absolute(JSR, env.get[ThingInMemory]("call"))
                  case _ =>
                    ctx.log.error("Not a function pointer", fp.position)
                    compile(ctx, fp, MExpressionTarget.NOTHING)
                }
              case List(fp, param) =>
                getExpressionType(ctx, fp) match {
                  case FunctionPointerType(_, _, _, Some(pt), Some(v)) =>
                    if (pt.size > 2 || pt.size < 1) {
                      ctx.log.error("Invalid parameter type", param.position)
                      compile(ctx, fp, MExpressionTarget.NOTHING) ++ compile(ctx, param, MExpressionTarget.NOTHING)
                    } else if (getExpressionType(ctx, param).isAssignableTo(pt)) {
                      // TODO: optimal compile order
                      pt.size match {
                        case 1 =>
                          compileToB(ctx, param) ++ stashBIfNeeded(ctx, compileToX(ctx, fp)) :+ MLine.absolute(JSR, env.get[ThingInMemory]("call"))
                        case 2 =>
                          compileToD(ctx, param) ++ stashDIfNeeded(ctx, compileToX(ctx, fp)) :+ MLine.absolute(JSR, env.get[ThingInMemory]("call"))
                      }
                    } else {
                      ctx.log.error("Invalid parameter type", param.position)
                      compile(ctx, fp, MExpressionTarget.NOTHING) ++ compile(ctx, param, MExpressionTarget.NOTHING)
                    }
                  case _ =>
                    ctx.log.error("Not a function pointer", fp.position)
                    compile(ctx, fp, MExpressionTarget.NOTHING) ++ compile(ctx, param, MExpressionTarget.NOTHING)
                }
              case _ =>
                ctx.log.error("Invalid call syntax", fce.position)
                Nil
            }
          case "*" =>
            getArithmeticParamMaxSize(ctx, params) match {
              case 1 => M6809MulDiv.compileByteMultiplication(ctx, params, updateDerefX = false) ++ targetifyB(ctx, target, isSigned = false)
              case 2 => M6809MulDiv.compileWordMultiplication(ctx, params, updateDerefX = false) ++ targetifyD(ctx, target)
            }
          case "*'" => ctx.log.error("Decimal multiplication not implemented yet", fce.position); Nil
          case "/" =>
            assertArithmeticBinary(ctx, params) match {
              case (l, r, 1) => M6809MulDiv.compileByteDivision(ctx, Some(l), r, mod=false) ++ targetifyB(ctx, target, isSigned = false)
              case (l, r, 2) => M6809MulDiv.compileWordDivision(ctx, Some(l), r, mod=false) ++ targetifyD(ctx, target)
            }
          case "%%" =>
            assertArithmeticBinary(ctx, params) match {
              case (l, r, 1) => M6809MulDiv.compileByteDivision(ctx, Some(l), r, mod=true) ++ targetifyB(ctx, target, isSigned = false)
              case (l, r, 2) =>
                if (AbstractExpressionCompiler.getExpressionType(ctx, r).size == 1) {
                  M6809MulDiv.compileWordDivision(ctx, Some(l), r, mod=true) ++ targetifyDWithNarrowing(ctx, target)
                } else {
                  M6809MulDiv.compileWordDivision(ctx, Some(l), r, mod=true) ++ targetifyD(ctx, target)
                }
            }
          case "&" =>
            getArithmeticParamMaxSize(ctx, params) match {
              case 1 => M6809Buitins.compileByteBitwise(ctx, params, fromScratch = true, ANDB, MathOperator.And, 0xff) ++ targetifyB(ctx, target, isSigned = false)
              case 2 => M6809Buitins.compileWordBitwise(ctx, params, fromScratch = true, ANDA, ANDB, MathOperator.And, 0xffff) ++ targetifyD(ctx, target)
            }
          case "|" =>
            getArithmeticParamMaxSize(ctx, params) match {
              case 1 => M6809Buitins.compileByteBitwise(ctx, params, fromScratch = true, ORB, MathOperator.Or, 0) ++ targetifyB(ctx, target, isSigned = false)
              case 2 => M6809Buitins.compileWordBitwise(ctx, params, fromScratch = true, ORA, ORB, MathOperator.Or, 0) ++ targetifyD(ctx, target)
            }
          case "^" =>
            getArithmeticParamMaxSize(ctx, params) match {
              case 1 => M6809Buitins.compileByteBitwise(ctx, params, fromScratch = true, EORB, MathOperator.Exor, 0) ++ targetifyB(ctx, target, isSigned = false)
              case 2 => M6809Buitins.compileWordBitwise(ctx, params, fromScratch = true, EORA, EORB, MathOperator.Exor, 0) ++ targetifyD(ctx, target)
            }
          case "&&" =>
            assertBool(ctx, "&&", params)
            branches match {
              case BranchIfFalse(_) =>
                params.flatMap(compile(ctx, _, target, branches))
              case _ =>
                val skip = ctx.nextLabel("an")
                params.init.flatMap(compile(ctx, _, target, BranchIfFalse(skip))) ++
                  compile(ctx, params.last, target, branches) ++
                  List(MLine.label(skip))
            }
          case "||" =>
            assertBool(ctx, "||", params)
            branches match {
              case BranchIfTrue(_) =>
                params.flatMap(compile(ctx, _, target, branches))
              case _ =>
                val skip = ctx.nextLabel("or")
                params.init.flatMap(compile(ctx, _, target, BranchIfTrue(skip))) ++
                  compile(ctx, params.last, target, branches) ++
                  List(MLine.label(skip))
            }
          case "==" =>
            val size = params.map(p => getExpressionType(ctx, p).size).max
            compileTransitiveRelation(ctx, "==", params, target, branches) { (l, r) =>
              size match {
                case 1 => M6809Comparisons.compile8BitComparison(ctx, ComparisonType.Equal, l, r, branches)
                case 2 => M6809Comparisons.compile16BitComparison(ctx, ComparisonType.Equal, l, r, branches)
                case _ => ???
              }
            }
          case "!=" =>
            val size = params.map(p => getExpressionType(ctx, p).size).max
            compileTransitiveRelation(ctx, "!=", params, target, branches) { (l, r) =>
              size match {
                case 1 => M6809Comparisons.compile8BitComparison(ctx, ComparisonType.NotEqual, l, r, branches)
                case 2 => M6809Comparisons.compile16BitComparison(ctx, ComparisonType.NotEqual, l, r, branches)
                case _ => ???
              }
            }
          case "<" =>
            val (size, signed) = assertArithmeticComparison(ctx, params)
            compileTransitiveRelation(ctx, "<", params, target, branches) { (l, r) =>
              size match {
                case 1 => M6809Comparisons.compile8BitComparison(ctx, if (signed) ComparisonType.LessSigned else ComparisonType.LessUnsigned, l, r, branches)
                case 2 => M6809Comparisons.compile16BitComparison(ctx, if (signed) ComparisonType.LessSigned else ComparisonType.LessUnsigned, l, r, branches)
                case _ => ???
              }
            }
          case ">" =>
            val (size, signed) = assertArithmeticComparison(ctx, params)
            compileTransitiveRelation(ctx, ">", params, target, branches) { (l, r) =>
              size match {
                case 1 => M6809Comparisons.compile8BitComparison(ctx, if (signed) ComparisonType.GreaterSigned else ComparisonType.GreaterUnsigned, l, r, branches)
                case 2 => M6809Comparisons.compile16BitComparison(ctx, if (signed) ComparisonType.GreaterSigned else ComparisonType.GreaterUnsigned, l, r, branches)
                case _ => ???
              }
            }
          case "<=" =>
            val (size, signed) = assertArithmeticComparison(ctx, params)
            compileTransitiveRelation(ctx, "<=", params, target, branches) { (l, r) =>
              size match {
                case 1 => M6809Comparisons.compile8BitComparison(ctx, if (signed) ComparisonType.LessOrEqualSigned else ComparisonType.LessOrEqualUnsigned, l, r, branches)
                case 2 => M6809Comparisons.compile16BitComparison(ctx, if (signed) ComparisonType.LessOrEqualSigned else ComparisonType.LessOrEqualUnsigned, l, r, branches)
                case _ => ???
              }
            }
          case ">=" =>
            val (size, signed) = assertArithmeticComparison(ctx, params)
            compileTransitiveRelation(ctx, ">=", params, target, branches) { (l, r) =>
              size match {
                case 1 => M6809Comparisons.compile8BitComparison(ctx, if (signed) ComparisonType.GreaterOrEqualSigned else ComparisonType.GreaterOrEqualUnsigned, l, r, branches)
                case 2 => M6809Comparisons.compile16BitComparison(ctx, if (signed) ComparisonType.GreaterOrEqualSigned else ComparisonType.GreaterOrEqualUnsigned, l, r, branches)
                case _ => ???
              }
            }
          case "<<" =>
            getArithmeticParamMaxSize(ctx, params) match {
              case 1 => compileToB(ctx, params(0)) ++ M6809Buitins.compileByteShiftForB(ctx, params(1), left = true) ++ targetifyB(ctx, target, isSigned = false)
              case 2 => compileToD(ctx, params(0)) ++ M6809Buitins.compileWordShiftForD(ctx, params(1), left = true) ++ targetifyD(ctx, target)
            }
          case ">>" =>
            getArithmeticParamMaxSize(ctx, params) match {
              case 1 => compileToB(ctx, params(0)) ++ M6809Buitins.compileByteShiftForB(ctx, params(1), left = false) ++ targetifyB(ctx, target, isSigned = false)
              case 2 => compileToD(ctx, params(0)) ++ M6809Buitins.compileWordShiftForD(ctx, params(1), left = false) ++ targetifyD(ctx, target)
            }
          case ">>>>" =>
            // TODO: this works, but is really suboptimal
            getArithmeticParamMaxSize(ctx, params) match {
              case 1 | 2 => compileToD(ctx, params(0)) ++
                List(MLine.immediate(ANDA, 1)) ++
                M6809Buitins.compileWordShiftForD(ctx, params(1), left = false) ++
                targetifyB(ctx, target, isSigned = false)
            }
          case "<<'" =>
            assertArithmeticBinary(ctx, params) match {
              case (l, r, 1) => M6809DecimalBuiltins.compileByteDecimalShiftLeft(ctx, Some(l), r) ++ targetifyA(ctx, target, isSigned = false)
              case (l, r, 2) => ???
              case (l, r, _) => ???
            }
          case ">>'" => ???
          case "+=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 => M6809Buitins.perform8BitInPlace(ctx, l, r, ADDB)
              case 2 => M6809Buitins.perform16BitInPlace(ctx, l, r, ADDD, commutative = true)
              case _ => ctx.log.error("Long addition not implemented yet", fce.position); Nil
            }
          case "+'=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                val lc = compileAddressToX(ctx, l)
                val rc = compileToA(ctx, r)
                val add = List(MLine.indexedX(ADDA, 0), MLine.inherent(DAA), MLine.indexedX(STA, 0))
                (lc.exists(_.changesRegister(M6809Register.A)), rc.exists(_.changesRegister(M6809Register.X))) match {
                  case (false, false) => rc ++ lc ++ add
                  case (false, true) => rc ++ lc ++ add
                  case (true, false) => lc ++ rc ++ add
                  case (true, true) => rc ++ stashAIfNeeded(ctx, lc) ++ add
                }
              case 2 => ???
              case _ => ???
            }
          case "-=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 => M6809Buitins.perform8BitInPlace(ctx, l, r, SUBB)
              case 2 => M6809Buitins.perform16BitInPlace(ctx, l, r, SUBD, commutative = false)
              case _ => ???
            }
          case "-'=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                val lc = compileAddressToX(ctx, l)
                val rc = compileToB(ctx, r)
                lc ++ List(MLine.pp(PSHS, M6809Register.B)) ++
                  rc ++ List(MLine.pp(PSHS, M6809Register.B), MLine.immediate(LDA, 0x9a), MLine.accessAndPullS(SUBA), MLine.accessAndPullS(ADDA), MLine.inherent(DAA), MLine.indexedX(STA, 0))
              case 2 => ???
              case _ => ???
            }
          case "*=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 => compileAddressToX(ctx, l) ++ M6809MulDiv.compileByteMultiplication(ctx, List(r), updateDerefX = true)
              case 2 => compileAddressToX(ctx, l) ++ M6809MulDiv.compileWordMultiplication(ctx, List(r), updateDerefX = true)
            }
          case "*'=" => ctx.log.error("Decimal multiplication not implemented yet", fce.position); Nil
          case "/=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 => compileAddressToX(ctx, l) ++ M6809MulDiv.compileByteDivision(ctx, None, r, mod=false)
              case 2 => compileAddressToX(ctx, l) ++ M6809MulDiv.compileWordDivision(ctx, None, r, mod=false)
            }
          case "%%=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 => compileAddressToX(ctx, l) ++ M6809MulDiv.compileByteDivision(ctx, None, r, mod=true)
              case 2 => compileAddressToX(ctx, l) ++ M6809MulDiv.compileWordDivision(ctx, None, r, mod=true)
            }
          case "&=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 => M6809Buitins.perform8BitInPlace(ctx, l, r, ANDB)
              case 2 => M6809Buitins.perform16BitInPlace(ctx, l, r, ANDA, ANDB, commutative = true)
              case _ => ???
            }
          case "|=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 => M6809Buitins.perform8BitInPlace(ctx, l, r, ORB)
              case 2 => M6809Buitins.perform16BitInPlace(ctx, l, r, ORA, ORB, commutative = true)
              case _ => ???
            }
          case "^=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 => M6809Buitins.perform8BitInPlace(ctx, l, r, EORB)
              case 2 => M6809Buitins.perform16BitInPlace(ctx, l, r, EORA, EORB, commutative = true)
              case _ => ???
            }
          case "<<=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            // TODO: optimize shifts directly in memory
            size match {
              case 1 =>
                handleInPlaceModification(ctx, l, 1, M6809Buitins.compileByteShiftForB(ctx, r, left = true))
              case 2 =>
                handleInPlaceModification(ctx, l, 2, M6809Buitins.compileWordShiftForD(ctx, r, left = true))
            }
          case "<<'=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                compileAddressToX(ctx, l) ++ M6809DecimalBuiltins.compileByteDecimalShiftLeft(ctx, None, r)
              case 2 => ???
            }
          case ">>=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            // TODO: optimize shifts directly in memory
            size match {
              case 1 => handleInPlaceModification(ctx, l, 1, M6809Buitins.compileByteShiftForB(ctx, r, left = false))
              case 2 => handleInPlaceModification(ctx, l, 2, M6809Buitins.compileWordShiftForD(ctx, r, left = false))
            }
          case ">>'=" => ???
          case ">>>>=" => ???
          case _ =>
            env.maybeGet[Type](fce.functionName) match {
              case Some(typ) =>
                val sourceType = validateTypeCastAndGetSourceExpressionType(ctx, typ, params)
                if (sourceType.isBoollike) {
                  return compileToFatBooleanInB(ctx, params.head) ++ targetifyB(ctx, target, isSigned = false)
                }
                return typ.size match {
                  // TODO: alternating signedness?
                  case 1 => compileToB(ctx, params.head) ++ targetifyB(ctx, target, isSigned = typ.isSigned)
                  case 2 => compileToD(ctx, params.head) ++ targetifyD(ctx, target)
                  case _ => ???
                }
              case None =>
              // fallthrough to the lookup below
            }
            val f = lookupFunction(ctx, fce)
            f match {
              case function: MacroFunction =>
                val (paramPreparation, statements) = M6809MacroExpander.inlineFunction(ctx, function, params, expr.position)
                paramPreparation ++ statements.flatMap { s =>
                  M6809StatementCompiler.compile(ctx, s) match {
                    case (code, Nil) => code
                    case (code, _) => ctx.log.error("Invalid statement in macro expansion", fce.position); code
                  }
                }
              case _:FunctionInMemory =>
                val prepareParams: List[MLine] = f.params match {
                  case NormalParamSignature(List(param)) if param.typ.size == 1 =>
                    compileToB(ctx, params.head)
                  case NormalParamSignature(List(param)) if param.typ.size == 2 =>
                    compileToD(ctx, params.head)
                  case NormalParamSignature(signature) =>
                    params.zip(signature).flatMap { case (paramExpr, paramVar) =>
                      val callCtx = callingContext(ctx, f.name, paramVar)
                      paramVar.typ.size match {
                        case 1 =>
                          compileToB(ctx, paramExpr) ++ storeB(callCtx, VariableExpression(paramVar.name + "`aa"))
                        case 2 =>
                          compileToD(ctx, paramExpr) ++ storeD(callCtx, VariableExpression(paramVar.name + "`aa"))
                        case _ =>
                          ???
                      }
                    }
                  case AssemblyOrMacroParamSignature(signature) =>
                    params.zip(signature).flatMap { case (e, a) =>
                      val compiled = a.variable match {
                        case M6809RegisterVariable(M6809Register.A, _) => compileToA(ctx, e)
                        case M6809RegisterVariable(M6809Register.B, _) => compileToB(ctx, e)
                        case M6809RegisterVariable(M6809Register.D, _) => compileToD(ctx, e)
                        case M6809RegisterVariable(M6809Register.X, _) => compileToX(ctx, e)
                        case M6809RegisterVariable(M6809Register.Y, _) => compileToY(ctx, e)
                        case M6809RegisterVariable(M6809Register.U, _) => compileToU(ctx, e)
                        case _ => ???
                      }
                      if (compiled.length > 1) ???
                      compiled
                    }
                  case _ => ???
                }
                val actualCall = f match {
                  case nf:NormalFunction =>
                    List(MLine.absolute(JSR, nf.toAddress))
                  case nf:ExternFunction =>
                    List(MLine.absolute(JSR, nf.toAddress))
                  case _ =>
                    println("Unsupported function: "  + f.name)
                    ???
                }
                val storeResult = f.returnType.size match {
                  case 1 => targetifyB(ctx, target, f.returnType.isSigned)
                  case 2 => targetifyD(ctx, target)
                  case _ =>
                    if (target == MExpressionTarget.NOTHING) {
                      Nil
                    } else {
                      println("Unsupported function: " + f.name)
                      ???
                    }
                }
                prepareParams ++ actualCall ++ storeResult
            }
        }
      case _ => ???
    }
  } catch {
    case ex: NonFatalCompilationException =>
      ctx.log.error(ex.getMessage, ex.position.orElse(expr.position))
      Nil
  }

  def compileToA(ctx: CompilationContext, expr: Expression): List[MLine] = compile(ctx, expr, MExpressionTarget.A)

  def compileToB(ctx: CompilationContext, expr: Expression): List[MLine] = compile(ctx, expr, MExpressionTarget.B)

  def compileToFatBooleanInB(ctx: CompilationContext, expr: Expression): List[MLine] = {
    val sourceType = AbstractExpressionCompiler.getExpressionType(ctx, expr)
    sourceType match {
      case FatBooleanType => compileToB(ctx, expr)
      case t: ConstantBooleanType =>
        List(MLine.immediate(MOpcode.LDB, if (t.value) 1 else 0))
      case BuiltInBooleanType | _: FlagBooleanType =>
        val label = ctx.env.nextLabel("bo")
        val condition = compile(ctx, expr, MExpressionTarget.NOTHING, BranchIfFalse(label))
        val conditionWithoutJump = condition.init

        if (conditionWithoutJump.exists(l => l.parameter match {
          case MemoryAddressConstant(Label(l)) if l == label => true
          case _ => false
        })){
          val label2 = ctx.env.nextLabel("bo")
          return condition ++ List(
            MLine.immediate(LDB, 1),
            MLine.shortBranch(BRA, label2),
            MLine.label(label),
            MLine.immediate(LDB, 0),
            MLine.label(label2),
            MLine.inherentB(ROL)
          )
        }

        condition.last.opcode match {
          case BCC =>
            conditionWithoutJump ++ List(MLine.immediate(LDB, 0), MLine.inherentB(ROL))
          case BCS =>
            conditionWithoutJump ++ List(MLine.immediate(LDB, 0), MLine.inherentB(ROL), MLine.immediate(EORB, 1))
          case BVC | BVS =>
            conditionWithoutJump ++ List(
              MLine.immediate(LDB, 0),
              condition.last,
              MLine.inherentB(INC),
              MLine.label(label)
            )
          case _ =>
            conditionWithoutJump ++ List(
              MLine.immediate(ANDCC, 0xfe),
              condition.last,
              MLine.immediate(ORCC, 1),
              MLine.label(label),
              MLine.immediate(LDB, 0),
              MLine.inherentB(ROL)
            )
        }
      case _ =>
        println(sourceType)
        println(expr)
        ???
    }
  }

  def compileToD(ctx: CompilationContext, expr: Expression): List[MLine] = compile(ctx, expr, MExpressionTarget.D)

  def compileToX(ctx: CompilationContext, expr: Expression): List[MLine] = compile(ctx, expr, MExpressionTarget.X)

  def compileToY(ctx: CompilationContext, expr: Expression): List[MLine] = compile(ctx, expr, MExpressionTarget.Y)

  def compileToU(ctx: CompilationContext, expr: Expression): List[MLine] = compile(ctx, expr, MExpressionTarget.U)

  def zeroextendB(ctx: CompilationContext, target: MExpressionTarget.Value, isSigned: Boolean): List[MLine] = {
    val extendToA = if (isSigned) MLine.inherent(SEX) else MLine.immediate(LDA, 0)
    target match {
      case MExpressionTarget.D => List(extendToA)
      case MExpressionTarget.X => List(extendToA, MLine.tfr(M6809Register.D, M6809Register.X))
      case MExpressionTarget.Y => List(extendToA, MLine.tfr(M6809Register.D, M6809Register.Y))
      case MExpressionTarget.U => List(extendToA, MLine.tfr(M6809Register.D, M6809Register.U))
      case _ => ???
    }
  }

  def stashAIfNeeded(ctx: CompilationContext, lines: List[MLine]): List[MLine] = {
    if (lines.exists(_.changesRegister(M6809Register.A))) MLine.pp(PSHS, M6809Register.A) :: (lines :+ MLine.pp(PULS, M6809Register.A))
    else lines
  }

  def stashBIfNeeded(ctx: CompilationContext, lines: List[MLine]): List[MLine] = {
    if (lines.exists(_.changesRegister(M6809Register.B))) MLine.pp(PSHS, M6809Register.B) :: (lines :+ MLine.pp(PULS, M6809Register.B))
    else lines
  }

  def stashDIfNeeded(ctx: CompilationContext, lines: List[MLine]): List[MLine] = {
    if (lines.exists(_.changesRegister(M6809Register.D))) MLine.pp(PSHS, M6809Register.D) :: (lines :+ MLine.pp(PULS, M6809Register.D))
    else lines
  }

  def stashXIfNeeded(ctx: CompilationContext, lines: List[MLine]): List[MLine] = {
    if (lines.exists(_.changesRegister(M6809Register.X))) MLine.pp(PSHS, M6809Register.X) :: (lines :+ MLine.pp(PULS, M6809Register.X))
    else lines
  }

  def storeA(ctx: CompilationContext, target: LhsExpression): List[MLine] = store8(ctx, target, stashAIfNeeded, STA)

  def storeB(ctx: CompilationContext, target: LhsExpression): List[MLine] = store8(ctx, target, stashBIfNeeded, STB)

  @inline
  private def store8(ctx: CompilationContext, target: LhsExpression, stashIfNeeded: (CompilationContext, List[MLine]) => List[MLine], store: MOpcode.Value): List[MLine] = {
    target match {
      case VariableExpression(name) =>
        val variable = ctx.env.get[Variable](name)
        List(MLine.variable(ctx, store, variable))
      case DerefExpression(inner, offset, _) =>
        stashIfNeeded(ctx, compileToX(ctx, inner)) :+ MLine.indexedX(store, NumericConstant(offset, 2))
      case IndexedExpression(name, index) =>
        ctx.env.getPointy(name) match {
          case p: ConstantPointy =>
            if (p.readOnly) {
              ctx.log.error("Writing to a constant array", target.position)
            }
            val (variableIndex, constOffset) = ctx.env.evalVariableAndConstantSubParts(index)
            val effectiveBase = (p.value + constOffset).quickSimplify
            variableIndex match {
              case Some(ix) =>
                stashIfNeeded(ctx, compileToX(ctx, ix)) :+ MLine.indexedX(store, effectiveBase)
              case None =>
                List(MLine.absolute(store, effectiveBase))
            }
          case v: VariablePointy =>
            ctx.env.eval(index) match {
              case Some(ix) => List(MLine.absolute(LDX, v.addr), MLine.indexedX(store, ix * v.elementType.size))
              case _ =>
                v.indexType.size match {
                  case 1 | 2 =>
                    stashIfNeeded(ctx,
                      MLine.absolute(LDX, v.addr) ::
                        (compileToD(ctx, index) :+
                          MLine(LEAX, DAccumulatorIndexed(M6809Register.X, indirect = false), Constant.Zero))) :+
                      MLine.indexedX(store, Constant.Zero)
                }
            }
          case v: StackVariablePointy =>
            ctx.env.eval(index) match {
              case Some(ix) => List(MLine.variablestack(ctx, LDX, v.offset), MLine.indexedX(store, ix * v.elementType.size))
            }
        }
    }
  }

  def storeD(ctx: CompilationContext, target: LhsExpression): List[MLine] = {
    target match {
      case VariableExpression(name) =>
        val variable = ctx.env.get[Variable](name)
        List(MLine.variable(ctx, STD, variable))
      case SeparateBytesExpression(hi: LhsExpression, lo: LhsExpression) =>
        val sh = storeA(ctx, hi)
        val sl = storeB(ctx, lo)
        stashBIfNeeded(ctx, sh) ++ sl // TODO: optimize
      case DerefExpression(inner, offset, _) =>
        stashDIfNeeded(ctx, compileToX(ctx, inner)) :+ MLine(STD, Indexed(M6809Register.X, indirect = false), NumericConstant(offset, 2))
    }
  }

  def targetifyB(ctx: CompilationContext, target: MExpressionTarget.Value, isSigned: Boolean): List[MLine] = target match {
    case MExpressionTarget.NOTHING => Nil
    case MExpressionTarget.B => Nil
    case MExpressionTarget.A => List(MLine.tfr(M6809Register.B, M6809Register.A))
    case MExpressionTarget.D | MExpressionTarget.X | MExpressionTarget.Y | MExpressionTarget.U => zeroextendB(ctx, target, isSigned)
  }

  def targetifyA(ctx: CompilationContext, target: MExpressionTarget.Value, isSigned: Boolean): List[MLine] = target match {
    case MExpressionTarget.NOTHING => Nil
    case MExpressionTarget.A => Nil
    case MExpressionTarget.B => List(MLine.tfr(M6809Register.A, M6809Register.B))
    case MExpressionTarget.D | MExpressionTarget.X | MExpressionTarget.Y | MExpressionTarget.U =>
      List(MLine.tfr(M6809Register.A, M6809Register.B)) ++ zeroextendB(ctx, target, isSigned)
  }

  def targetifyD(ctx: CompilationContext, target: MExpressionTarget.Value): List[MLine] = target match {
    case MExpressionTarget.NOTHING => Nil
    case MExpressionTarget.D => Nil
    case MExpressionTarget.X => List(MLine.tfr(M6809Register.D, M6809Register.X))
    case MExpressionTarget.Y => List(MLine.tfr(M6809Register.D, M6809Register.Y))
    case MExpressionTarget.U => List(MLine.tfr(M6809Register.D, M6809Register.U))
  }

  //Assume that A=0 and the source type is an unsigned byte stored in D
  def targetifyDWithNarrowing(ctx: CompilationContext, target: MExpressionTarget.Value): List[MLine] = target match {
    case MExpressionTarget.NOTHING => Nil
    case MExpressionTarget.B => Nil
    case MExpressionTarget.A => List(MLine.tfr(M6809Register.B, M6809Register.A))
    case MExpressionTarget.D => Nil
    case MExpressionTarget.X => List(MLine.tfr(M6809Register.D, M6809Register.X))
    case MExpressionTarget.Y => List(MLine.tfr(M6809Register.D, M6809Register.Y))
    case MExpressionTarget.U => List(MLine.tfr(M6809Register.D, M6809Register.U))
  }

  def compileAddressToX(ctx: CompilationContext, expr: LhsExpression): List[MLine] = {
    expr match {
      case VariableExpression(name) =>
        val variable = ctx.env.get[VariableInMemory](name)
        List(MLine.immediate(MOpcode.LDX, variable.toAddress))
      case DerefExpression(inner, offset, _) =>
        compileToX(ctx, inner) :+ MLine.indexedX(MOpcode.LEAX, Constant(offset))
      case IndexedExpression(aname, index) =>
        ctx.env.getPointy(aname) match {
          case p: VariablePointy => compileToD(ctx, index #*# p.elementType.size) ++ List(MLine.absolute(ADDD, p.addr), MLine.tfr(M6809Register.D, M6809Register.X))
          case p: ConstantPointy =>
            if (p.sizeInBytes.exists(_ < 255)) {
              compileToB(ctx, index #*# p.elementType.size) ++ List(MLine.immediate(LDX, p.value), MLine.inherent(ABX))
            } else {
              compileToX(ctx, index #*# p.elementType.size) :+ MLine.indexedX(LEAX, p.value)
            }
          case p:StackVariablePointy => compileToD(ctx, index #*# p.elementType.size) ++ List(MLine.variablestack(ctx, ADDD, p.offset), MLine.tfr(M6809Register.D, M6809Register.X))
        }
      case _ => ???
    }
  }

  def handleInPlaceModification(ctx: CompilationContext, l: LhsExpression, size: Int, r: List[MLine]): List[MLine] = {
    compileAddressToX(ctx, l) match {
      case List(MLine0(LDX, Immediate, addr)) =>
        size match {
          case 1 => List(MLine.absolute(LDB, addr)) ++ r ++ List(MLine.absolute(STB, addr))
          case 2 => List(MLine.absolute(LDD, addr)) ++ r ++ List(MLine.absolute(STD, addr))
        }
      case lc =>
        size match {
          case 1 =>
            lc ++
              List(MLine.indexedX(LDB, Constant.Zero)) ++
              stashXIfNeeded(ctx, r) ++
              List(MLine.indexedX(STB, Constant.Zero))
          case 2 =>
            lc ++
              List(MLine.indexedX(LDD, Constant.Zero)) ++
              stashXIfNeeded(ctx, r) ++
              List(MLine.indexedX(LDD, Constant.Zero))
        }
    }
  }


  private def compileTransitiveRelation(ctx: CompilationContext,
                                        operator: String,
                                        params: List[Expression],
                                        target: MExpressionTarget.Value,
                                        branches: BranchSpec)(binary: (Expression, Expression) => List[MLine]): List[MLine] = {
    params match {
      case List(l, r) => binary(l, r)
      case List(_) | Nil =>
        ctx.log.fatal("")
      case _ =>
        params.tail.init.foreach { e =>
          if (ctx.env.eval(e).isEmpty) e match {
            case VariableExpression(_) =>
            case LiteralExpression(_, _) =>
            case GeneratedConstantExpression(_, _) =>
            case IndexedExpression(_, VariableExpression(_)) =>
            case IndexedExpression(_, LiteralExpression(_, _)) =>
            case IndexedExpression(_, GeneratedConstantExpression(_, _)) =>
            case IndexedExpression(_, SumExpression(sumParams, false)) if isUpToOneVar(sumParams) =>
            case _ =>
              if (ctx.options.flag(CompilationFlag.BuggyCodeWarning)) {
                ctx.log.warn("A complex expression may be evaluated multiple times", e.position)
              }
          }
        }
        val conjunction = params.init.zip(params.tail).map {
          case (l, r) => FunctionCallExpression(operator, List(l, r))
        }.reduceLeft((a, b) => FunctionCallExpression("&&", List(a, b)))
        compile(ctx, conjunction, target, branches)
    }
  }

  def areNZFlagsBasedOnB(code: List[MLine]): Boolean = {
    for (line <- code.reverse) {
      line.opcode match {
        case ADDB | ADCB | SUBB | SBCB | ANDB | ORB => return true
        case CLR | ASL | LSR | ASR | ROL | ROR | INC | DEC | NEG => return line.addrMode == InherentB
        case CMPB => return line.addrMode == Immediate && line.parameter.isProvablyZero
        case PULS | PULU =>
          line.addrMode match {
            case r: RegisterSet =>
              if (r.contains(M6809Register.B)) return false
              if (r.contains(M6809Register.CC)) return false
            case _ =>
          }
        case LEAS | LEAU | PSHS | PSHU | TFR | NOP | ABX => // loop
        case _ => return false
      }
    }
    false
  }
}
