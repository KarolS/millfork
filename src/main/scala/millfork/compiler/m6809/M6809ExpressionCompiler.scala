package millfork.compiler.m6809

import millfork.assembly.m6809.{DAccumulatorIndexed, Indexed, MLine, MOpcode, TwoRegisters}
import millfork.compiler.{AbstractExpressionCompiler, BranchIfFalse, BranchIfTrue, BranchSpec, ComparisonType, CompilationContext, NoBranching}
import millfork.node.{DerefExpression, Expression, FunctionCallExpression, GeneratedConstantExpression, IndexedExpression, LhsExpression, LiteralExpression, M6809Register, SumExpression, VariableExpression}
import millfork.assembly.m6809.MOpcode._
import millfork.env.{Constant, ConstantBooleanType, ConstantPointy, FatBooleanType, MathOperator, MemoryVariable, NormalFunction, NormalParamSignature, NumericConstant, StackVariablePointy, Variable, VariablePointy}

import scala.collection.GenTraversableOnce

/**
  * @author Karol Stasiak
  */
object MExpressionTarget extends Enumeration {
  val A, B, D, X, Y, NOTHING = Value

  def toLd(r: Value): MOpcode.Value = r match {
    case A => LDA
    case B => LDB
    case D => LDD
    case X => LDX
    case Y => LDY
    case _ => ???
  }

  def toLea(r: Value): MOpcode.Value = r match {
    case X => LEAX
    case Y => LEAY
    case _ => ???
  }

  def size(r: Value): Int = r match {
    case A | B => 1
    case D | X | Y => 2
    case NOTHING => 0
  }

}

import MExpressionTarget.toLd

object M6809ExpressionCompiler extends AbstractExpressionCompiler[MLine] {

  def compile(ctx: CompilationContext, expr: Expression, target: MExpressionTarget.Value, branches: BranchSpec = BranchSpec.None): List[MLine] = {
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
              case 1 => List(MLine.variable(toLd(target), variable))
              case 2 => List(MLine.variable(LDB, variable)) ++ zeroextendB(ctx, target, exprType.isSigned)
            }
          case 2 =>
            targetSize match {
              case 0 => Nil
              case 2 => List(MLine.variable(toLd(target), variable))
            }
        }
      case LiteralExpression(c, _) =>
        target match {
          case MExpressionTarget.NOTHING => Nil
          case _ => List(MLine.immediate(MExpressionTarget.toLd(target), NumericConstant(c, MExpressionTarget.size(target))))
        }
      case DerefExpression(inner, offset, _) =>
        compileToX(ctx, inner) :+ MLine(toLd(target), Indexed(M6809Register.X, indirect = false), NumericConstant(offset, 2))
      case e@SumExpression(expressions, decimal) =>
        getArithmeticParamMaxSize(ctx, expressions.map(_._2)) match {
          case 1 => M6809Buitins.compileByteSum(ctx, e, fromScratch = true) ++ targetifyB(ctx, target, isSigned = false)
          case 2 => M6809Buitins.compileWordSum(ctx, e, fromScratch = false) ++ targetifyD(ctx, target)
        }
      case fce@FunctionCallExpression(functionName, params) =>
        functionName match {
          case "not" => ???
          case "nonet" => ???
          case "hi" =>
            compileToD(ctx, params.head) ++ targetifyA(ctx, target, isSigned = false)
          case "lo" =>
            compileToD(ctx, params.head) ++ targetifyB(ctx, target, isSigned = false)
          case "call" => ???
          case "*" => ???
          case "*'" => ???
          case "/" => ???
          case "%%" => ???
          case "&" =>
            getArithmeticParamMaxSize(ctx, params) match {
              case 1 => M6809Buitins.compileByteBitwise(ctx, params, fromScratch = true, ANDB, MathOperator.And, 0xff) ++ targetifyB(ctx, target, isSigned = false)
              case 2 => M6809Buitins.compileWordBitwise(ctx, params, fromScratch = true, ANDA, ANDB, MathOperator.And, 0xffff) ++ targetifyB(ctx, target, isSigned = false)
            }
          case "|" => ???
            getArithmeticParamMaxSize(ctx, params) match {
              case 1 => M6809Buitins.compileByteBitwise(ctx, params, fromScratch = true, ORB, MathOperator.Or, 0) ++ targetifyB(ctx, target, isSigned = false)
              case 2 => M6809Buitins.compileWordBitwise(ctx, params, fromScratch = true, ORA, ORB, MathOperator.Or, 0) ++ targetifyB(ctx, target, isSigned = false)
            }
          case "^" => ???
            getArithmeticParamMaxSize(ctx, params) match {
              case 1 => M6809Buitins.compileByteBitwise(ctx, params, fromScratch = true, EORB, MathOperator.Exor, 0) ++ targetifyB(ctx, target, isSigned = false)
              case 2 => M6809Buitins.compileWordBitwise(ctx, params, fromScratch = true, EORA, EORB, MathOperator.Exor, 0) ++ targetifyB(ctx, target, isSigned = false)
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
                case 2 => ???
                case _ => ???
              }
            }
          case "!=" =>
            val size = params.map(p => getExpressionType(ctx, p).size).max
            compileTransitiveRelation(ctx, "!=", params, target, branches) { (l, r) =>
              size match {
                case 1 => M6809Comparisons.compile8BitComparison(ctx, ComparisonType.NotEqual, l, r, branches)
                case 2 => ???
                case _ => ???
              }
            }
          case "<" =>
            val (size, signed) = assertArithmeticComparison(ctx, params)
            compileTransitiveRelation(ctx, "<", params, target, branches) { (l, r) =>
              size match {
                case 1 => M6809Comparisons.compile8BitComparison(ctx, if (signed) ComparisonType.LessSigned else ComparisonType.LessUnsigned, l, r, branches)
                case _ => ???
              }
            }
          case ">" =>
            val (size, signed) = assertArithmeticComparison(ctx, params)
            compileTransitiveRelation(ctx, ">", params, target, branches) { (l, r) =>
              size match {
                case 1 => M6809Comparisons.compile8BitComparison(ctx, if (signed) ComparisonType.GreaterSigned else ComparisonType.GreaterUnsigned, l, r, branches)
                case _ => ???
              }
            }
          case "<=" =>
            val (size, signed) = assertArithmeticComparison(ctx, params)
            compileTransitiveRelation(ctx, "<=", params, target, branches) { (l, r) =>
              size match {
                case 1 => M6809Comparisons.compile8BitComparison(ctx, if (signed) ComparisonType.LessOrEqualSigned else ComparisonType.LessOrEqualUnsigned, l, r, branches)
                case _ => ???
              }
            }
          case ">=" =>
            val (size, signed) = assertArithmeticComparison(ctx, params)
            compileTransitiveRelation(ctx, ">=", params, target, branches) { (l, r) =>
              size match {
                case 1 => M6809Comparisons.compile8BitComparison(ctx, if (signed) ComparisonType.GreaterOrEqualSigned else ComparisonType.GreaterOrEqualUnsigned, l, r, branches)
                case _ => ???
              }
            }
          case "<<" => ???
          case ">>" => ???
          case ">>>>" => ???
          case "<<'" => ???
          case ">>'" => ???
          case "+=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 => M6809Buitins.perform8BitInPlace(ctx, l, r, ADDB)
              case _ => ???
            }
          case "+'=" => ???
          case "-=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 => M6809Buitins.perform8BitInPlace(ctx, l, r, SUBB)
              case _ => ???
            }
          case "-'=" => ???
          case "*=" => ???
          case "*'=" => ???
          case "/=" => ???
          case "%%=" => ???
          case "&=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 => M6809Buitins.perform8BitInPlace(ctx, l, r, ANDB)
              case _ => ???
            }
          case "|=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 => M6809Buitins.perform8BitInPlace(ctx, l, r, ORB)
              case _ => ???
            }
          case "^=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 => M6809Buitins.perform8BitInPlace(ctx, l, r, EORB)
              case _ => ???
            }
          case "<<=" => ???
          case "<<'=" => ???
          case ">>=" => ???
          case ">>'=" => ???
          case ">>>>=" => ???
          case _ =>
            val f = lookupFunction(ctx, fce)
            f.params match {
              case NormalParamSignature(Nil) => //ok
              case _ => ???
            }
            f match {
              case nf:NormalFunction =>
                List(MLine.absolute(JSR, nf.toAddress))
              case _ => ???
            }
        }
      case _ => ???
    }
  }

  def compileToB(ctx: CompilationContext, expr: Expression): List[MLine] = compile(ctx, expr, MExpressionTarget.B)

  def compileToD(ctx: CompilationContext, expr: Expression): List[MLine] = compile(ctx, expr, MExpressionTarget.D)

  def compileToX(ctx: CompilationContext, expr: Expression): List[MLine] = compile(ctx, expr, MExpressionTarget.X)

  def zeroextendB(ctx: CompilationContext, target: MExpressionTarget.Value, isSigned: Boolean): List[MLine] = {
    val extendToA = if (isSigned) MLine.immediate(LDA, 0) else MLine.inherent(SEX)
    target match {
      case MExpressionTarget.D => List(extendToA)
      case MExpressionTarget.X => List(extendToA, MLine.tfr(M6809Register.D, M6809Register.X))
      case MExpressionTarget.Y => List(extendToA, MLine.tfr(M6809Register.D, M6809Register.Y))
      case _ => ???
    }
  }

  def stashBIfNeeded(ctx: CompilationContext, lines: List[MLine]): List[MLine] = {
    // TODO: only push if needed
    MLine.pp(PSHS, M6809Register.B) :: (lines :+ MLine.pp(PULS, M6809Register.B))
  }

  def storeB(ctx: CompilationContext, target: LhsExpression): List[MLine] = {
    target match {
      case VariableExpression(name) =>
        val variable = ctx.env.get[Variable](name)
        List(MLine.variable(STB, variable))
      case DerefExpression(inner, offset, _) =>
        compileToX(ctx, inner) :+ MLine(STB, Indexed(M6809Register.X, indirect = false), NumericConstant(offset, 2))
      case IndexedExpression(name, index) =>
        ctx.env.getPointy(name) match {
          case p: ConstantPointy =>
            compileToX(ctx, index) :+ MLine(STB, Indexed(M6809Register.X, indirect = false), p.value)
          case v: VariablePointy =>
            ctx.env.eval(index) match {
              case Some(ix) => List(MLine.absolute(LDX, v.addr), MLine(STB, Indexed(M6809Register.X, indirect = false), ix * v.elementType.size))
              case _ =>
                v.indexType.size match {
                  case 1 =>
                    stashBIfNeeded(ctx,
                      compileToD(ctx, index) :+ MLine(LEAX, DAccumulatorIndexed(M6809Register.X, indirect = false), Constant.Zero)) :+
                      MLine(STB, Indexed(M6809Register.X, indirect = false), Constant.Zero)
                }
            }
          case v: StackVariablePointy =>
            ctx.env.eval(index) match {
              case Some(ix) => List(MLine.userstack(LDX, v.offset), MLine(STB, Indexed(M6809Register.X, indirect = false), ix * v.elementType.size))
            }
        }
    }
  }

  def storeD(ctx: CompilationContext, target: LhsExpression): List[MLine] = {
    target match {
      case VariableExpression(name) =>
        val variable = ctx.env.get[Variable](name)
        List(MLine.variable(STD, variable))
      case DerefExpression(inner, offset, _) =>
        compileToX(ctx, inner) :+ MLine(STD, Indexed(M6809Register.X, indirect = false), NumericConstant(offset, 2))
    }
  }

  def targetifyB(ctx: CompilationContext, target: MExpressionTarget.Value, isSigned: Boolean): List[MLine] = target match {
    case MExpressionTarget.NOTHING => Nil
    case MExpressionTarget.B => Nil
    case MExpressionTarget.A => List(MLine.tfr(M6809Register.B, M6809Register.A))
    case MExpressionTarget.D | MExpressionTarget.X | MExpressionTarget.Y => zeroextendB(ctx, target, isSigned)
  }

  def targetifyA(ctx: CompilationContext, target: MExpressionTarget.Value, isSigned: Boolean): List[MLine] = target match {
    case MExpressionTarget.NOTHING => Nil
    case MExpressionTarget.A => Nil
    case MExpressionTarget.B => List(MLine.tfr(M6809Register.A, M6809Register.B))
    case MExpressionTarget.D | MExpressionTarget.X | MExpressionTarget.Y =>
      List(MLine.tfr(M6809Register.A, M6809Register.B)) ++ zeroextendB(ctx, target, isSigned)
  }

  def targetifyD(ctx: CompilationContext, target: MExpressionTarget.Value): List[MLine] = target match {
    case MExpressionTarget.NOTHING => Nil
    case MExpressionTarget.D => Nil
    case MExpressionTarget.X => List(MLine.tfr(M6809Register.D, M6809Register.X))
    case MExpressionTarget.Y => List(MLine.tfr(M6809Register.D, M6809Register.Y))
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
              ctx.log.warn("A complex expression may be evaluated multiple times", e.position)
          }
        }
        val conjunction = params.init.zip(params.tail).map {
          case (l, r) => FunctionCallExpression(operator, List(l, r))
        }.reduceLeft((a, b) => FunctionCallExpression("&&", List(a, b)))
        compile(ctx, conjunction, target, branches)
    }
  }
}
