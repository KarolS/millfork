package millfork.compiler.z80

import millfork.assembly.z80._
import millfork.compiler._
import millfork.env._
import millfork.node.{ZRegister, _}
import millfork.assembly.z80.ZOpcode._
import millfork.error.ErrorReporting

import scala.collection.GenTraversableOnce

/**
  * @author Karol Stasiak
  */
object ZExpressionTarget extends Enumeration {
  val A, HL, NOTHING = Value
}

object Z80ExpressionCompiler extends AbstractExpressionCompiler[ZLine] {

  def compileToA(ctx: CompilationContext, expression: Expression): List[ZLine] = compile(ctx, expression, ZExpressionTarget.A)

  def compileToHL(ctx: CompilationContext, expression: Expression): List[ZLine] = compile(ctx, expression, ZExpressionTarget.HL)

  def changesBC(line: ZLine): Boolean = {
    import ZRegister._
    if (ZOpcodeClasses.ChangesBCAlways(line.opcode)) return true
    if (ZOpcodeClasses.ChangesFirstRegister(line.opcode)) return line.registers match {
      case TwoRegisters(B | C | BC, _) => true
      case _ => false
    }
    if (ZOpcodeClasses.ChangesOnlyRegister(line.opcode)) return line.registers match {
      case OneRegister(B | C | BC) => true
      case _ => false
    }
    false
  }

  def changesDE(line: ZLine): Boolean = {
    import ZRegister._
    if (ZOpcodeClasses.ChangesDEAlways(line.opcode)) return true
    if (ZOpcodeClasses.ChangesFirstRegister(line.opcode)) return line.registers match {
      case TwoRegisters(D | E | DE, _) => true
      case _ => false
    }
    if (ZOpcodeClasses.ChangesOnlyRegister(line.opcode)) return line.registers match {
      case OneRegister(D | E | DE) => true
      case _ => false
    }
    false
  }

  def changesHL(line: ZLine): Boolean = {
    import ZRegister._
    if (ZOpcodeClasses.ChangesHLAlways(line.opcode)) return true
    if (ZOpcodeClasses.ChangesFirstRegister(line.opcode)) return line.registers match {
      case TwoRegisters(H | L | HL, _) => true
      case _ => false
    }
    if (ZOpcodeClasses.ChangesOnlyRegister(line.opcode)) return line.registers match {
      case OneRegister(H | L | HL) => true
      case _ => false
    }
    false
  }

  def changesA(line: ZLine): Boolean = {
    import ZRegister._
    if (ZOpcodeClasses.ChangesAAlways(line.opcode)) return true
    if (ZOpcodeClasses.ChangesFirstRegister(line.opcode)) return line.registers match {
      case TwoRegisters(A | AF, _) => true
      case _ => false
    }
    if (ZOpcodeClasses.ChangesOnlyRegister(line.opcode)) return line.registers match {
      case OneRegister(A | AF) => true
      case _ => false
    }
    false
  }


  def stashBCIfChanged(lines: List[ZLine]): List[ZLine] = if (lines.exists(changesBC))
    ZLine.register(PUSH, ZRegister.BC) :: (lines :+ ZLine.register(POP, ZRegister.BC)) else lines

  def stashDEIfChanged(lines: List[ZLine]): List[ZLine] = if (lines.exists(changesDE))
    ZLine.register(PUSH, ZRegister.DE) :: (lines :+ ZLine.register(POP, ZRegister.DE)) else lines

  def stashHLIfChanged(lines: List[ZLine]): List[ZLine] = if (lines.exists(changesHL))
    ZLine.register(PUSH, ZRegister.HL) :: (lines :+ ZLine.register(POP, ZRegister.HL)) else lines

  def targetifyA(target: ZExpressionTarget.Value, lines: List[ZLine]): List[ZLine] = target match {
    case ZExpressionTarget.NOTHING | ZExpressionTarget.A => lines
    case ZExpressionTarget.HL => lines ++ List(
      ZLine.ld8(ZRegister.L, ZRegister.A),
      ZLine.ldImm8(ZRegister.H, 0)
    )
  }

  def targetifyHL(target: ZExpressionTarget.Value, lines: List[ZLine]): List[ZLine] = target match {
    case ZExpressionTarget.NOTHING | ZExpressionTarget.HL => lines
    case ZExpressionTarget.A => lines :+ ZLine.ld8(ZRegister.A, ZRegister.L)
  }

  def compile(ctx: CompilationContext, expression: Expression, target: ZExpressionTarget.Value, branches: BranchSpec = BranchSpec.None): List[ZLine] = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    val w = env.get[Type]("word")
    env.eval(expression) match {
      case Some(const) =>
        target match {
          case ZExpressionTarget.A =>
            List(ZLine.ldImm8(ZRegister.A, const))
          case ZExpressionTarget.HL =>
            List(ZLine.ldImm16(ZRegister.HL, const))
          case ZExpressionTarget.NOTHING =>
            Nil // TODO
        }
      case None =>
        expression match {
          case LiteralExpression(value, _) => ???
          case VariableExpression(name) =>
            env.get[Variable](name) match {
              case v: VariableInMemory =>
                v.typ.size match {
                  case 0 => ???
                  case 1 => loadByte(v.toAddress, target)
                  case 2 => target match {
                    case ZExpressionTarget.NOTHING => Nil
                    case ZExpressionTarget.HL =>
                      List(ZLine.ldAbs16(ZRegister.HL, v))
                  }
                  case _ => ???
                }
            }
          case i: IndexedExpression =>
            calculateAddressToHL(ctx, i) match {
              case List(ZLine(LD_16, TwoRegisters(ZRegister.HL, ZRegister.IMM_16), addr, _)) => loadByte(addr, target)
              case code => code ++ loadByteViaHL(target)
            }
          case SumExpression(params, decimal) =>
            getParamMaxSize(ctx, params.map(_._2)) match {
              case 1 => targetifyA(target, ZBuiltIns.compile8BitSum(ctx, params, decimal))
              case 2 => targetifyHL(target, ZBuiltIns.compile16BitSum(ctx, params, decimal))
            }
          case f@FunctionCallExpression(name, params) =>
            name match {
              case "not" =>
                assertBool(ctx, "not", params, 1)
                compile(ctx, params.head, target, branches.flip)
              case "hi" | "lo" => ??? // TODO
              case "nonet" => ??? // TODO
              case "&&" =>
                assertBool(ctx, "&&", params)
                branches match {
                  case BranchIfFalse(_) =>
                    params.flatMap(compile(ctx, _, target, branches))
                  case _ =>
                    val skip = Z80Compiler.nextLabel("an")
                    params.init.flatMap(compile(ctx, _, target, BranchIfFalse(skip))) ++
                      compile(ctx, params.last, target, branches) ++
                      List(ZLine.label(skip))
                }
              case "||" =>
                assertBool(ctx, "||", params)
                branches match {
                  case BranchIfTrue(_) =>
                    params.flatMap(compile(ctx, _, target, branches))
                  case _ =>
                    val skip = Z80Compiler.nextLabel("or")
                    params.init.flatMap(compile(ctx, _, target, BranchIfTrue(skip))) ++
                      compile(ctx, params.last, target, branches) ++
                      List(ZLine.label(skip))
                }
              case "^^" => ???

              case "&" =>
                getParamMaxSize(ctx, params) match {
                  case 1 => targetifyA(target, ZBuiltIns.compile8BitOperation(ctx, AND, params))
                  case 2 => targetifyHL(target, ZBuiltIns.compile16BitOperation(ctx, AND, params))
                }
              case "*" => ???
              case "|" =>
                getParamMaxSize(ctx, params) match {
                  case 1 => targetifyA(target, ZBuiltIns.compile8BitOperation(ctx, OR, params))
                  case 2 => targetifyHL(target, ZBuiltIns.compile16BitOperation(ctx, OR, params))
                }
              case "^" =>
                getParamMaxSize(ctx, params) match {
                  case 1 => targetifyA(target, ZBuiltIns.compile8BitOperation(ctx, XOR, params))
                  case 2 => targetifyHL(target, ZBuiltIns.compile16BitOperation(ctx, XOR, params))
                }
              case ">>>>" =>
                val (l, r, 2) = assertBinary(ctx, params)
                ???
              case "<<" =>
                val (l, r, size) = assertBinary(ctx, params)
                size match {
                  case 1 => targetifyA(target, Z80Shifting.compile8BitShift(ctx, l, r, left = true))
                  case 2 => Z80Shifting.compile16BitShift(ctx, l, r, left = true)
                  case _ => ???
                }
              case ">>" =>
                val (l, r, size) = assertBinary(ctx, params)
                size match {
                  case 1 => targetifyA(target, Z80Shifting.compile8BitShift(ctx, l, r, left = false))
                  case 2 => Z80Shifting.compile16BitShift(ctx, l, r, left = false)
                  case _ => ???
                }
              case "<<'" =>
                assertAllBytes("Long shift ops not supported", ctx, params)
                val (l, r, 1) = assertBinary(ctx, params)
                ???
              case ">>'" =>
                assertAllBytes("Long shift ops not supported", ctx, params)
                val (l, r, 1) = assertBinary(ctx, params)
                ???
              case "<" =>
                val (size, signed) = assertComparison(ctx, params)
                compileTransitiveRelation(ctx, "<", params, target, branches) { (l, r) =>
                  size match {
                    case 1 => Z80Comparisons.compile8BitComparison(ctx, if (signed) ComparisonType.LessSigned else ComparisonType.LessUnsigned, l, r, branches)
                    case _ => ???
                  }
                }
              case ">=" =>
                val (size, signed) = assertComparison(ctx, params)
                compileTransitiveRelation(ctx, ">=", params, target, branches) { (l, r) =>
                  size match {
                    case 1 => Z80Comparisons.compile8BitComparison(ctx, if (signed) ComparisonType.GreaterOrEqualSigned else ComparisonType.GreaterOrEqualUnsigned, l, r, branches)
                    case _ => ???
                  }
                }
              case ">" =>
                val (size, signed) = assertComparison(ctx, params)
                compileTransitiveRelation(ctx, ">", params, target, branches) { (l, r) =>
                  size match {
                    case 1 => Z80Comparisons.compile8BitComparison(ctx, if (signed) ComparisonType.GreaterSigned else ComparisonType.GreaterUnsigned, l, r, branches)
                    case _ => ???
                  }
                }
              case "<=" =>
                val (size, signed) = assertComparison(ctx, params)
                compileTransitiveRelation(ctx, "<=", params, target, branches) { (l, r) =>
                  size match {
                    case 1 => Z80Comparisons.compile8BitComparison(ctx, if (signed) ComparisonType.LessOrEqualSigned else ComparisonType.LessOrEqualUnsigned, l, r, branches)
                    case _ => ???
                  }
                }
              case "==" =>
                val size = params.map(p => getExpressionType(ctx, p).size).max
                compileTransitiveRelation(ctx, "==", params, target, branches) { (l, r) =>
                  size match {
                    case 1 => Z80Comparisons.compile8BitComparison(ctx, ComparisonType.Equal, l, r, branches)
                    case _ => ???
                  }
                }
              case "!=" =>
                val (l, r, size) = assertBinary(ctx, params)
                compileTransitiveRelation(ctx, "!=", params, target, branches) { (l, r) =>
                  size match {
                    case 1 => Z80Comparisons.compile8BitComparison(ctx, ComparisonType.NotEqual, l, r, branches)
                    case _ => ???
                  }
                }
              case "+=" =>
                val (l, r, size) = assertAssignmentLike(ctx, params)
                size match {
                  case 1 => ZBuiltIns.perform8BitInPlace(ctx, l, r, ADD)
                  case _ => ???
                }
              case "-=" =>
                val (l, r, size) = assertAssignmentLike(ctx, params)
                size match {
                  case 1 => ZBuiltIns.perform8BitInPlace(ctx, l, r, SUB)
                  case _ => ???
                }
              case "+'=" =>
                val (l, r, size) = assertAssignmentLike(ctx, params)
                size match {
                  case 1 => ZBuiltIns.perform8BitInPlace(ctx, l, r, ADD, decimal = true)
                  case _ => ???
                }
              case "-'=" =>
                val (l, r, size) = assertAssignmentLike(ctx, params)
                size match {
                  case 1 => ZBuiltIns.perform8BitInPlace(ctx, l, r, SUB, decimal = true)
                  case _ => ???
                }
                Nil
              case "<<=" =>
                val (l, r, size) = assertAssignmentLike(ctx, params)
                size match {
                  case 1 => Z80Shifting.compile8BitShiftInPlace(ctx, l, r, left = true)
                  case 2 => Z80Shifting.compile16BitShift(ctx, l, r, left = true) ++ storeHL(ctx, l, signedSource = false)
                  case _ => ???
                }
              case ">>=" =>
                val (l, r, size) = assertAssignmentLike(ctx, params)
                size match {
                  case 1 => Z80Shifting.compile8BitShiftInPlace(ctx, l, r, left = false)
                  case 2 => Z80Shifting.compile16BitShift(ctx, l, r, left = false) ++ storeHL(ctx, l, signedSource = false)
                  case _ => ???
                }
              case "<<'=" =>
                val (l, r, size) = assertAssignmentLike(ctx, params)
                ???
              case ">>'=" =>
                val (l, r, size) = assertAssignmentLike(ctx, params)
                ???
              case "*=" =>
                assertAllBytes("Long multiplication not supported", ctx, params)
                val (l, r, 1) = assertAssignmentLike(ctx, params)
                ???
              case "*'=" =>
                assertAllBytes("Long multiplication not supported", ctx, params)
                val (l, r, 1) = assertAssignmentLike(ctx, params)
                ???
              case "&=" =>
                val (l, r, size) = assertAssignmentLike(ctx, params)
                size match {
                  case 1 => ZBuiltIns.perform8BitInPlace(ctx, l, r, AND)
                  case _ => ???
                }
              case "^=" =>
                val (l, r, size) = assertAssignmentLike(ctx, params)
                size match {
                  case 1 => ZBuiltIns.perform8BitInPlace(ctx, l, r, XOR)
                  case _ => ???
                }
              case "|=" =>
                val (l, r, size) = assertAssignmentLike(ctx, params)
                size match {
                  case 1 => ZBuiltIns.perform8BitInPlace(ctx, l, r, OR)
                  case _ => ???
                }
              case _ =>
                env.maybeGet[Type](f.functionName) match {
                  case Some(typ) =>
                    var failed = false
                    if (typ.name == "pointer") {
                      ErrorReporting.error("Cannot cast into pointer")
                      failed = true
                    }
                    if (params.length != 1) {
                      ErrorReporting.error("Type casting should have exactly one argument")
                      failed = true
                    }
                    val sourceType = getExpressionType(ctx, params.head)
                    if (typ.size != sourceType.size) {
                      ErrorReporting.error("Cannot cast a type to a type of different size")
                      failed = true
                    }
                    return sourceType.size match {
                      case 1 => targetifyA(target, compileToA(ctx, params.head))
                      case 2 => targetifyHL(target, compileToHL(ctx, params.head))
                      case _ => ???
                    }
                  case None =>
                  // fallthrough to the lookup below
                }
                lookupFunction(ctx, f) match {
                  case function: MacroFunction =>
                    val (paramPreparation, statements) = Z80MacroExpander.inlineFunction(ctx, function, params, expression.position)
                    paramPreparation ++ statements.map {
                      case _ => ???
                    }
                  case function: EmptyFunction =>
                    ??? // TODO: type conversion?
                  case function: FunctionInMemory =>
                    function match {
                      case nf: NormalFunction =>
                        if (nf.interrupt) {
                          ErrorReporting.error(s"Calling an interrupt function `${f.functionName}`", expression.position)
                        }
                      case _ => ()
                    }
                    val result = function.params match {
                      case AssemblyParamSignature(paramConvs) =>
                        ???
                      case NormalParamSignature(paramVars) =>
                        params.zip(paramVars).flatMap {
                          case (paramExpr, paramVar) =>
                            val callCtx = callingContext(ctx, paramVar)
                            paramVar.typ.size match {
                              case 1 =>
                                compileToA(ctx, paramExpr) ++ storeA(callCtx, VariableExpression(paramVar.name), paramVar.typ.isSigned)
                              case 2 =>
                                compileToHL(ctx, paramExpr) ++ storeHL(callCtx, VariableExpression(paramVar.name), paramVar.typ.isSigned)
                              case _ => ???
                            }
                        } ++ List(ZLine(CALL, NoRegisters, function.toAddress))
                    }
                    result
                }
            }

        }
    }
  }

  def calculateAddressToAppropriatePointer(ctx: CompilationContext, expr: LhsExpression): Option[(LocalVariableAddressOperand, List[ZLine])] = {
    val env = ctx.env
    expr match {
      case VariableExpression(name) =>
        env.get[Variable](name) match {
          case v:VariableInMemory => Some(LocalVariableAddressViaHL -> List(ZLine.ldImm16(ZRegister.HL, v.toAddress)))
          case v:StackVariable => Some(LocalVariableAddressViaIX(v.baseOffset) -> Nil)
        }
      case i:IndexedExpression => Some(LocalVariableAddressViaHL -> calculateAddressToHL(ctx, i))
      case _:SeparateBytesExpression => None
      case _ => ???
    }
  }

  def calculateAddressToHL(ctx: CompilationContext, i: IndexedExpression): List[ZLine] = {
    val env = ctx.env
    env.getPointy(i.name) match {
      case ConstantPointy(baseAddr, _) =>
        env.evalVariableAndConstantSubParts(i.index) match {
          case (None, offset) => List(ZLine.ldImm16(ZRegister.HL, (baseAddr + offset).quickSimplify))
          case (Some(index), offset) =>
            List(ZLine.ldImm16(ZRegister.BC, (baseAddr + offset).quickSimplify)) ++
              stashBCIfChanged(compileToHL(ctx, index)) ++
              List(ZLine.registers(ADD_16, ZRegister.HL, ZRegister.BC))
        }
      case VariablePointy(varAddr) =>
        compileToHL(ctx, i.index) ++
          loadBCFromHL ++
          List(
            ZLine.ldAbs16(ZRegister.HL, varAddr),
            ZLine.registers(ADD_16, ZRegister.HL, ZRegister.BC))
    }
  }

  private val loadBCFromHL = List(
    ZLine.ld8(ZRegister.B, ZRegister.H),
    ZLine.ld8(ZRegister.C, ZRegister.L)
  )

  def loadByte(sourceAddr: Constant, target: ZExpressionTarget.Value): List[ZLine] = {
    target match {
      case ZExpressionTarget.NOTHING => Nil
      case ZExpressionTarget.A => List(ZLine.ldAbs8(ZRegister.A, sourceAddr))
      case ZExpressionTarget.HL => List(ZLine.ldAbs8(ZRegister.A, sourceAddr), ZLine.ld8(ZRegister.L, ZRegister.A), ZLine.ldImm8(ZRegister.H, 0))
    }
  }

  def loadByteViaHL(target: ZExpressionTarget.Value): List[ZLine] = {
    target match {
      case ZExpressionTarget.NOTHING => Nil
      case ZExpressionTarget.A => List(ZLine.ld8(ZRegister.A, ZRegister.MEM_HL))
      case ZExpressionTarget.HL => List(ZLine.ld8(ZRegister.A, ZRegister.MEM_HL), ZLine.ld8(ZRegister.L, ZRegister.A), ZLine.ldImm8(ZRegister.H, 0))
    }
  }

  def signExtend(targetAddr: Constant, hiRegister: ZRegister.Value, bytes: Int, signedSource: Boolean): List[ZLine] = {
    if (bytes == 0) return Nil
    val prepareA = if (signedSource) {
      val prefix = if (hiRegister == ZRegister.A) Nil else List(ZLine.ld8(ZRegister.A, hiRegister))
      val label = Z80Compiler.nextLabel("sx")
      prefix ++ List(
        ZLine.imm8(OR, 0x7f),
        ZLine.jump(label, IfFlagSet(ZFlag.S)),
        ZLine.ldImm8(ZRegister.A, 0),
        ZLine.label(label))
    } else {
      List(ZLine.ldImm8(ZRegister.A, 0))
    }
    val fillUpperBytes = List.tabulate(bytes)(i => ZLine.ldAbs8((targetAddr + i).quickSimplify, ZRegister.A))
    prepareA ++ fillUpperBytes
  }

  def storeA(targetAddr: Constant, targetSize: Int, signedSource: Boolean): List[ZLine] = {
    targetSize match {
      case 0 => Nil
      case 1 => List(ZLine.ldAbs8(targetAddr, ZRegister.A))
      case n => ZLine.ldAbs8(targetAddr, ZRegister.A) :: signExtend(targetAddr + 1, ZRegister.A, n - 1, signedSource)
    }
  }

  def storeHL(targetAddr: Constant, targetSize: Int, signedSource: Boolean): List[ZLine] = {
    // TODO: LD (nnnn),HL compatibility?
    targetSize match {
      case 0 => Nil
      case 1 => List(ZLine.ld8(ZRegister.A, ZRegister.L), ZLine.ldAbs8(targetAddr, ZRegister.A))
      case 2 => List(ZLine.ldAbs16(targetAddr, ZRegister.HL))
      case n => ZLine.ldAbs16(targetAddr, ZRegister.HL) :: signExtend(targetAddr + 2, ZRegister.H, n - 2, signedSource)
    }
  }

  def storeA(ctx: CompilationContext, target: LhsExpression, signedSource: Boolean): List[ZLine] = {
    val env = ctx.env
    target match {
      case VariableExpression(vname) =>
        env.get[Variable](vname) match {
          case v: VariableInMemory => storeA(v.toAddress, v.typ.size, signedSource)
        }
      case i:IndexedExpression =>
        calculateAddressToHL(ctx, i) match {
          case List(ZLine(LD_16, TwoRegisters(ZRegister.HL, ZRegister.IMM_16), addr, _)) => storeA(addr, 1, signedSource)
          case code => if (code.exists(changesA)) {
            List(ZLine.ld8(ZRegister.E, ZRegister.A)) ++ stashDEIfChanged(code) :+ ZLine.ld8(ZRegister.MEM_HL, ZRegister.E)
          } else code :+ ZLine.ld8(ZRegister.MEM_HL, ZRegister.A)
        }
      //TODO
      case SeparateBytesExpression(hi, lo) => ???
    }
  }

  def storeHL(ctx: CompilationContext, target: LhsExpression, signedSource: Boolean): List[ZLine] = {
    val env = ctx.env
    target match {
      case VariableExpression(vname) =>
        env.get[Variable](vname) match {
          case v: VariableInMemory => storeHL(v.toAddress, v.typ.size, signedSource)
        }
      case IndexedExpression(pointyName, indexExpr) =>
        env.getPointy(pointyName) match {
          case p: ConstantPointy =>
            env.evalVariableAndConstantSubParts(indexExpr) match {
              case (None, offset) => ZLine.ld8(ZRegister.A, ZRegister.L) :: storeA((p.value + offset).quickSimplify, 1, signedSource)
            }
        }
      //TODO
      case SeparateBytesExpression(hi, lo) => ???
    }
  }

  private def compileTransitiveRelation(ctx: CompilationContext,
                                        operator: String,
                                        params: List[Expression],
                                        target: ZExpressionTarget.Value,
                                        branches: BranchSpec)(binary: (Expression, Expression) => List[ZLine]): List[ZLine] = {
    params match {
      case List(l, r) => binary(l, r)
      case List(_) | Nil =>
        ErrorReporting.fatal("")
      case _ =>
        params.tail.init.foreach { e =>
          if (ctx.env.eval(e).isEmpty) e match {
            case VariableExpression(_) =>
            case LiteralExpression(_, _) =>
            case IndexedExpression(_, VariableExpression(_)) =>
            case IndexedExpression(_, LiteralExpression(_, _)) =>
            case IndexedExpression(_, SumExpression(List(
            (_, LiteralExpression(_, _)),
            (false, VariableExpression(_))
            ), false)) =>
            case IndexedExpression(_, SumExpression(List(
            (false, VariableExpression(_)),
            (_, LiteralExpression(_, _))
            ), false)) =>
            case _ =>
              ErrorReporting.warn("A complex expression may be evaluated multiple times", ctx.options, e.position)
          }
        }
        val conjunction = params.init.zip(params.tail).map {
          case (l, r) => FunctionCallExpression(operator, List(l, r))
        }.reduceLeft((a, b) => FunctionCallExpression("&&", List(a, b)))
        compile(ctx, conjunction, target, branches)
    }
  }
}
