package millfork.compiler.mos

import millfork.{CompilationFlag, env}
import millfork.assembly.Elidability
import millfork.assembly.mos.AddrMode._
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos._
import millfork.assembly.z80.ZLine
import millfork.compiler._
import millfork.env._
import millfork.error.ConsoleLogger
import millfork.node.{MosRegister, _}
import millfork.output.NoAlignment
/**
  * @author Karol Stasiak
  */
object MosExpressionCompiler extends AbstractExpressionCompiler[AssemblyLine] {

  def compileConstant(ctx: CompilationContext, expr: Constant, exprType: Type, target: Variable): List[AssemblyLine] = {
    target match {
      case RegisterVariable(MosRegister.A, _) => List(AssemblyLine(LDA, Immediate, expr))
      case RegisterVariable(MosRegister.AW, _) =>
        List(
          AssemblyLine.accu16,
          AssemblyLine(LDA_W, WordImmediate, expr),
          AssemblyLine.accu8)
      case RegisterVariable(MosRegister.X, _) => List(AssemblyLine(LDX, Immediate, expr))
      case RegisterVariable(MosRegister.Y, _) => List(AssemblyLine(LDY, Immediate, expr))
      case RegisterVariable(MosRegister.AX, _) => List(
        AssemblyLine(LDA, Immediate, expr.loByte),
        AssemblyLine(LDX, Immediate, expr.hiByte))
      case RegisterVariable(MosRegister.AY, _) => List(
        AssemblyLine(LDA, Immediate, expr.loByte),
        AssemblyLine(LDY, Immediate, expr.hiByte))
      case RegisterVariable(MosRegister.XA, _) => List(
        AssemblyLine(LDA, Immediate, expr.hiByte),
        AssemblyLine(LDX, Immediate, expr.loByte))
      case RegisterVariable(MosRegister.YA, _) => List(
        AssemblyLine(LDA, Immediate, expr.hiByte),
        AssemblyLine(LDY, Immediate, expr.loByte))
      case RegisterVariable(MosRegister.XY, _) => List(
        AssemblyLine(LDY, Immediate, expr.hiByte),
        AssemblyLine(LDX, Immediate, expr.loByte))
      case RegisterVariable(MosRegister.YX, _) => List(
        AssemblyLine(LDX, Immediate, expr.hiByte),
        AssemblyLine(LDY, Immediate, expr.loByte))
      case m: VariableInMemory =>
        val elidability = if (m.isVolatile) Elidability.Volatile else Elidability.Elidable
        val addrMode = if (m.zeropage) ZeroPage else Absolute
        val addr = m.toAddress
        m.typ.size match {
          case 0 => Nil
          case 1 => List(
            AssemblyLine(LDA, Immediate, expr.loByte),
            AssemblyLine(STA, addrMode, addr, elidability = elidability))
          case 2 => if (ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)) {
            AssemblyLine.accu16 :: AssemblyLine(LDA_W, WordImmediate, expr) ::(AssemblyLine.variable(ctx, STA_W, m) :+ AssemblyLine.accu8)
          } else List(
            AssemblyLine(LDA, Immediate, expr.loByte),
            AssemblyLine(STA, addrMode, addr, elidability = elidability),
            AssemblyLine(LDA, Immediate, expr.hiByte),
            AssemblyLine(STA, addrMode, addr + 1, elidability = elidability))
          case s => List.tabulate(s)(i => List(
            AssemblyLine(LDA, Immediate, expr.subbyte(i)),
            AssemblyLine(STA, addrMode, addr + i, elidability = elidability))).flatten
        }
      case m@StackVariable(_, t, offset) =>
        t.size match {
          case 0 => Nil
          case 1 => AssemblyLine.tsx(ctx) ++ List(
            AssemblyLine.immediate(LDA, expr.loByte),
            AssemblyLine.dataStackX(ctx, STA, offset))
          case 2 => if (ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)) {
            AssemblyLine.accu16 :: AssemblyLine(LDA_W, WordImmediate, expr) :: (AssemblyLine.variable(ctx, STA_W, m) :+ AssemblyLine.accu8)
          } else AssemblyLine.tsx(ctx) ++ List(
            AssemblyLine.implied(TSX),
            AssemblyLine.immediate(LDA, expr.loByte),
            AssemblyLine.dataStackX(ctx, STA, offset),
            AssemblyLine.immediate(LDA, expr.hiByte),
            AssemblyLine.absoluteX(STA, offset + 1))
          case s => AssemblyLine.tsx(ctx) ++ List.tabulate(s)(i => List(
            AssemblyLine.immediate(LDA, expr.subbyte(i)),
            AssemblyLine.dataStackX(ctx, STA, offset + i))).flatten
        }
    }
  }

  def fixTsx(code: List[AssemblyLine]): List[AssemblyLine] = code match {
    case (access@AssemblyLine0(_, Stack | IndexedSY, p)) :: xs => access.copy(parameter =  (p + 1).quickSimplify) :: fixTsx(xs)
    case (tsx@AssemblyLine0(TSX, _, _)) :: xs => tsx :: AssemblyLine.implied(INX) :: fixTsx(xs)
    case (txs@AssemblyLine0(TXS, _, _)) :: xs => ???
    case x :: xs => x :: fixTsx(xs)
    case Nil => Nil
  }

  def preserveZpregIfNeededDestroyingAAndX(ctx: CompilationContext, Offset: Int, code: List[AssemblyLine]): List[AssemblyLine] = {
    if (changesZpreg(code, Offset)) {
      List(AssemblyLine.zeropage(LDA, ctx.env.get[VariableInMemory]("__reg"), Offset), AssemblyLine.implied(PHA)) ++
      code ++
        List(
          AssemblyLine.implied(TAX),
          AssemblyLine.implied(PLA),
          AssemblyLine.zeropage(STA, ctx.env.get[VariableInMemory]("__reg"), Offset),
          AssemblyLine.implied(TXA))
    } else code
  }

  def changesZpreg(code: List[AssemblyLine], Offset: Int): Boolean = {
    code.exists {
      case AssemblyLine0(op,
      AddrMode.ZeroPage | AddrMode.Absolute | AddrMode.LongAbsolute,
      CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(Offset, _))) if th.name == "__reg" && OpcodeClasses.ChangesMemoryAlways(op) || OpcodeClasses.ChangesMemoryIfNotImplied(op) => true
      case AssemblyLine0(op,
      AddrMode.ZeroPage | AddrMode.Absolute | AddrMode.LongAbsolute,
      MemoryAddressConstant(th)) if th.name == "__reg" && Offset == 0 && OpcodeClasses.ChangesMemoryAlways(op) || OpcodeClasses.ChangesMemoryIfNotImplied(op) => true
      case AssemblyLine0(JSR | BYTE | BSR, _, _) => true
      case _ => false
    }
  }

  def preserveCarryIfNeeded(ctx: CompilationContext, code: List[AssemblyLine]): List[AssemblyLine] = {
    if (code.exists {
      case AssemblyLine0(JSR | BSR, Absolute | LongAbsolute, MemoryAddressConstant(th)) => true
      case x => OpcodeClasses.ChangesC(x.opcode)
    }) {
      AssemblyLine.implied(PHP) +: fixTsx(code) :+ AssemblyLine.implied(PLP)
    } else code
  }

  def preserveRegisterIfNeeded(ctx: CompilationContext, register: MosRegister.Value, code: List[AssemblyLine]): List[AssemblyLine] = {
    val states = register match {
      case MosRegister.A => Seq(State.A)
      case MosRegister.AX | MosRegister.XA => Seq(State.A, State.X)
      case MosRegister.X => Seq(State.X)
      case MosRegister.AY | MosRegister.YA => Seq(State.A, State.Y)
      case MosRegister.Y => Seq(State.Y)
    }

    val cmos = ctx.options.flag(CompilationFlag.EmitCmosOpcodes)
    if (states.exists(state => AssemblyLine.treatment(code, state) != Treatment.Unchanged)) {
      register match {
        case MosRegister.A => AssemblyLine.implied(PHA) +: fixTsx(code) :+ AssemblyLine.implied(PLA)
        case MosRegister.X | MosRegister.AX | MosRegister.XA => if (cmos) {
          List(
            AssemblyLine.implied(PHA),
            AssemblyLine.implied(PHX),
          ) ++ fixTsx(fixTsx(code)) ++ List(
            AssemblyLine.implied(PLX),
            AssemblyLine.implied(PLA),
          )
        } else {
          List(
            AssemblyLine.implied(PHA),
            AssemblyLine.implied(TXA),
            AssemblyLine.implied(PHA),
          ) ++ fixTsx(fixTsx(code)) ++ List(
            AssemblyLine.implied(PLA),
            AssemblyLine.implied(TAX),
            AssemblyLine.implied(PLA),
          )
        }
        case MosRegister.Y | MosRegister.AY | MosRegister.YA => if (cmos) {
          List(
            AssemblyLine.implied(PHA),
            AssemblyLine.implied(PHY),
          ) ++ fixTsx(fixTsx(code)) ++ List(
            AssemblyLine.implied(PLY),
            AssemblyLine.implied(PLA),
          )
        } else {
          List(
            AssemblyLine.implied(PHA),
            AssemblyLine.implied(TYA),
            AssemblyLine.implied(PHA),
          ) ++ fixTsx(fixTsx(code)) ++ List(
            AssemblyLine.implied(PLA),
            AssemblyLine.implied(TAY),
            AssemblyLine.implied(PLA),
          )
        }
      }
    } else {
      code
    }
  }

  def prepareWordIndexing(ctx: CompilationContext, pointy: Pointy, indexExpression: Expression): List[AssemblyLine] = {
    val w = ctx.env.get[Type]("word")
    if (ctx.options.zpRegisterSize < 2) {
      ctx.log.error("16-bit indexing requires a zeropage pseudoregister")
      return Nil
    }
    val reg = ctx.env.get[VariableInMemory]("__reg")
    val compileIndex = compile(ctx, indexExpression, Some(MosExpressionCompiler.getExpressionType(ctx, indexExpression) -> RegisterVariable(MosRegister.YA, w)), BranchSpec.None)
    val prepareRegister = pointy match {
      case p:ConstantPointy =>
        List(
          AssemblyLine.implied(CLC),
          AssemblyLine.immediate(ADC, p.value.hiByte),
          AssemblyLine.zeropage(STA, reg, 1),
          AssemblyLine.immediate(LDA, p.value.loByte),
          AssemblyLine.zeropage(STA, reg))
      case VariablePointy(addr, _, _, true) =>
        List(
          AssemblyLine.implied(CLC),
          AssemblyLine.zeropage(ADC, addr + 1),
          AssemblyLine.zeropage(STA, reg, 1),
          AssemblyLine.zeropage(LDA, addr),
          AssemblyLine.zeropage(STA, reg))
      case VariablePointy(addr, _, _, false) =>
        List(
          AssemblyLine.implied(CLC),
          AssemblyLine.absolute(ADC, addr + 1),
          AssemblyLine.zeropage(STA, reg, 1),
          AssemblyLine.absolute(LDA, addr),
          AssemblyLine.zeropage(STA, reg))
      case StackVariablePointy(offset, _, _) =>
        if (ctx.options.flag(CompilationFlag.EmitEmulation65816Opcodes)) {
          List(
            AssemblyLine.implied(CLC),
            AssemblyLine.stackRelative(ADC, offset + 1 + ctx.extraStackOffset),
            AssemblyLine.zeropage(STA, reg, 1),
            AssemblyLine.stackRelative(LDA, offset + ctx.extraStackOffset),
            AssemblyLine.zeropage(STA, reg))
        } else {
          List(
            AssemblyLine.implied(CLC),
            AssemblyLine.implied(TSX),
            AssemblyLine.absoluteX(ADC, offset + 1 + ctx.extraStackOffset),
            AssemblyLine.zeropage(STA, reg, 1),
            AssemblyLine.absoluteX(LDA, offset + ctx.extraStackOffset),
            AssemblyLine.zeropage(STA, reg))
        }
    }
    compileIndex ++ prepareRegister
  }

  def compileByteStorage(ctx: CompilationContext, register: MosRegister.Value, target: LhsExpression): List[AssemblyLine] = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    val store = register match {
      case MosRegister.A => STA
      case MosRegister.X => STX
      case MosRegister.Y => STY
    }
    val transferToA = register match {
      case MosRegister.A => NOP
      case MosRegister.X => TXA
      case MosRegister.Y => TYA
    }
    target match {
      case VariableExpression(name) =>
        val v = env.get[Variable](name)
        v.typ.size match {
          case 0 => ???
          case 1 =>
            v match {
              case mv: VariableInMemory => AssemblyLine.variable(ctx, store, mv)
              case sv: StackVariable =>
                AssemblyLine.implied(transferToA) :: (AssemblyLine.tsx(ctx) :+ AssemblyLine.dataStackX(ctx, STA, sv))
            }
          case s if s > 1 =>
            v match {
              case mv: VariableInMemory =>
                AssemblyLine.variable(ctx, store, mv) ++
                  List(AssemblyLine.immediate(LDA, 0)) ++
                  List.tabulate(s - 1)(i => AssemblyLine.variable(ctx, STA, mv, i + 1)).flatten
              case sv: StackVariable =>
                AssemblyLine.implied(transferToA) :: (
                  AssemblyLine.tsx(ctx) ++
                    List(AssemblyLine.dataStackX(ctx, STA, sv), AssemblyLine.immediate(LDA, 0)) ++
                    List.tabulate(s - 1)(i => AssemblyLine.dataStackX(ctx, STA, sv, i + 1))
                  )

            }
        }
      case IndexedExpression(arrayName, indexExpr) =>
        val pointy = env.getPointy(arrayName)
        val (variableIndex, constIndex) = env.evalVariableAndConstantSubParts(indexExpr)
        val variableIndexSize = variableIndex.map(v => getExpressionType(ctx, v).size).getOrElse(0)
        val totalIndexSize = getExpressionType(ctx, indexExpr).size

        def storeToArrayAtUnknownIndex(variableIndex: Expression, arrayAddr: Constant) = {
          // TODO check typ
          val indexRegister = if (register == MosRegister.Y) MosRegister.X else MosRegister.Y
          val calculatingIndex = preserveRegisterIfNeeded(ctx, register, compile(ctx, variableIndex, Some(b, RegisterVariable(indexRegister, b)), NoBranching))
          if (register == MosRegister.A) {
            indexRegister match {
              case MosRegister.Y =>
                calculatingIndex ++ arrayBoundsCheck(ctx, pointy, MosRegister.Y, indexExpr) ++ List(AssemblyLine.absoluteY(STA, arrayAddr + constIndex))
              case MosRegister.X =>
                calculatingIndex ++ arrayBoundsCheck(ctx, pointy, MosRegister.X, indexExpr) ++ List(AssemblyLine.absoluteX(STA, arrayAddr + constIndex))
            }
          } else {
            indexRegister match {
              case MosRegister.Y =>
                calculatingIndex ++ arrayBoundsCheck(ctx, pointy, MosRegister.Y, indexExpr) ++ List(AssemblyLine.implied(transferToA), AssemblyLine.absoluteY(STA, arrayAddr + constIndex))
              case MosRegister.X =>
                calculatingIndex ++ arrayBoundsCheck(ctx, pointy, MosRegister.X, indexExpr) ++ List(AssemblyLine.implied(transferToA), AssemblyLine.absoluteX(STA, arrayAddr + constIndex))
            }
          }
        }
        def wrapWordIndexingStorage(code: List[AssemblyLine]) = {
          val reg = ctx.env.get[VariableInMemory]("__reg")
          val cmos = ctx.options.flag(CompilationFlag.EmitCmosOpcodes)
          register match {
            case MosRegister.A =>
              List(AssemblyLine.implied(PHA)) ++ fixTsx(code) ++ List(AssemblyLine.implied(PLA), AssemblyLine.indexedY(STA, reg))
            case MosRegister.X =>
              if (code.exists(l => OpcodeClasses.ChangesX(l.opcode))) {
                if (cmos)
                  List(AssemblyLine.implied(PHX)) ++ fixTsx(code) ++ List(AssemblyLine.implied(PLA), AssemblyLine.indexedY(STA, reg))
                else
                  List(AssemblyLine.implied(TXA), AssemblyLine.implied(PHA)) ++ fixTsx(code) ++ List(AssemblyLine.implied(PLA), AssemblyLine.indexedY(STA, reg))
              } else {
                code ++ List(AssemblyLine.implied(TXA), AssemblyLine.indexedY(STA, reg))
              }
            case MosRegister.Y =>
              if (cmos)
                List(AssemblyLine.implied(PHY)) ++ fixTsx(code) ++ List(AssemblyLine.implied(PLA), AssemblyLine.indexedY(STA, reg))
              else
                List(AssemblyLine.implied(TYA), AssemblyLine.implied(PHA)) ++ fixTsx(code) ++ List(AssemblyLine.implied(PLA), AssemblyLine.indexedY(STA, reg))
          }
        }

        (pointy, variableIndex, variableIndexSize, totalIndexSize) match {
          case (p: ConstantPointy, None, _, _) =>
            if (p.readOnly) {
              ctx.log.error("Writing to a constant array", target.position)
            }
            List(AssemblyLine.absolute(store, env.genRelativeVariable(p.value + constIndex, b, zeropage = false)))
          case (p: VariablePointy, _, _, 2) =>
            wrapWordIndexingStorage(prepareWordIndexing(ctx, p, indexExpr))
          case (p: VariablePointy, _, 0 | 1, _) if !p.zeropage =>
            // TODO: optimize?
            wrapWordIndexingStorage(prepareWordIndexing(ctx, p, indexExpr))
          case (p: ConstantPointy, Some(v), 2, _) =>
            if (p.readOnly) {
              ctx.log.error("Writing to a constant array", target.position)
            }
            val w = env.get[VariableType]("word")
            wrapWordIndexingStorage(prepareWordIndexing(ctx, ConstantPointy(p.value + constIndex, None, if (constIndex.isProvablyZero) p.sizeInBytes else None, if (constIndex.isProvablyZero) p.elementCount else None, w, p.elementType, NoAlignment, p.readOnly), v))
          case (p: ConstantPointy, Some(v), 1, _) =>
            if (p.readOnly) {
              ctx.log.error("Writing to a constant array", target.position)
            }
            storeToArrayAtUnknownIndex(v, p.value)
          //TODO: should there be a type check or a zeropage check?
          case (pointerVariable@VariablePointy(varAddr, _, _, true), None, _, 0 | 1) =>
            register match {
              case MosRegister.A =>
                List(AssemblyLine.immediate(LDY, constIndex), AssemblyLine.indexedY(STA, pointerVariable.addr))
              case MosRegister.Y =>
                List(AssemblyLine.implied(TYA), AssemblyLine.immediate(LDY, constIndex), AssemblyLine.indexedY(STA, pointerVariable.addr), AssemblyLine.implied(TAY))
              case MosRegister.X =>
                List(AssemblyLine.immediate(LDY, constIndex), AssemblyLine.implied(TXA), AssemblyLine.indexedY(STA, pointerVariable.addr))
              case _ =>
                ctx.log.error("Cannot store a word in an array", target.position)
                Nil
            }
          case (p@VariablePointy(varAddr, _, _, true), Some(_), _, 0 | 1) =>
            val calculatingIndex = compile(ctx, indexExpr, Some(b, RegisterVariable(MosRegister.Y, b)), NoBranching)
            register match {
              case MosRegister.A =>
                preserveRegisterIfNeeded(ctx, MosRegister.A, calculatingIndex) :+ AssemblyLine.indexedY(STA, varAddr)
              case MosRegister.X =>
                preserveRegisterIfNeeded(ctx, MosRegister.X, calculatingIndex) ++ List(AssemblyLine.implied(TXA), AssemblyLine.indexedY(STA, varAddr))
              case MosRegister.Y =>
                AssemblyLine.implied(TYA) :: preserveRegisterIfNeeded(ctx, MosRegister.A, calculatingIndex) ++ List(
                  AssemblyLine.indexedY(STA, varAddr), AssemblyLine.implied(TAY)
                )
              case _ =>
                ctx.log.error("Cannot store a word in an array", target.position)
                Nil
            }
          case (StackVariablePointy(offset, _, _), None, _, 0 | 1) if ctx.options.flag(CompilationFlag.EmitEmulation65816Opcodes) =>
            register match {
              case MosRegister.A =>
                List(AssemblyLine.immediate(LDY, constIndex), AssemblyLine.indexedSY(STA, offset))
              case MosRegister.Y =>
                List(AssemblyLine.implied(TYA), AssemblyLine.immediate(LDY, constIndex), AssemblyLine.indexedSY(STA, offset), AssemblyLine.implied(TAY))
              case MosRegister.X =>
                List(AssemblyLine.immediate(LDY, constIndex), AssemblyLine.implied(TXA), AssemblyLine.indexedSY(STA, offset))
              case _ =>
                ctx.log.error("Cannot store a word in an array", target.position)
                Nil
            }
          case (p@StackVariablePointy(offset, _, _), Some(_), _, 0 | 1) if ctx.options.flag(CompilationFlag.EmitEmulation65816Opcodes) =>
            val calculatingIndex = compile(ctx, indexExpr, Some(b, RegisterVariable(MosRegister.Y, b)), NoBranching)
            register match {
              case MosRegister.A =>
                preserveRegisterIfNeeded(ctx, MosRegister.A, calculatingIndex) :+ AssemblyLine.indexedSY(STA, offset)
              case MosRegister.X =>
                preserveRegisterIfNeeded(ctx, MosRegister.X, calculatingIndex) ++ List(AssemblyLine.implied(TXA), AssemblyLine.indexedSY(STA, offset))
              case MosRegister.Y =>
                AssemblyLine.implied(TYA) :: preserveRegisterIfNeeded(ctx, MosRegister.A, calculatingIndex) ++ List(
                  AssemblyLine.indexedSY(STA, offset), AssemblyLine.implied(TAY)
                )
              case _ =>
                ctx.log.error("Cannot store a word in an array", target.position)
                Nil
            }
          case (p: StackVariablePointy, _, _, _) =>
            // TODO: optimize?
            wrapWordIndexingStorage(prepareWordIndexing(ctx, p, indexExpr))
          case _ =>
            ctx.log.error("Invalid index for writing", indexExpr.position)
            Nil
        }
      case DerefExpression(inner, offset, targetType) =>
        val (prepare, addr, am) = getPhysicalPointerForDeref(ctx, inner)
        val lo = preserveRegisterIfNeeded(ctx, MosRegister.A, prepare) ++ List(AssemblyLine.immediate(LDY, offset), AssemblyLine(STA, am, addr))
        if (targetType.size == 1) {
          lo
        } else {
          lo ++ List(AssemblyLine.immediate(LDA, 0)) ++
                List.tabulate(targetType.size - 1)(i => List(AssemblyLine.implied(INY), AssemblyLine(STA, am, addr))).flatten
        }
    }
  }
  val noop: List[AssemblyLine] = Nil

  def areNZFlagsBasedOnA(code: List[AssemblyLine]): Boolean = {
    for (line <- code.reverse) {
      line.opcode match {
        case TXA | LDA | ADC | EOR | SBC | AND | ORA | TYA | TZA | PLA | LAX => return true
        case CMP => return line.addrMode == Immediate && line.parameter.isProvablyZero
        case STA | STX | STY | STZ | SAX |
             STA_W | STX_W | STY_W | STZ_W |
             CLD | SED | CLV | SEI | CLI | SEC | CLC |
             NOP => ()
        case ASL | LSR | ROL | ROR | INC | DEC => return line.addrMode == Implied
        case _ => return false
      }
    }
    false
  }

  def compileToFatBooleanInA(ctx: CompilationContext, expr: Expression): List[AssemblyLine] = {
    val env = ctx.env
    val sourceType = AbstractExpressionCompiler.getExpressionType(ctx, expr)
    sourceType match {
      case FatBooleanType =>
        compileToA(ctx, expr)
      case t: ConstantBooleanType =>
        List(AssemblyLine.immediate(LDA, if (t.value) 1 else 0))
      case _: FlagBooleanType | BuiltInBooleanType =>
        val label = env.nextLabel("bo")
        val condition = compile(ctx, expr, None, BranchIfFalse(label))
        if (condition.isEmpty) {
          ???
        }
        val conditionWithoutJump = condition.init
        val hasOnlyOneJump = !conditionWithoutJump.exists(_.refersTo(label))
        // TODO: helper functions to convert flags to booleans, to make code smaller
        if (hasOnlyOneJump) {
          condition.last.opcode match {
            case BCC =>
              // our bool is in the carry flag
              // 3 bytes 4 cycles
              return conditionWithoutJump ++ List(AssemblyLine.immediate(LDA, 0), AssemblyLine.implied(ROL))
            case BCS if !ctx.options.flag(CompilationFlag.OptimizeForSpeed) =>
              // our bool is in the carry flag, negated
              // 5 bytes 6 cycles
              return conditionWithoutJump ++ List(AssemblyLine.immediate(LDA, 0), AssemblyLine.implied(ROL), AssemblyLine.immediate(EOR, 1))
            case BPL if areNZFlagsBasedOnA(conditionWithoutJump) =>
              // our bool is in the N flag and the 7th bit of A
              // 4 bytes 6 cycles
              return conditionWithoutJump ++ List(AssemblyLine.implied(ASL), AssemblyLine.immediate(LDA, 0), AssemblyLine.implied(ROL))
            case BMI if areNZFlagsBasedOnA(conditionWithoutJump) && ctx.options.flag(CompilationFlag.OptimizeForSize)=>
              // our bool is in the N flag and the 7th bit of A, negated
              // 6 bytes 8 cycles
              return conditionWithoutJump ++ List(AssemblyLine.implied(ASL), AssemblyLine.immediate(LDA, 0), AssemblyLine.implied(ROL), AssemblyLine.immediate(EOR, 1))
            case _ =>
          }
        }
        if (hasOnlyOneJump) {
          condition.last.opcode match {
            case BCC | BCS | BVC | BVS =>
              // 7 bytes; for true: 5 cycles, for false 6 cycles
              return conditionWithoutJump ++ List(
                AssemblyLine.immediate(LDA, 0),
                condition.last,
                AssemblyLine.immediate(LDA, 1),
                AssemblyLine.label(label))
            case _ => ()
          }
        }
        val skip = env.nextLabel("bo")
        // at most 9 bytes; for true: 7 cycles, for false 5 cycles
        condition ++ List(
          AssemblyLine.immediate(LDA, 1),
          AssemblyLine.absolute(JMP, Label(skip)),
          AssemblyLine.label(label),
          AssemblyLine.immediate(LDA, 0),
          AssemblyLine.label(skip))
      case _ =>
        ctx.log.fatal(s"Cannot assign `${sourceType.name}` to `bool`", expr.position)
    }
  }

  def compileToA(ctx: CompilationContext, expr: Expression): List[AssemblyLine] = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    compile(ctx, expr, Some(b -> RegisterVariable(MosRegister.A, b)), BranchSpec.None)
  }

  def compileToAX(ctx: CompilationContext, expr: Expression): List[AssemblyLine] = {
    val env = ctx.env
    val w = env.get[Type]("word")
    compile(ctx, expr, Some(w -> RegisterVariable(MosRegister.AX, w)), BranchSpec.None)
  }

  def compileToZReg(ctx: CompilationContext, expr: Expression): List[AssemblyLine] = {
    val env = ctx.env
    val p = env.get[Type]("pointer")
    compile(ctx, expr, Some(p -> env.get[Variable]("__reg.loword")), BranchSpec.None)
  }

  def compileToZReg2(ctx: CompilationContext, expr: Expression): List[AssemblyLine] = {
    val env = ctx.env
    val p = env.get[Type]("pointer")
    compile(ctx, expr, Some(p -> env.get[Variable]("__reg.b2b3")), BranchSpec.None)
  }

  def getPhysicalPointerForDeref(ctx: CompilationContext, pointerExpression: Expression): (List[AssemblyLine], Constant, AddrMode.Value) = {
    pointerExpression match {
      case VariableExpression(name) =>
        val p = ctx.env.get[ThingInMemory](name)
        if (p.isInstanceOf[MfArray]) return (Nil, p.toAddress, AddrMode.AbsoluteY)
        if (p.zeropage) return (Nil, p.toAddress, AddrMode.IndexedY)
      case _ =>
    }
    ctx.env.eval(pointerExpression) match {
      case Some(addr) => (Nil, addr, AddrMode.AbsoluteY)
      case _ => (compileToZReg(ctx, pointerExpression), ctx.env.get[ThingInMemory]("__reg.loword").toAddress, AddrMode.IndexedY)
    }
  }

  def compileStackOffset(ctx: CompilationContext, target: Variable, offset: Int, subbyte: Option[Int]): List[AssemblyLine] = {
    val hi = if (ctx.options.flag(CompilationFlag.SoftwareStack)) {
      MemoryAddressConstant(ctx.env.get[ThingInMemory]("__stack")).hiByte
    } else {
      Constant.One
    }
    val tsx = if (ctx.options.flag(CompilationFlag.SoftwareStack)) {
      AssemblyLine.absolute(LDX, MemoryAddressConstant(ctx.env.get[ThingInMemory]("__sp")))
    } else {
      AssemblyLine.implied(TSX)
    }
    val actualOffset = if (ctx.options.flag(CompilationFlag.SoftwareStack)) {
      offset & 0xff
    } else {
      (offset + ctx.extraStackOffset) & 0xff
    }

    val b = ctx.env.get[Type]("byte")
    subbyte match {
      case Some(1) => compile(ctx, GeneratedConstantExpression(hi, b), Some(b -> target), BranchSpec.None)
      case Some(0) => target match {
        case RegisterVariable(MosRegister.A, _) => actualOffset match {
          case 0 => List(tsx, AssemblyLine.implied(TXA))
          case 1 => List(tsx, AssemblyLine.implied(INX), AssemblyLine.implied(TXA))
          case 2 => List(tsx, AssemblyLine.implied(INX), AssemblyLine.implied(INX), AssemblyLine.implied(TXA))
          case _ => List(tsx, AssemblyLine.implied(TXA), AssemblyLine.implied(CLC), AssemblyLine.immediate(ADC, actualOffset))
        }

        case RegisterVariable(MosRegister.X, _) => actualOffset match {
          case 0 => List(tsx)
          case 1 => List(tsx, AssemblyLine.implied(INX))
          case 2 => List(tsx, AssemblyLine.implied(INX), AssemblyLine.implied(INX))
          case 3 => List(tsx, AssemblyLine.implied(INX), AssemblyLine.implied(INX), AssemblyLine.implied(INX))
          case _ => List(tsx, AssemblyLine.implied(TXA), AssemblyLine.implied(CLC), AssemblyLine.immediate(ADC, actualOffset), AssemblyLine.implied(TAX))
        }
        case RegisterVariable(MosRegister.Y, _) => actualOffset match {
          case 0 => List(tsx)
          case 1 => List(tsx, AssemblyLine.implied(TXA), AssemblyLine.implied(TAY), AssemblyLine.implied(INY))
          case _ => List(tsx, AssemblyLine.implied(TXA), AssemblyLine.implied(CLC), AssemblyLine.immediate(ADC, actualOffset), AssemblyLine.implied(TAY))
        }
        case _ =>
          val loadA = actualOffset match {
            case 0 => List(tsx, AssemblyLine.implied(TXA))
            case 1 => List(tsx, AssemblyLine.implied(TXA), AssemblyLine.implied(INX))
            case 2 => List(tsx, AssemblyLine.implied(TXA), AssemblyLine.implied(INX), AssemblyLine.implied(INX))
            case _ => List(tsx, AssemblyLine.implied(TXA), AssemblyLine.implied(CLC), AssemblyLine.immediate(ADC, actualOffset))
          }
          loadA ++ expressionStorageFromA(ctx, Some(b -> target), None, signedSource = false)
      }
      case None =>
        val loadA = actualOffset match {
          case 0 => List(tsx, AssemblyLine.implied(TXA))
          case 1 => List(tsx, AssemblyLine.implied(INX), AssemblyLine.implied(TXA))
          case 2 => List(tsx, AssemblyLine.implied(INX), AssemblyLine.implied(INX), AssemblyLine.implied(TXA))
          case _ => List(tsx, AssemblyLine.implied(TXA), AssemblyLine.implied(CLC), AssemblyLine.immediate(ADC, actualOffset))
        }
        loadA ++ List(AssemblyLine.immediate(LDX, hi)) ++ expressionStorageFromAX(ctx, Some(ctx.env.get[Type]("pointer") -> target), None)
      case _ => throw new IllegalArgumentException
    }
  }

  def compile(ctx: CompilationContext, expr: Expression, exprTypeAndVariable: Option[(Type, Variable)], branches: BranchSpec): List[AssemblyLine] = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    val exprType = AbstractExpressionCompiler.getExpressionType(ctx, expr)
    if (branches != NoBranching) {
      (exprType, branches) match {
        case (FatBooleanType, _) =>
          return compile(ctx, FunctionCallExpression("!=", List(expr, LiteralExpression(0, 1))), exprTypeAndVariable, branches)
        case (ConstantBooleanType(_, false), BranchIfTrue(_)) | (ConstantBooleanType(_, true), BranchIfFalse(_))=>
          return compile(ctx, expr, exprTypeAndVariable, NoBranching)
        case (ConstantBooleanType(_, true), BranchIfTrue(x)) =>
          return compile(ctx, expr, exprTypeAndVariable, NoBranching) :+ AssemblyLine.absolute(JMP, Label(x))
        case (ConstantBooleanType(_, false), BranchIfFalse(x)) =>
          return compile(ctx, expr, exprTypeAndVariable, NoBranching) :+ AssemblyLine.absolute(JMP, Label(x))
        case _ => ()
      }
    }
    env.eval(expr) match {
      case Some(value) =>
        return exprTypeAndVariable.fold(noop) { case (exprType, target) =>
          assertCompatible(exprType, target.typ)
          compileConstant(ctx, value, exprType, target)
        }
      case _ =>
    }
    val w = env.get[Type]("word")
    expr match {
      case HalfWordExpression(expression, _) => ??? // TODO
      case LiteralExpression(value, size) =>
        exprTypeAndVariable.fold(noop) { case (exprType, target) =>
          assertCompatible(exprType, target.typ)
          compileConstant(ctx, NumericConstant(value, size), exprType, target)
        }
      case GeneratedConstantExpression(value, _) =>
        exprTypeAndVariable.fold(noop) { case (exprType, target) =>
          assertCompatible(exprType, target.typ)
          compileConstant(ctx, value, exprType, target)
        }
      case VariableExpression(name) =>
        exprTypeAndVariable.fold(noop) { case (exprType, target) =>
          assertCompatible(exprType, target.typ)
          env.eval(expr).map(c => compileConstant(ctx, c, exprType, target)).getOrElse {
            env.get[TypedThing](name) match {
              case source: StackOffsetThing => compileStackOffset(ctx, target, source.offset, source.subbyte)
              case source: VariableInMemory =>
                target match {
                  case RegisterVariable(MosRegister.A, _) => AssemblyLine.variable(ctx, LDA, source)
                  case RegisterVariable(MosRegister.AW, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        AssemblyLine.variable(ctx, LDA, source) ++ List(
                          AssemblyLine.implied(PHA)) ++ signExtendA(ctx) ++ List(
                          AssemblyLine.implied(XBA),
                          AssemblyLine.implied(PLA))
                      } else List(AssemblyLine.immediate(LDX, 0), AssemblyLine.implied(XBA)) ++ AssemblyLine.variable(ctx, LDA, source) :+ AssemblyLine.immediate(LDX, 0)
                      case 2 =>
                        if (ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)) {
                          AssemblyLine.accu16 :: (AssemblyLine.variable(ctx, LDA_W, source) :+ AssemblyLine.accu8)
                        } else {
                          AssemblyLine.variable(ctx, LDA, source, 1) ++ List(AssemblyLine.implied(XBA)) ++ AssemblyLine.variable(ctx, LDA, source)
                        }
                    }
                  case RegisterVariable(MosRegister.X, _) => AssemblyLine.variable(ctx, LDX, source)
                  case RegisterVariable(MosRegister.Y, _) => AssemblyLine.variable(ctx, LDY, source)
                  case RegisterVariable(MosRegister.AX, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        AssemblyLine.variable(ctx, LDA, source) ++ List(
                          AssemblyLine.implied(PHA)) ++ signExtendA(ctx) ++ List(
                          AssemblyLine.implied(TAX),
                          AssemblyLine.implied(PLA))
                      } else AssemblyLine.variable(ctx, LDA, source) :+ AssemblyLine.immediate(LDX, 0)
                      case 2 =>
                        AssemblyLine.variable(ctx, LDA, source) ++ AssemblyLine.variable(ctx, LDX, source, 1)
                    }
                  case RegisterVariable(MosRegister.AY, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        AssemblyLine.variable(ctx, LDA, source) ++ List(
                          AssemblyLine.implied(PHA)) ++ signExtendA(ctx) ++ List(
                          AssemblyLine.implied(TAY),
                          AssemblyLine.implied(PLA))
                      } else {
                        AssemblyLine.variable(ctx, LDA, source) :+ AssemblyLine.immediate(LDY, 0)
                      }
                      case 2 =>
                        AssemblyLine.variable(ctx, LDA, source) ++ AssemblyLine.variable(ctx, LDY, source, 1)
                    }
                  case RegisterVariable(MosRegister.XA, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        AssemblyLine.variable(ctx, LDX, source) ++ List(AssemblyLine.implied(TXA)) ++ signExtendA(ctx)
                      } else
                        AssemblyLine.variable(ctx, LDX, source) :+ AssemblyLine.immediate(LDA, 0)
                      case 2 =>
                        AssemblyLine.variable(ctx, LDX, source) ++ AssemblyLine.variable(ctx,LDA, source, 1)
                    }
                  case RegisterVariable(MosRegister.XY, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        AssemblyLine.variable(ctx, LDX, source) ++ List(AssemblyLine.implied(TXA)) ++ signExtendA(ctx) ++ List(AssemblyLine.implied(TAY))
                      } else
                        AssemblyLine.variable(ctx, LDX, source) :+ AssemblyLine.immediate(LDY, 0)
                      case 2 =>
                        AssemblyLine.variable(ctx, LDX, source) ++ AssemblyLine.variable(ctx, LDY, source, 1)
                    }
                  case RegisterVariable(MosRegister.YA, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        AssemblyLine.variable(ctx, LDY, source) ++ List(AssemblyLine.implied(TYA)) ++ signExtendA(ctx)
                      } else
                        AssemblyLine.variable(ctx, LDY, source) :+ AssemblyLine.immediate(LDA, 0)
                      case 2 =>
                        AssemblyLine.variable(ctx, LDY, source) ++ AssemblyLine.variable(ctx, LDA, source, 1)
                    }
                  case RegisterVariable(MosRegister.YX, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        AssemblyLine.variable(ctx, LDY, source) ++ List(AssemblyLine.implied(TYA)) ++ signExtendA(ctx) ++ List(AssemblyLine.implied(TAX))
                      } else
                        AssemblyLine.variable(ctx, LDY, source) :+ AssemblyLine.immediate(LDX, 0)
                      case 2 =>
                        AssemblyLine.variable(ctx, LDY, source) ++ AssemblyLine.variable(ctx, LDX, source, 1)
                    }
                  case target: VariableInMemory =>
                    if (exprType.size > target.typ.size) {
                      ctx.log.error(s"Variable `$target.name` is too small", expr.position)
                      Nil
                    } else if (exprType.size == 2 && target.typ.size == 2 && ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)) {
                      AssemblyLine.accu16 :: (AssemblyLine.variable(ctx, LDA_W, source) ++ AssemblyLine.variable(ctx, STA_W, target) :+ AssemblyLine.accu8)
                    } else {
                      val copyFromLo  = List.tabulate(exprType.size)(i => AssemblyLine.variable(ctx, LDA, source, i) ++ AssemblyLine.variable(ctx, STA, target, i))
                      val copy = if (shouldCopyFromHiToLo(source.toAddress, target.toAddress)) copyFromLo.reverse else copyFromLo
                      val extend = if (exprType.size == target.typ.size) Nil else if (exprType.isSigned) {
                        signExtendA(ctx) ++ List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.variable(ctx, STA, target, i + exprType.size)).flatten
                      } else {
                        AssemblyLine.immediate(LDA, 0) ::
                          List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.variable(ctx, STA, target, i + exprType.size)).flatten
                      }
                      copy.flatten ++ extend
                    }
                  case target: StackVariable =>
                    if (exprType.size > target.typ.size) {
                      ctx.log.error(s"Variable `$target.name` is too small", expr.position)
                      Nil
                    } else if (exprType.size == 2 && target.typ.size == 2 && ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)) {
                      AssemblyLine.accu16 :: (AssemblyLine.variable(ctx, LDA_W, source) ++ AssemblyLine.variable(ctx, STA_W, target) :+ AssemblyLine.accu8)
                    } else {
                      val copy = List.tabulate(exprType.size)(i => AssemblyLine.variable(ctx, LDA, source, i) :+ AssemblyLine.dataStackX(ctx, STA, target, i))
                      val extend = if (exprType.size == target.typ.size) Nil else if (exprType.isSigned) {
                        signExtendA(ctx) ++ List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.dataStackX(ctx, STA, target, i + exprType.size))
                      } else {
                        AssemblyLine.immediate(LDA, 0) ::
                          List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.dataStackX(ctx, STA, target, i + exprType.size))
                      }
                      AssemblyLine.tsx(ctx) ++ copy.flatten ++ extend
                    }
                }
              case source@StackVariable(_, sourceType, offset) =>
                target match {
                  case RegisterVariable(MosRegister.A, _) => AssemblyLine.tsx(ctx) :+ AssemblyLine.dataStackX(ctx, LDA, offset)
                  case RegisterVariable(MosRegister.X, _) => AssemblyLine.tsx(ctx) ++ List(AssemblyLine.dataStackX(ctx, LDA, offset), AssemblyLine.implied(TAX))
                  case RegisterVariable(MosRegister.Y, _) => AssemblyLine.tsx(ctx) :+ AssemblyLine.dataStackX(ctx, LDY, offset)
                  case RegisterVariable(MosRegister.AX, _) =>
                    AssemblyLine.tsx(ctx) ++ (exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        List(
                          AssemblyLine.dataStackX(ctx, LDA, offset),
                          AssemblyLine.implied(PHA)) ++ signExtendA(ctx) ++ List(
                          AssemblyLine.implied(TAX),
                          AssemblyLine.implied(PLA))
                      } else List(
                        AssemblyLine.implied(TSX),
                        AssemblyLine.dataStackX(ctx, LDA, offset),
                        AssemblyLine.immediate(LDX, 0))
                      case 2 => List(
                        AssemblyLine.implied(TSX),
                        AssemblyLine.dataStackX(ctx, LDA, offset),
                        AssemblyLine.implied(PHA),
                        AssemblyLine.dataStackX(ctx, LDA, offset + 1),
                        AssemblyLine.implied(TAX),
                        AssemblyLine.implied(PLA))
                    })
                  case RegisterVariable(MosRegister.AY, _) =>
                    AssemblyLine.tsx(ctx) ++ (exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        List(
                          AssemblyLine.dataStackX(ctx, LDA, offset),
                          AssemblyLine.implied(PHA)) ++
                          signExtendA(ctx) ++
                          List(AssemblyLine.implied(PLA))
                      } else {
                        List(
                          AssemblyLine.dataStackX(ctx, LDA, offset),
                          AssemblyLine.immediate(LDY, 0))
                      }
                      case 2 => List(
                        AssemblyLine.dataStackX(ctx, LDA, offset),
                        AssemblyLine.dataStackX(ctx, LDY, offset + 1))
                    })
                  case RegisterVariable(MosRegister.XA, _) =>
                    AssemblyLine.tsx(ctx) ++ (exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        List(
                          AssemblyLine.dataStackX(ctx, LDA, offset),
                          AssemblyLine.implied(TAX)) ++
                          signExtendA(ctx)
                      } else {
                        List(
                          AssemblyLine.dataStackX(ctx, LDA, offset),
                          AssemblyLine.implied(TAX),
                          AssemblyLine.immediate(LDA, 0))
                      }
                      case 2 => List(
                        AssemblyLine.dataStackX(ctx, LDA, offset),
                        AssemblyLine.dataStackX(ctx, LDY, offset + 1),
                        AssemblyLine.implied(TAX),
                        AssemblyLine.implied(TYA))
                    })
                  case RegisterVariable(MosRegister.YA, _) =>
                    AssemblyLine.tsx(ctx) ++ (exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        List(
                          AssemblyLine.dataStackX(ctx, LDA, offset),
                          AssemblyLine.implied(TAY)) ++ signExtendA(ctx)
                      } else {
                        List(
                          AssemblyLine.dataStackX(ctx, LDY, offset),
                          AssemblyLine.immediate(LDA, 0))
                      }
                      case 2 => List(
                        AssemblyLine.dataStackX(ctx, LDY, offset),
                        AssemblyLine.dataStackX(ctx, LDA, offset + 1))
                    })
                  case RegisterVariable(MosRegister.YX, _) =>
                    AssemblyLine.tsx(ctx) ++ (exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        List(
                          AssemblyLine.dataStackX(ctx, LDA, offset),
                          AssemblyLine.implied(TAY)) ++
                          signExtendA(ctx) ++
                          List(AssemblyLine.implied(TAX))
                      } else {
                        List(
                          AssemblyLine.dataStackX(ctx, LDY, offset),
                          AssemblyLine.immediate(LDX, 0))
                      }
                      case 2 => List(
                        AssemblyLine.dataStackX(ctx, LDY, offset),
                        AssemblyLine.dataStackX(ctx, LDA, offset + 1),
                        AssemblyLine.implied(TAX))
                    })
                  case RegisterVariable(MosRegister.XY, _) =>
                    AssemblyLine.tsx(ctx) ++ (exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        List(
                          AssemblyLine.dataStackX(ctx, LDA, offset),
                          AssemblyLine.implied(TAX)) ++
                          signExtendA(ctx) ++
                          List(AssemblyLine.implied(TAY))
                      } else {
                        List(
                          AssemblyLine.dataStackX(ctx, LDA, offset),
                          AssemblyLine.implied(TAX),
                          AssemblyLine.immediate(LDY, 0))
                      }
                      case 2 => List(
                        AssemblyLine.dataStackX(ctx, LDA, offset),
                        AssemblyLine.dataStackX(ctx, LDY, offset + 1),
                        AssemblyLine.implied(TAX))
                    })
                  case target: VariableInMemory =>
                    if (exprType.size > target.typ.size) {
                      ctx.log.error(s"Variable `$target.name` is too small", expr.position)
                      Nil
                    } else if (exprType.size == 2 && target.typ.size == 2 && ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)) {
                      AssemblyLine.accu16 :: (AssemblyLine.variable(ctx, LDA_W, source) ++ AssemblyLine.variable(ctx, STA_W, target) :+ AssemblyLine.accu8)
                    } else {
                      val copy = List.tabulate(exprType.size)(i => AssemblyLine.dataStackX(ctx, LDA, offset + i) :: AssemblyLine.variable(ctx, STA, target, i))
                      val extend = if (exprType.size == target.typ.size) Nil else if (exprType.isSigned) {
                        signExtendA(ctx) ++ List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.variable(ctx, STA, target, i + exprType.size)).flatten
                      } else {
                        AssemblyLine.immediate(LDA, 0) ::
                          List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.variable(ctx, STA, target, i + exprType.size)).flatten
                      }
                      AssemblyLine.tsx(ctx) ++ copy.flatten ++ extend
                    }
                  case target: StackVariable =>
                    if (exprType.size > target.typ.size) {
                      ctx.log.error(s"Variable `$target.name` is too small", expr.position)
                      Nil
                    } else if (exprType.size == 2 && target.typ.size == 2 && ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)) {
                      AssemblyLine.accu16 :: (AssemblyLine.variable(ctx, LDA_W, source) ++ AssemblyLine.variable(ctx, STA_W, target) :+ AssemblyLine.accu8)
                    } else {
                      val copyFromLo = List.tabulate(exprType.size)(i => List(AssemblyLine.dataStackX(ctx, LDA, offset + i), AssemblyLine.dataStackX(ctx, STA, target, i)))
                      val copy = if (shouldCopyFromHiToLo(NumericConstant(source.baseOffset, 2), NumericConstant(target.baseOffset, 2))) copyFromLo.reverse else copyFromLo
                      val extend = if (exprType.size == target.typ.size) Nil else if (exprType.isSigned) {
                        signExtendA(ctx) ++ List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.dataStackX(ctx, STA, target, i + exprType.size))
                      } else {
                        AssemblyLine.immediate(LDA, 0) ::
                          List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.dataStackX(ctx, STA, target, i + exprType.size))
                      }
                      AssemblyLine.tsx(ctx) ++ copy.flatten ++ extend
                    }
                }
              case source@ConstantThing(_, value, _) =>
                compileConstant(ctx, value, exprType, target)
            }
          }
        }
      case IndexedExpression(arrayName, indexExpr) =>
        val pointy = env.getPointy(arrayName)
        AbstractExpressionCompiler.checkIndexType(ctx, pointy, indexExpr)
        if (pointy.elementType.size != 1) ctx.log.fatal("Whee!") // the statement preprocessor should have removed all of those
        // TODO: check
        val (variableIndex, constantIndex) = env.evalVariableAndConstantSubParts(indexExpr)
        val variableIndexSize = variableIndex.map(v => getExpressionType(ctx, v).size).getOrElse(0)
        val totalIndexSize = getExpressionType(ctx, indexExpr).size
        exprTypeAndVariable.fold(compile(ctx, indexExpr, None, BranchSpec.None)) { case (exprType, target) =>

          val register = target match {
            case RegisterVariable(r, _) => r
            case _ => MosRegister.A
          }
          val loRegister = register match {
            case MosRegister.AX => MosRegister.A
            case MosRegister.AY => MosRegister.A
            case MosRegister.AW => MosRegister.A
            case MosRegister.XA => MosRegister.X
            case MosRegister.XY => MosRegister.X
            case MosRegister.YA => MosRegister.Y
            case MosRegister.YX => MosRegister.Y
            case _ => register
          }
          val suffix = target match {
            case RegisterVariable(_, _) => Nil
            case target: VariableInMemory =>
              if (target.typ.size == 1) {
                AssemblyLine.variable(ctx, STA, target)
              }
              else if (target.typ.isSigned) {
                AssemblyLine.variable(ctx, STA, target) ++ signExtendA(ctx) ++
                  List.tabulate(target.typ.size - 1)(i => AssemblyLine.variable(ctx, STA, target, i + 1)).flatten
              } else {
                AssemblyLine.variable(ctx, STA, target) ++
                  List(AssemblyLine.immediate(LDA, 0)) ++
                  List.tabulate(target.typ.size - 1)(i => AssemblyLine.variable(ctx, STA, target, i + 1)).flatten
              }
          }
          val load = loRegister match {
            case MosRegister.A => LDA
            case MosRegister.X => LDX
            case MosRegister.Y => LDY
          }

          def loadFromArrayAtUnknownIndex(variableIndex: Expression, arrayAddr: Constant) = {
            // TODO check typ
            val indexRegister = if (register == MosRegister.Y) MosRegister.X else MosRegister.Y
            val calculatingIndex = compile(ctx, variableIndex, Some(b, RegisterVariable(indexRegister, b)), NoBranching)
            indexRegister match {
              case MosRegister.Y =>
                calculatingIndex ++ arrayBoundsCheck(ctx, pointy, MosRegister.Y, indexExpr) ++ List(AssemblyLine.absoluteY(load, arrayAddr + constantIndex))
              case MosRegister.X =>
                calculatingIndex ++ arrayBoundsCheck(ctx, pointy, MosRegister.X, indexExpr) ++ List(AssemblyLine.absoluteX(load, arrayAddr + constantIndex))
            }
          }

          def loadFromReg() = {
            val reg = ctx.env.get[VariableInMemory]("__reg")
            loRegister match {
              case MosRegister.A =>
                List(AssemblyLine.indexedY(LDA, reg))
              case MosRegister.X =>
                List(AssemblyLine.indexedY(LDA, reg), AssemblyLine.implied(TAX))
              case MosRegister.Y =>
                List(AssemblyLine.indexedY(LDA, reg), AssemblyLine.implied(TAY))
            }
          }
          val result = (pointy, variableIndex, totalIndexSize, variableIndexSize) match {
            case (a: ConstantPointy, None, _, _) =>
              List(AssemblyLine.absolute(load, env.genRelativeVariable(a.value + constantIndex, b, zeropage = false)))
            case (a: ConstantPointy, Some(v), _, 1) =>
              loadFromArrayAtUnknownIndex(v, a.value)
            case (a: ConstantPointy, Some(v), _, 2) =>
              prepareWordIndexing(ctx, ConstantPointy(
                a.value + constantIndex,
                None,
                if (constantIndex.isProvablyZero) a.sizeInBytes else None,
                if (constantIndex.isProvablyZero) a.elementCount else None,
                env.get[VariableType]("word"),
                a.elementType, NoAlignment, a.readOnly), v) ++ loadFromReg()
            case (a: VariablePointy, _, 2, _) =>
              prepareWordIndexing(ctx, a, indexExpr) ++ loadFromReg()
            case (p: VariablePointy, _, 0 | 1, _) if !p.zeropage =>
              prepareWordIndexing(ctx, p, indexExpr) ++ loadFromReg()
            case (p:VariablePointy, None, 0 | 1, _) =>
              loRegister match {
                case MosRegister.A =>
                  List(AssemblyLine.immediate(LDY, constantIndex), AssemblyLine.indexedY(LDA, p.addr))
                case MosRegister.Y =>
                  List(AssemblyLine.immediate(LDY, constantIndex), AssemblyLine.indexedY(LDA, p.addr), AssemblyLine.implied(TAY))
                case MosRegister.X =>
                  List(AssemblyLine.immediate(LDY, constantIndex), AssemblyLine.indexedY(LDA, p.addr), AssemblyLine.implied(TAX))
              }
            case (p:VariablePointy, Some(_), 0 | 1, _) =>
              val calculatingIndex = compile(ctx, indexExpr, Some(b, RegisterVariable(MosRegister.Y, b)), NoBranching)
              loRegister match {
                case MosRegister.A =>
                  calculatingIndex :+ AssemblyLine.indexedY(LDA, p.addr)
                case MosRegister.X =>
                  calculatingIndex ++ List(AssemblyLine.indexedY(LDA, p.addr), AssemblyLine.implied(TAX))
                case MosRegister.Y =>
                  calculatingIndex ++ List(AssemblyLine.indexedY(LDA, p.addr), AssemblyLine.implied(TAY))
              }
            case (p: StackVariablePointy, _, 0 | 1, _) if ctx.options.flag(CompilationFlag.EmitEmulation65816Opcodes) =>
              loRegister match {
                case MosRegister.A =>
                  List(AssemblyLine.immediate(LDY, constantIndex), AssemblyLine.indexedSY(LDA, p.offset))
                case MosRegister.Y =>
                  List(AssemblyLine.immediate(LDY, constantIndex), AssemblyLine.indexedSY(LDA, p.offset), AssemblyLine.implied(TAY))
                case MosRegister.X =>
                  List(AssemblyLine.immediate(LDY, constantIndex), AssemblyLine.indexedSY(LDA, p.offset), AssemblyLine.implied(TAX))
              }
            case (p: StackVariablePointy, _, _, _) =>
              prepareWordIndexing(ctx, p, indexExpr) ++ loadFromReg()
            case _ =>
              ctx.log.error("Invalid index for reading", indexExpr.position)
              Nil
          }
          register match {
            case MosRegister.A | MosRegister.X | MosRegister.Y => result ++ suffix
            case MosRegister.AX | MosRegister.YX => result :+ AssemblyLine.immediate(LDX, 0) // TODO: signedness?
            case MosRegister.AY | MosRegister.XY => result :+ AssemblyLine.immediate(LDY, 0)
            case MosRegister.AW => result ++ List(AssemblyLine.implied(XBA), AssemblyLine.immediate(LDA, 0), AssemblyLine.implied(XBA))
          }
        }
      case DerefExpression(inner, offset, targetType) =>
        val (prepare, addr, am) = getPhysicalPointerForDeref(ctx, inner)
        (targetType.size, am) match {
          case (1, AbsoluteY) =>
            prepare ++ List(AssemblyLine.absolute(LDA, addr + offset)) ++ expressionStorageFromA(ctx, exprTypeAndVariable, expr.position, exprType.isSigned)
          case (1, _) =>
            prepare ++ List(AssemblyLine.immediate(LDY, offset), AssemblyLine(LDA, am, addr)) ++ expressionStorageFromA(ctx, exprTypeAndVariable, expr.position, exprType.isSigned)
          case (2, AbsoluteY) =>
            prepare ++
              List(
                AssemblyLine.absolute(LDA, addr + offset),
                AssemblyLine.absolute(LDX, addr + offset + 1)) ++
              expressionStorageFromAX(ctx, exprTypeAndVariable, expr.position)
          case (2, _) =>
            prepare ++
              List(
                AssemblyLine.immediate(LDY, offset+1),
                AssemblyLine(LDA, am, addr),
                AssemblyLine.implied(TAX),
                AssemblyLine.implied(DEY),
                AssemblyLine(LDA, am, addr)) ++
              expressionStorageFromAX(ctx, exprTypeAndVariable, expr.position)
          case _ =>
            exprTypeAndVariable match {
              case Some((variableType, variable)) =>
                prepare ++ (0 until variableType.size).flatMap { i =>
                  val load =
                    if (i >= targetType.size) List(AssemblyLine.immediate(LDA, 0))
                    else if (am == AbsoluteY) List(AssemblyLine.absolute(LDA, addr + offset + i))
                    else if (i == 0) List(AssemblyLine.immediate(LDY, offset), AssemblyLine(LDA, am, addr))
                    else List(AssemblyLine.implied(INY), AssemblyLine(LDA, am, addr))
                  load ++ AssemblyLine.variable(ctx, STA, variable, i)
                }
              case None => Nil
            }
        }
      case SumExpression(params, decimal) =>
        assertAllArithmetic(ctx, params.map(_._2))
        val a = params.map{case (n, p) => env.eval(p).map(n -> _)}
        if (a.forall(_.isDefined)) {
            val value = a.foldLeft(Constant.Zero){(c, pair) =>
              val Some((neg, v)) = pair
              CompoundConstant(if (decimal) {
                if (neg) MathOperator.DecimalMinus else MathOperator.DecimalPlus
              } else {
                if (neg) MathOperator.Minus else MathOperator.Plus
              }, c, v).quickSimplify
            }
          exprTypeAndVariable.map(x => compileConstant(ctx, value.quickSimplify, exprType, x._2)).getOrElse(Nil)
        } else {
          getSumSize(ctx, params) match {
            case 1 =>
              val calculate = BuiltIns.compileAddition(ctx, params, decimal = decimal)
              val store = expressionStorageFromAX(ctx, exprTypeAndVariable, expr.position)
              if (exprTypeAndVariable.exists(_._1.size >= 2)) {
                calculate ++ List(AssemblyLine.immediate(LDX, 0)) ++ store
              } else {
                calculate ++ store
              }
            case 2 =>
              if (ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)) {
                val calculate = PseudoregisterBuiltIns.compileWordAdditionToAW(ctx, params, decimal = decimal)
                val store = expressionStorageFromAW(ctx, exprTypeAndVariable, expr.position)
                calculate ++ store
              } else {
                PseudoregisterBuiltIns.compileWordAdditionViaAX(ctx, exprTypeAndVariable, expr.position, params, decimal = decimal)
              }
            case _ =>
              ctx.log.error("Non-in-place addition or subtraction of variables larger than 2 bytes is not supported", expr.position)
              Nil
          }
        }
      case SeparateBytesExpression(h, l) =>
        exprTypeAndVariable.fold {
          // TODO: order?
          compile(ctx, l, None, branches) ++ compile(ctx, h, None, branches)
        } { case (exprType, target) =>
          assertCompatible(exprType, target.typ)
          target match {
            // TODO: some more complex ones may not work correctly
            case RegisterVariable(MosRegister.A | MosRegister.X | MosRegister.Y, _) => compile(ctx, l, exprTypeAndVariable, branches)
            case RegisterVariable(MosRegister.AX, _) =>
              compile(ctx, l, Some(b -> RegisterVariable(MosRegister.A, b)), branches) ++
                preserveRegisterIfNeeded(ctx, MosRegister.A, compile(ctx, h, Some(b -> RegisterVariable(MosRegister.X, b)), branches))
            case RegisterVariable(MosRegister.AY, _) =>
              compile(ctx, l, Some(b -> RegisterVariable(MosRegister.A, b)), branches) ++
                preserveRegisterIfNeeded(ctx, MosRegister.A, compile(ctx, h, Some(b -> RegisterVariable(MosRegister.Y, b)), branches))
            case RegisterVariable(MosRegister.XA, _) =>
              compile(ctx, h, Some(b -> RegisterVariable(MosRegister.A, b)), branches) ++
                preserveRegisterIfNeeded(ctx, MosRegister.A, compile(ctx, l, Some(b -> RegisterVariable(MosRegister.X, b)), branches))
            case RegisterVariable(MosRegister.YA, _) =>
              compile(ctx, h, Some(b -> RegisterVariable(MosRegister.A, b)), branches) ++
                preserveRegisterIfNeeded(ctx, MosRegister.A, compile(ctx, l, Some(b -> RegisterVariable(MosRegister.Y, b)), branches))
            case RegisterVariable(MosRegister.XY, _) =>
              compile(ctx, l, Some(b -> RegisterVariable(MosRegister.X, b)), branches) ++
                preserveRegisterIfNeeded(ctx, MosRegister.X, compile(ctx, h, Some(b -> RegisterVariable(MosRegister.Y, b)), branches))
            case RegisterVariable(MosRegister.YX, _) =>
              compile(ctx, l, Some(b -> RegisterVariable(MosRegister.Y, b)), branches) ++
                preserveRegisterIfNeeded(ctx, MosRegister.Y, compile(ctx, h, Some(b -> RegisterVariable(MosRegister.X, b)), branches))
            case target: VariableInMemory =>
              target.typ.size match {
                case 1 =>
                  ctx.log.error(s"Variable `$target.name` cannot hold a word", expr.position)
                  Nil
                case 2 =>
                  compile(ctx, l, Some(b -> env.genRelativeVariable(target.toAddress, b, zeropage = target.zeropage)), branches) ++
                    compile(ctx, h, Some(b -> env.genRelativeVariable(target.toAddress + 1, b, zeropage = target.zeropage)), branches)
                case n if n > 2 =>
                  val zero = LiteralExpression(0,1).pos(expr.position)
                  compile(ctx, l, Some(b -> env.genRelativeVariable(target.toAddress, b, zeropage = target.zeropage)), branches) ++
                    compile(ctx, h, Some(b -> env.genRelativeVariable(target.toAddress + 1, b, zeropage = target.zeropage)), branches) ++
                    List.tabulate(n - 2)(i => compile(ctx, zero, Some(b -> env.genRelativeVariable(target.toAddress + (i + 2), b, zeropage = target.zeropage)), branches)).flatten
              }
            case target: StackVariable =>
              target.typ.size match {
                case 1 =>
                  ctx.log.error(s"Variable `$target.name` cannot hold a word", expr.position)
                  Nil
                case 2 =>
                  if (ctx.options.flag(CompilationFlag.SoftwareStack)) {
                    compile(ctx, l, Some(b -> StackVariable("", b, target.baseOffset)), branches) ++
                      compile(ctx, h, Some(b -> StackVariable("", b, target.baseOffset + 1)), branches)
                  } else {
                    compile(ctx, l, Some(b -> StackVariable("", b, target.baseOffset + ctx.extraStackOffset)), branches) ++
                      compile(ctx, h, Some(b -> StackVariable("", b, target.baseOffset + ctx.extraStackOffset + 1)), branches)
                  }
                case n if n > 2 =>
                  val zero = LiteralExpression(0, 1).pos(expr.position)
                  if (ctx.options.flag(CompilationFlag.SoftwareStack)) {
                    compile(ctx, l, Some(b -> StackVariable("", b, target.baseOffset)), branches) ++
                      compile(ctx, h, Some(b -> StackVariable("", b, target.baseOffset + 1)), branches) ++
                      List.tabulate(n - 2)(i => compile(ctx, zero, Some(b -> StackVariable("", b, target.baseOffset + 2 + i)), branches)).flatten
                  } else {
                    compile(ctx, l, Some(b -> StackVariable("", b, target.baseOffset + ctx.extraStackOffset)), branches) ++
                      compile(ctx, h, Some(b -> StackVariable("", b, target.baseOffset + ctx.extraStackOffset + 1)), branches) ++
                      List.tabulate(n - 2)(i => compile(ctx, zero, Some(b -> StackVariable("", b, target.baseOffset + ctx.extraStackOffset + 2 + i)), branches)).flatten
                  }

              }
          }
        }

      case f@FunctionCallExpression(name, params) =>
        var zeroExtend = false
        var signExtend = false
        var resultVariable = ""
        val calculate: List[AssemblyLine] = name match {
          case "call" =>
            params match {
              case List(fp) =>
                getExpressionType(ctx, fp) match {
                  case FunctionPointerType(_, _, _, _, Some(v)) if (v.name == "void") =>
                    compileToZReg2(ctx, fp) :+ AssemblyLine.absolute(JSR, env.get[ThingInMemory]("call"))
                  case _ =>
                    ctx.log.error("Not a function pointer", fp.position)
                    compile(ctx, fp, None, BranchSpec.None)
                }
              case List(fp, param) =>
                getExpressionType(ctx, fp) match {
                  case FunctionPointerType(_, _, _, Some(pt), Some(v)) =>
                    if (pt.size > 2 || pt.size < 1) {
                      ctx.log.error("Invalid parameter type", param.position)
                      compile(ctx, fp, None, BranchSpec.None) ++ compile(ctx, param, None, BranchSpec.None)
                    } else if (getExpressionType(ctx, param).isAssignableTo(pt)) {
                      pt.size match {
                        case 1 =>
                          compileToA(ctx, param) ++ preserveRegisterIfNeeded(ctx, MosRegister.A, compileToZReg2(ctx, fp)) :+ AssemblyLine.absolute(JSR, env.get[ThingInMemory]("call"))
                        case 2 =>
                          compileToAX(ctx, param) ++ preserveRegisterIfNeeded(ctx, MosRegister.AX, compileToZReg2(ctx, fp)) :+ AssemblyLine.absolute(JSR, env.get[ThingInMemory]("call"))
                      }

                    } else {
                      ctx.log.error("Invalid parameter type", param.position)
                      compile(ctx, fp, None, BranchSpec.None) ++ compile(ctx, param, None, BranchSpec.None)
                    }
                  case _ =>
                    ctx.log.error("Not a function pointer", fp.position)
                    compile(ctx, fp, None, BranchSpec.None) ++ compile(ctx, param, None, BranchSpec.None)
                }
              case _ =>
                ctx.log.error("Invalid call syntax", f.position)
                Nil
            }
          case "not" =>
            assertBool(ctx, "not", params, 1)
            compile(ctx, params.head, exprTypeAndVariable, branches.flip)
          case "hi" | "lo" =>
            zeroExtend = true
            val hi = name == "hi"
            if (params.length != 1) {
              ctx.log.error("Too many parameters for hi/lo", f.position)
              Nil
            } else {
              val param = params.head
              val typ = getExpressionType(ctx, param)
              if (typ.size < 1 || typ.size > 2) {
                ctx.log.error("Invalid parameter type for hi/lo", param.position)
                compile(ctx, param, None, BranchSpec.None)
              } else {
                val compilation = compile(ctx, param, Some(MosExpressionCompiler.getExpressionType(ctx, param) -> RegisterVariable(MosRegister.AX, w)), BranchSpec.None)
                if (hi) {
                  if (typ.size == 2) compilation :+ AssemblyLine.implied(TXA)
                  else if (typ.isSigned) compilation ++ signExtendA(ctx)
                  else List(AssemblyLine.immediate(LDA, 0))
                } else compilation
              }
            }
          case "sizeof" =>
            env.eval(expr) match {
              case Some(c) =>
                exprTypeAndVariable match {
                  case Some((t, v)) =>
                    compileConstant(ctx, c, w, v)
                  case _ =>
                    Nil
                }
              case None => Nil
            }
          case "nonet" =>
            if (params.length != 1) {
              ctx.log.error("Invalid number of parameters", f.position)
              Nil
            } else {
              env.eval(expr) match {
                case Some(c) =>
                  exprTypeAndVariable match {
                    case Some((t, v)) =>
                      compileConstant(ctx, c, w, v)
                    case _ =>
                      Nil
                  }
                case None =>
                  assertAllArithmeticBytes("Nonet argument has to be a byte", ctx, params)
                  params.head match {
                    case SumExpression(addends, _) =>
                      if (addends.exists(a => a._1)) {
                        ctx.log.warn("Nonet subtraction may not work as expected", expr.position)
                      }
                      if (addends.size > 2) {
                        ctx.log.warn("Nonet addition works correctly only for two operands", expr.position)
                      }
                    case FunctionCallExpression("+" | "+'" | "<<" | "<<'" | "nonet", _) => // ok
                    case _ =>
                      ctx.log.warn("Unspecified nonet operation, results might be unpredictable", expr.position)
                  }
                  val label = ctx.nextLabel("no")
                  compile(ctx, params.head, Some(b -> RegisterVariable(MosRegister.A, b)), BranchSpec.None) ++ List(
                    AssemblyLine.immediate(LDX, 0),
                    AssemblyLine.relative(BCC, label),
                    AssemblyLine.implied(INX),
                    AssemblyLine.label(label)
                  )
              }
            }
          case "&&" =>
            assertBool(ctx, "&&", params)
            branches match {
              case BranchIfFalse(_) =>
                params.flatMap(compile(ctx, _, exprTypeAndVariable, branches))
              case _ =>
                val skip = ctx.nextLabel("an")
                params.init.flatMap(compile(ctx, _, exprTypeAndVariable, BranchIfFalse(skip))) ++
                  compile(ctx, params.last, exprTypeAndVariable, branches) ++
                  List(AssemblyLine.label(skip))
            }
          case "||" =>
            assertBool(ctx, "||", params)
            branches match {
              case BranchIfTrue(_) =>
                params.flatMap(compile(ctx, _, exprTypeAndVariable, branches))
              case _ =>
                val skip = ctx.nextLabel("or")
                params.init.flatMap(compile(ctx, _, exprTypeAndVariable, BranchIfTrue(skip))) ++
                  compile(ctx, params.last, exprTypeAndVariable, branches) ++
                  List(AssemblyLine.label(skip))
            }
          case "^^" => ???
          case "&" =>
            getArithmeticParamMaxSize(ctx, params) match {
              case 1 =>
                zeroExtend = true
                BuiltIns.compileBitOps(AND, ctx, params)
              case 2 => PseudoregisterBuiltIns.compileWordBitOpsToAX(ctx, params, AND)
            }
          case "*" =>
            assertSizesForMultiplication(ctx, params, inPlace = false)
            getArithmeticParamMaxSize(ctx, params) match {
              case 1 =>
                zeroExtend = true
                BuiltIns.compileByteMultiplication(ctx, params)
              case 2 =>
                //noinspection ZeroIndexToHead
                PseudoregisterBuiltIns.compileWordMultiplication(ctx, Some(params(0)), params(1), storeInRegLo = false)
            }
          case "|" =>
            getArithmeticParamMaxSize(ctx, params) match {
              case 1 =>
                zeroExtend = true
                BuiltIns.compileBitOps(ORA, ctx, params)
              case 2 => PseudoregisterBuiltIns.compileWordBitOpsToAX(ctx, params, ORA)
            }
          case "^" =>
            getArithmeticParamMaxSize(ctx, params) match {
              case 1 =>
                zeroExtend = true
                BuiltIns.compileBitOps(EOR, ctx, params)
              case 2 => PseudoregisterBuiltIns.compileWordBitOpsToAX(ctx, params, EOR)
            }
          case ">>>>" =>
            val (l, r, size) = assertArithmeticBinary(ctx, params)
            size match {
              case 2 =>
                zeroExtend = true
                BuiltIns.compileNonetOps(ctx, l, r)
              case 1 =>
                zeroExtend = true
                BuiltIns.compileShiftOps(LSR, ctx, l ,r)
              case _ => ???
            }
          case "<<" =>
            val (l, r, size) = assertArithmeticBinary(ctx, params)
            size match {
              case 1 =>
                zeroExtend = true
                BuiltIns.compileShiftOps(ASL, ctx, l, r)
              case 2 =>
                BuiltIns.maybeCompileShiftFromByteToWord(ctx, l, r, left = true).getOrElse(PseudoregisterBuiltIns.compileWordShiftOps(left = true, ctx, l, r))
              case _ =>
                ctx.log.error("Long shift ops not supported", l.position)
                Nil
            }
          case ">>" =>
            val (l, r, size) = assertArithmeticBinary(ctx, params)
            size match {
              case 1 =>
                zeroExtend = true
                BuiltIns.compileShiftOps(LSR, ctx, l, r)
              case 2 =>
                BuiltIns.maybeCompileShiftFromByteToWord(ctx, l, r, left = false).getOrElse(PseudoregisterBuiltIns.compileWordShiftOps(left = false, ctx, l, r))
              case _ =>
                ctx.log.error("Long shift ops not supported", l.position)
                Nil
            }
          case "<<'" =>
            zeroExtend = true
            assertAllArithmeticBytes("Long shift ops not supported", ctx, params)
            val (l, r, 1) = assertArithmeticBinary(ctx, params)
            DecimalBuiltIns.compileByteShiftLeft(ctx, l, r, rotate = false)
          case ">>'" =>
            zeroExtend = true
            assertAllArithmeticBytes("Long shift ops not supported", ctx, params)
            val (l, r, 1) = assertArithmeticBinary(ctx, params)
            DecimalBuiltIns.compileByteShiftRight(ctx, l, r, rotate = false)
          case "<" =>
            // TODO: signed
            val (size, signed) = assertArithmeticComparison(ctx, params)
            compileTransitiveRelation(ctx, "<", params, exprTypeAndVariable, branches) { (l, r) =>
              size match {
                case 1 => BuiltIns.compileByteComparison(ctx, if (signed) ComparisonType.LessSigned else ComparisonType.LessUnsigned, l, r, branches)
                case 2 => BuiltIns.compileWordComparison(ctx, if (signed) ComparisonType.LessSigned else ComparisonType.LessUnsigned, l, r, branches)
                case _ => BuiltIns.compileLongComparison(ctx, if (signed) ComparisonType.LessSigned else ComparisonType.LessUnsigned, l, r, size, branches)
              }
            }
          case ">=" =>
            // TODO: signed
            val (size, signed) = assertArithmeticComparison(ctx, params)
            compileTransitiveRelation(ctx, ">=", params, exprTypeAndVariable, branches) { (l, r) =>
              size match {
                case 1 => BuiltIns.compileByteComparison(ctx, if (signed) ComparisonType.GreaterOrEqualSigned else ComparisonType.GreaterOrEqualUnsigned, l, r, branches)
                case 2 => BuiltIns.compileWordComparison(ctx, if (signed) ComparisonType.GreaterOrEqualSigned else ComparisonType.GreaterOrEqualUnsigned, l, r, branches)
                case _ => BuiltIns.compileLongComparison(ctx, if (signed) ComparisonType.GreaterOrEqualSigned else ComparisonType.GreaterOrEqualUnsigned, l, r, size, branches)
              }
            }
          case ">" =>
            // TODO: signed
            val (size, signed) = assertArithmeticComparison(ctx, params)
            compileTransitiveRelation(ctx, ">", params, exprTypeAndVariable, branches) { (l, r) =>
              size match {
                case 1 => BuiltIns.compileByteComparison(ctx, if (signed) ComparisonType.GreaterSigned else ComparisonType.GreaterUnsigned, l, r, branches)
                case 2 => BuiltIns.compileWordComparison(ctx, if (signed) ComparisonType.GreaterSigned else ComparisonType.GreaterUnsigned, l, r, branches)
                case _ => BuiltIns.compileLongComparison(ctx, if (signed) ComparisonType.GreaterSigned else ComparisonType.GreaterUnsigned, l, r, size, branches)
              }
            }
          case "<=" =>
            // TODO: signed
            val (size, signed) = assertArithmeticComparison(ctx, params)
            compileTransitiveRelation(ctx, "<=", params, exprTypeAndVariable, branches) { (l, r) =>
              size match {
                case 1 => BuiltIns.compileByteComparison(ctx, if (signed) ComparisonType.LessOrEqualSigned else ComparisonType.LessOrEqualUnsigned, l, r, branches)
                case 2 => BuiltIns.compileWordComparison(ctx, if (signed) ComparisonType.LessOrEqualSigned else ComparisonType.LessOrEqualUnsigned, l, r, branches)
                case _ => BuiltIns.compileLongComparison(ctx, if (signed) ComparisonType.LessOrEqualSigned else ComparisonType.LessOrEqualUnsigned, l, r, size, branches)
              }
            }
          case "==" =>
            val size = params.map(p => getExpressionType(ctx, p).size).max
            compileTransitiveRelation(ctx, "==", params, exprTypeAndVariable, branches) { (l, r) =>
              size match {
                case 1 => BuiltIns.compileByteComparison(ctx, ComparisonType.Equal, l, r, branches)
                case 2 => BuiltIns.compileWordComparison(ctx, ComparisonType.Equal, l, r, branches)
                case _ => BuiltIns.compileLongComparison(ctx, ComparisonType.Equal, l, r, size, branches)
              }
            }
          case "!=" =>
            val (l, r, size) = assertBinary(ctx, params)
            size match {
              case 1 => BuiltIns.compileByteComparison(ctx, ComparisonType.NotEqual, l, r, branches)
              case 2 => BuiltIns.compileWordComparison(ctx, ComparisonType.NotEqual, l, r, branches)
              case _ => BuiltIns.compileLongComparison(ctx, ComparisonType.NotEqual, l, r, size, branches)
            }
          case "+=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileInPlaceByteAddition(ctx, l, r, subtract = false, decimal = false)
              case 2 =>
                l match {
                  case v: LhsExpression =>
                    BuiltIns.compileInPlaceWordOrLongAddition(ctx, v, r, subtract = false, decimal = false)
                }
              case i if i > 2 =>
                l match {
                  case v: VariableExpression =>
                    BuiltIns.compileInPlaceWordOrLongAddition(ctx, v, r, subtract = false, decimal = false)
                  case _ =>
                    ctx.log.error("Cannot modify large object accessed via such complex expression", l.position)
                    compile(ctx, r, None, BranchSpec.None)
                }
            }
          case "-=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileInPlaceByteAddition(ctx, l, r, subtract = true, decimal = false)
              case 2 =>
                l match {
                  case v: LhsExpression =>
                    BuiltIns.compileInPlaceWordOrLongAddition(ctx, v, r, subtract = true, decimal = false)
                }
              case i if i > 2 =>
                l match {
                  case v: VariableExpression =>
                    BuiltIns.compileInPlaceWordOrLongAddition(ctx, v, r, subtract = true, decimal = false)
                  case _ =>
                    ctx.log.error("Cannot modify large object accessed via such complex expression", l.position)
                    compile(ctx, r, None, BranchSpec.None)
                }
            }
          case "+'=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileInPlaceByteAddition(ctx, l, r, subtract = false, decimal = true)
              case 2 =>
                l match {
                  case v: LhsExpression =>
                    BuiltIns.compileInPlaceWordOrLongAddition(ctx, v, r, subtract = false, decimal = true)
                }
              case i if i > 2 =>
                l match {
                  case v: VariableExpression =>
                    BuiltIns.compileInPlaceWordOrLongAddition(ctx, v, r, subtract = false, decimal = true)
                  case _ =>
                    ctx.log.error("Cannot modify large object accessed via such complex expression", l.position)
                    compile(ctx, r, None, BranchSpec.None)
                }
            }
          case "-'=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileInPlaceByteAddition(ctx, l, r, subtract = true, decimal = true)
              case 2 =>
                l match {
                  case v: LhsExpression =>
                    BuiltIns.compileInPlaceWordOrLongAddition(ctx, v, r, subtract = true, decimal = true)
                }
              case i if i > 2 =>
                l match {
                  case v: VariableExpression =>
                    BuiltIns.compileInPlaceWordOrLongAddition(ctx, v, r, subtract = true, decimal = true)
                  case _ =>
                    ctx.log.error("Cannot modify large object accessed via such complex expression", l.position)
                    compile(ctx, r, None, BranchSpec.None)
                }
            }
          case "<<=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileInPlaceByteShiftOps(ASL, ctx, l, r)
              case i if i >= 2 =>
                l match {
                  case v: LhsExpression =>
                    BuiltIns.compileInPlaceWordOrLongShiftOps(ctx, v, r, aslRatherThanLsr = true)
                }
            }
          case ">>=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileInPlaceByteShiftOps(LSR, ctx, l, r)
              case i if i >= 2 =>
                l match {
                  case v: LhsExpression =>
                    BuiltIns.compileInPlaceWordOrLongShiftOps(ctx, v, r, aslRatherThanLsr = false)
                }
            }
          case "<<'=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                DecimalBuiltIns.compileByteShiftLeft(ctx, l, r, rotate = false) ++ compileByteStorage(ctx, MosRegister.A, l)
              case i if i >= 2 =>
                l match {
                  case v: LhsExpression =>
                    DecimalBuiltIns.compileInPlaceLongShiftLeft(ctx, v, r)
                }
            }
          case ">>'=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                DecimalBuiltIns.compileByteShiftRight(ctx, l, r, rotate = false) ++ compileByteStorage(ctx, MosRegister.A, l)
              case i if i >= 2 =>
                l match {
                  case v: LhsExpression =>
                    DecimalBuiltIns.compileInPlaceLongShiftRight(ctx, v, r)
                }
            }
          case "*=" =>
            assertSizesForMultiplication(ctx, params, inPlace = true)
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileInPlaceByteMultiplication(ctx, l, r)
              case 2 =>
                BuiltIns.compileInPlaceWordMultiplication(ctx, l, r)
              case _ => ctx.log.fatal("Oops")
            }
          case "/=" | "%%=" =>
            assertSizesForDivision(ctx, params, inPlace = true)
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileUnsignedByteDivision(ctx, l, r, f.functionName == "%%=") ++ compileByteStorage(ctx, MosRegister.A, l)
              case 2 =>
                if (f.functionName == "%%=") {
                  BuiltIns.compileUnsignedWordByByteDivision(ctx, l, r, modulo = true) ++ compileByteStorage(ctx, MosRegister.A, l)
                } else {
                  compileAssignment(ctx, FunctionCallExpression("/", List(l, r)).pos(f.position), l)
                }
              case _ => ctx.log.fatal("Oops")
            }
          case "/" | "%%" =>
            assertSizesForDivision(ctx, params, inPlace = false)
            val (l, r, size) = assertArithmeticBinary(ctx, params)
            size match {
              case 1 =>
                zeroExtend = true
                BuiltIns.compileUnsignedByteDivision(ctx, l, r, f.functionName == "%%")
              case 2 =>
                BuiltIns.compileUnsignedWordByByteDivision(ctx, l, r, f.functionName == "%%")
            }
          case "*'=" =>
            assertAllArithmeticBytes("Long multiplication not supported", ctx, params)
            val (l, r, 1) = assertArithmeticAssignmentLike(ctx, params)
            DecimalBuiltIns.compileInPlaceByteMultiplication(ctx, l, r)
          case "&=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileInPlaceByteBitOp(ctx, l, r, AND)
              case i if i >= 2 =>
                l match {
                  case v: LhsExpression =>
                    BuiltIns.compileInPlaceWordOrLongBitOp(ctx, l, r, AND)
                }
            }
          case "^=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileInPlaceByteBitOp(ctx, l, r, EOR)
              case i if i >= 2 =>
                l match {
                  case v: LhsExpression =>
                    BuiltIns.compileInPlaceWordOrLongBitOp(ctx, l, r, EOR)
                }
            }
          case "|=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileInPlaceByteBitOp(ctx, l, r, ORA)
              case i if i >= 2 =>
                l match {
                  case v: LhsExpression =>
                    BuiltIns.compileInPlaceWordOrLongBitOp(ctx, l, r, ORA)
                }
            }
          case _ =>
            env.maybeGet[Type](f.functionName) match {
              case Some(typ) =>
                val sourceType = validateTypeCastAndGetSourceExpressionType(ctx, typ, params)
                exprTypeAndVariable match {
                  case None =>
                    return compile(ctx, params.head, None, branches)
                  case Some((_, targetVariable)) =>
                    val targetType = targetVariable.typ
                    val officialType: Type = if (targetType.size == typ.size) {
                      typ
                    } else if (targetType.size == sourceType.size) {
                      targetType
                    } else if (typ.isSigned == sourceType.isSigned) {
                      targetType
                    } else if (typ.size == 1) {
                      return compileToA(ctx, f) ++ expressionStorageFromA(ctx, exprTypeAndVariable, position = expr.position, signedSource = typ.isSigned)
                    } else if (typ.size == 2) {
                      return compileToAX(ctx, f) ++ expressionStorageFromAX(ctx, exprTypeAndVariable, position = expr.position)
                    } else {
                      ctx.log.fatal(s"Cannot compile $typ($sourceType(...)) to $targetType")
                    }
                    return compile(ctx, params.head, Some(officialType -> targetVariable), branches)
                }
              case None =>
                // fallthrough to the lookup below
            }
            lookupFunction(ctx, f) match {
              case function: MacroFunction =>
                val (paramPreparation, statements) = MosMacroExpander.inlineFunction(ctx, function, params, expr.position)
                paramPreparation ++ statements.map {
                  case MosAssemblyStatement(opcode, addrMode, expression, elidability) =>
                    val param = env.evalForAsm(expression).getOrElse {
                      ctx.log.error("Inlining failed due to non-constant things", expression.position)
                      Constant.Zero
                    }
                    AssemblyLine(opcode, addrMode, param, elidability)
                }
              case function: EmptyFunction =>
                ??? // TODO: type conversion?
              case function: FunctionInMemory =>
                if (function.returnType.size > 2) {
                  resultVariable = function.name + ".return"
                }
                if (function.returnType.size == 1) {
                  zeroExtend = !function.returnType.isSigned
                  signExtend = function.returnType.isSigned
                }
                function match {
                  case nf: NormalFunction =>
                    if (nf.interrupt) {
                      ctx.log.error(s"Calling an interrupt function `${f.functionName}`", expr.position)
                    }
                    if (nf.name == "main" && ctx.options.flag(CompilationFlag.SoftwareStack)) {
                      if (nf.stackVariablesSize != 0 || env.things.values.exists(_.isInstanceOf[StackVariable])) {
                        ctx.log.error("Calling the main function when using software stack is not allowed", expr.position)
                      } else {
                        ctx.log.warn("Calling the main function when using software stack is not allowed", expr.position)
                      }
                    }
                  case _ => ()
                }
                val result = function.params match {
                  case AssemblyParamSignature(paramConvs) =>
                    val pairs = params.zip(paramConvs)
                    val secondViaMemory = pairs.flatMap {
                      case (paramExpr, AssemblyParam(typ, paramVar: VariableInMemory, AssemblyParameterPassingBehaviour.Copy)) =>
                        compile(ctx, paramExpr, Some(typ -> paramVar), NoBranching)
                      case _ => Nil
                    }
                    val thirdViaRegisters = pairs.flatMap {
                      case (paramExpr, AssemblyParam(typ, paramVar@RegisterVariable(register, _), AssemblyParameterPassingBehaviour.Copy)) =>
                        Some(register -> compile(ctx, paramExpr, Some(typ -> paramVar), NoBranching))
                      case _ => Nil
                    } match {
                      case Seq() => Nil
                      case Seq((_, param)) => param
                      case Seq((MosRegister.A, pa), (_, pxy)) => pa ++ preserveRegisterIfNeeded(ctx, MosRegister.A, pxy)
                      case Seq((_, pxy), (MosRegister.A, pa)) => pa ++ preserveRegisterIfNeeded(ctx, MosRegister.A, pxy)
                      case other =>
                        ctx.log.warn("Unsupported register parameter combination: " + other.map(_._1.toString).mkString("(", ",", ")"), expr.position)
                        other.flatMap(_._2) // TODO : make sure all registers are passed in correctly
                    }
                    secondViaMemory ++ thirdViaRegisters :+ AssemblyLine.absoluteOrLongAbsolute(JSR, function, ctx.options)
                  case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 1 =>
                    compile(ctx, params.head, Some(b -> RegisterVariable(MosRegister.A, b)), BranchSpec.None) ++ List(AssemblyLine.absoluteOrLongAbsolute(JSR, function, ctx.options))
                  case NormalParamSignature(paramVars) =>
                    params.zip(paramVars).flatMap {
                      case (paramExpr, paramVar) =>
                        val callCtx = callingContext(ctx, function.name, paramVar)
                        compileAssignment(callCtx, paramExpr, VariableExpression(paramVar.name + "`aa"))
                    } ++ List(AssemblyLine.absoluteOrLongAbsolute(JSR, function, ctx.options))
                }
                result
            }
        }
        if (resultVariable == "") {
          val store: List[AssemblyLine] = if (zeroExtend || signExtend) {
            expressionStorageFromA(ctx, exprTypeAndVariable, expr.position, signExtend)
          } else {
            expressionStorageFromAX(ctx, exprTypeAndVariable, expr.position)
          }
          calculate ++ store
        } else {
          calculate ++ compile(ctx, VariableExpression(resultVariable), exprTypeAndVariable, branches)
        }
    }
  }

  private def compileTransitiveRelation(ctx: CompilationContext,
                                        operator: String,
                                        params: List[Expression],
                                        exprTypeAndVariable: Option[(Type, Variable)],
                                        branches: BranchSpec)(binary: (Expression, Expression) => List[AssemblyLine]): List[AssemblyLine] = {
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
            case IndexedExpression(_, SumExpression(ps, false)) if isUpToOneVar(ps) =>
            case _ =>
              ctx.log.warn("A complex expression may be evaluated multiple times", e.position)
          }
        }
        val conjunction = params.init.zip(params.tail).map {
          case (l, r) => FunctionCallExpression(operator, List(l, r))
        }.reduceLeft((a, b) => FunctionCallExpression("&&", List(a, b)))
        compile(ctx, conjunction, exprTypeAndVariable, branches)
    }
  }

  def expressionStorageFromAX(ctx: CompilationContext, exprTypeAndVariable: Option[(Type, Variable)], position: Option[Position]): List[AssemblyLine] = {
    exprTypeAndVariable.fold(noop) {
      case (VoidType, _) => ctx.log.fatal("Cannot assign word to void", position)
      case (_, RegisterVariable(MosRegister.A, _)) => noop
      case (_, RegisterVariable(MosRegister.AW, _)) => List(AssemblyLine.implied(XBA), AssemblyLine.implied(TXA), AssemblyLine.implied(XBA))
      case (_, RegisterVariable(MosRegister.X, _)) => List(AssemblyLine.implied(TAX))
      case (_, RegisterVariable(MosRegister.Y, _)) => List(AssemblyLine.implied(TAY))
      case (_, RegisterVariable(MosRegister.AX, _)) =>
        // TODO: sign extension
        noop
      case (_, RegisterVariable(MosRegister.XA, _)) =>
        // TODO: sign extension
        if (ctx.options.flag(CompilationFlag.EmitHudsonOpcodes)) {
          List(AssemblyLine.implied(HuSAX))
        } else if (ctx.options.flag(CompilationFlag.EmitCmosOpcodes)) {
          List(
            AssemblyLine.implied(PHA),
            AssemblyLine.implied(PHX),
            AssemblyLine.implied(PLA),
            AssemblyLine.implied(PLX))
        } else {
          List(
            AssemblyLine.implied(TAY),
            AssemblyLine.implied(TXA),
            AssemblyLine.implied(PHA),
            AssemblyLine.implied(TYA),
            AssemblyLine.implied(TAX),
            AssemblyLine.implied(PLA)) // fuck this shit
        }
      case (_, RegisterVariable(MosRegister.YA, _)) =>
        // TODO: sign extension
        List(
          AssemblyLine.implied(TAY),
          AssemblyLine.implied(TXA))
      case (_, RegisterVariable(MosRegister.AY, _)) =>
        // TODO: sign extension
        if (ctx.options.flag(CompilationFlag.EmitHudsonOpcodes)) {
          List(AssemblyLine.implied(SXY))
        } else if (ctx.options.flag(CompilationFlag.EmitEmulation65816Opcodes)) {
          List(AssemblyLine.implied(TXY))
        } else {
          List(
            AssemblyLine.implied(PHA),
            AssemblyLine.implied(TXA),
            AssemblyLine.implied(TAY),
            AssemblyLine.implied(PLA))
        }
      case (t, v: VariableInMemory) => t.size match {
        case 0 => ???
        case 1 => v.typ.size match {
          case 1 =>
            AssemblyLine.variable(ctx, STA, v)
          case s if s > 1 =>
            if (t.isSigned) {
              AssemblyLine.variable(ctx, STA, v) ++ signExtendA(ctx) ++ List.tabulate(s - 1)(i => AssemblyLine.variable(ctx, STA, v, i + 1)).flatten
            } else {
              AssemblyLine.variable(ctx, STA, v) ++ List(AssemblyLine.immediate(LDA, 0)) ++
                List.tabulate(s - 1)(i => AssemblyLine.variable(ctx, STA, v, i + 1)).flatten
            }
        }
        case 2 => v.typ.size match {
          case 1 =>
            ctx.log.error(s"Variable `${v.name}` cannot hold a word", position)
            Nil
          case 2 =>
            AssemblyLine.variable(ctx, STA, v) ++ AssemblyLine.variable(ctx, STX, v, 1)
          case s if s > 2 =>
            if (t.isSigned) {
              AssemblyLine.variable(ctx, STA, v) ++
                AssemblyLine.variable(ctx, STX, v, 1) ++
                List(AssemblyLine.implied(TXA)) ++
                signExtendA(ctx) ++
                List.tabulate(s - 2)(i => AssemblyLine.variable(ctx, STA, v, i + 2)).flatten
            } else {
              AssemblyLine.variable(ctx, STA, v) ++ AssemblyLine.variable(ctx, STX, v, 1) ++ List(
                AssemblyLine.immediate(LDA, 0)) ++
                List.tabulate(s - 2)(i => AssemblyLine.variable(ctx, STA, v, i + 2)).flatten
            }
        }
      }
      case (t, v: StackVariable) => t.size match {
        case 1 => v.typ.size match {
          case 1 =>
            AssemblyLine.tsx(ctx) :+ AssemblyLine.dataStackX(ctx, STA, v)
          case s if s > 1 =>
            AssemblyLine.tsx(ctx) ++ (if (t.isSigned) {
              List(
                AssemblyLine.dataStackX(ctx, STA, v.baseOffset)) ++
                signExtendA(ctx) ++
                List.tabulate(s - 1)(i => AssemblyLine.dataStackX(ctx, STA, v, i + 1))
            } else {
              List(
                AssemblyLine.dataStackX(ctx, STA, v.baseOffset),
                AssemblyLine.immediate(LDA, 0)) ++
                List.tabulate(s - 1)(i => AssemblyLine.dataStackX(ctx, STA, v, i + 1))
            })
        }
        case 2 => v.typ.size match {
          case 1 =>
            ctx.log.error(s"Variable `${v.name}` cannot hold a word", position)
            Nil
          case 2 =>
            List(
              AssemblyLine.implied(TAY),
              AssemblyLine.implied(TXA)) ++ AssemblyLine.tsx(ctx) ++ List(
              AssemblyLine.dataStackX(ctx, STA, v, 1),
              AssemblyLine.implied(TYA),
              AssemblyLine.dataStackX(ctx, STA, v))
          case s if s > 2 => ???
        }
      }
    }
  }

  def expressionStorageFromA(ctx: CompilationContext, exprTypeAndVariable: Option[(Type, Variable)], position: Option[Position], signedSource: Boolean): List[AssemblyLine] = {
    exprTypeAndVariable.fold(noop) {
      case (VoidType, _) => ctx.log.fatal("Cannot assign word to void", position)
      case (_, RegisterVariable(MosRegister.A, _)) => noop
      case (typ, RegisterVariable(MosRegister.AW, _)) =>
        if (signedSource) List(AssemblyLine.implied(TAX)) ++ signExtendA(ctx) ++ List(AssemblyLine.implied(XBA), AssemblyLine.implied(TXA))
        else List(AssemblyLine.implied(XBA), AssemblyLine.immediate(LDA, 0), AssemblyLine.implied(XBA))
      case (_, RegisterVariable(MosRegister.X, _)) => List(AssemblyLine.implied(TAX))
      case (_, RegisterVariable(MosRegister.Y, _)) => List(AssemblyLine.implied(TAY))
      case (typ, RegisterVariable(MosRegister.AX, _)) =>
        if (signedSource) {
          if (ctx.options.flag(CompilationFlag.EmitHudsonOpcodes)) {
            List(AssemblyLine.implied(TAX)) ++ signExtendA(ctx) ++ List(AssemblyLine.implied(HuSAX))
        } else {
            List(AssemblyLine.implied(PHA)) ++ signExtendA(ctx) ++ List(AssemblyLine.implied(TAX), AssemblyLine.implied(PLA))
          }
        } else List(AssemblyLine.immediate(LDX, 0))
      case (typ, RegisterVariable(MosRegister.XA, _)) =>
        if (signedSource) {
          List(AssemblyLine.implied(TAX)) ++ signExtendA(ctx)
        } else {
          List(AssemblyLine.implied(TAX), AssemblyLine.immediate(LDA, 0))
        }
      case (typ, RegisterVariable(MosRegister.YA, _)) =>
        if (signedSource) {
          List(AssemblyLine.implied(TAY)) ++ signExtendA(ctx)
        } else {
          List(AssemblyLine.implied(TAY), AssemblyLine.immediate(LDA, 0))
        }
      case (typ, RegisterVariable(MosRegister.AY, _)) =>
        if (signedSource) {
          if (ctx.options.flag(CompilationFlag.EmitHudsonOpcodes)) {
            List(AssemblyLine.implied(TAY)) ++ signExtendA(ctx) ++ List(AssemblyLine.implied(SAY))
        } else {
            List(AssemblyLine.implied(PHA)) ++ signExtendA(ctx) ++ List(AssemblyLine.implied(TAY), AssemblyLine.implied(PLA))
          }
        } else List(AssemblyLine.immediate(LDY, 0))
      case (t, v: VariableInMemory) =>
        v.typ.size match {
          case 1 =>
            AssemblyLine.variable(ctx, STA, v)
          case s if s > 1 =>
            if (signedSource) {
              AssemblyLine.variable(ctx, STA, v) ++ signExtendA(ctx) ++ List.tabulate(s - 1)(i => AssemblyLine.variable(ctx, STA, v, i + 1)).flatten
            } else {
              AssemblyLine.variable(ctx, STA, v) ++ List(AssemblyLine.immediate(LDA, 0)) ++
                List.tabulate(s - 1)(i => AssemblyLine.variable(ctx, STA, v, i + 1)).flatten
            }
        }
      case (t, v: StackVariable) =>
        v.typ.size match {
          case 1 =>
            AssemblyLine.tsx(ctx) :+ AssemblyLine.dataStackX(ctx, STA, v)
          case s if s > 1 =>
            AssemblyLine.tsx(ctx) ++ (if (signedSource) {
              List(
                AssemblyLine.dataStackX(ctx, STA, v.baseOffset)) ++
                signExtendA(ctx) ++
                List.tabulate(s - 1)(i => AssemblyLine.dataStackX(ctx, STA, v, i + 1))
            } else {
              List(
                AssemblyLine.dataStackX(ctx, STA, v.baseOffset),
                AssemblyLine.immediate(LDA, 0)) ++
                List.tabulate(s - 1)(i => AssemblyLine.dataStackX(ctx, STA, v, i + 1))
            })
        }
    }
  }

  def expressionStorageFromAW(ctx: CompilationContext, exprTypeAndVariable: Option[(Type, Variable)], position: Option[Position]): List[AssemblyLine] = {
    exprTypeAndVariable.fold(noop) {
      case (VoidType, _) => ctx.log.fatal("Cannot assign word to void", position)
      case (_, RegisterVariable(MosRegister.A, _)) => noop
      case (_, RegisterVariable(MosRegister.AW, _)) => noop
      case (_, RegisterVariable(MosRegister.X, _)) => List(AssemblyLine.implied(TAX))
      case (_, RegisterVariable(MosRegister.Y, _)) => List(AssemblyLine.implied(TAY))
      case (_, RegisterVariable(MosRegister.AX, _)) =>
        // TODO: sign extension
        List(AssemblyLine.implied(XBA), AssemblyLine.implied(TAX), AssemblyLine.implied(XBA))
      case (_, RegisterVariable(MosRegister.XA, _)) =>
        // TODO: sign extension
        List(AssemblyLine.implied(TAX), AssemblyLine.implied(XBA))
      case (_, RegisterVariable(MosRegister.YA, _)) =>
        // TODO: sign extension
        List(AssemblyLine.implied(TAY), AssemblyLine.implied(XBA))
      case (_, RegisterVariable(MosRegister.AY, _)) =>
        // TODO: sign extension
        List(AssemblyLine.implied(XBA), AssemblyLine.implied(TAY), AssemblyLine.implied(XBA))
      case (t, v: VariableInMemory) => t.size match {
        case 1 => v.typ.size match {
          case 1 =>
            AssemblyLine.variable(ctx, STA, v)
          case s if s > 1 =>
            if (t.isSigned) {
              AssemblyLine.variable(ctx, STA, v) ++ signExtendA(ctx) ++ List.tabulate(s - 1)(i => AssemblyLine.variable(ctx, STA, v, i + 1)).flatten
            } else {
              AssemblyLine.variable(ctx, STA, v) ++ List(AssemblyLine.immediate(LDA, 0)) ++
                List.tabulate(s - 1)(i => AssemblyLine.variable(ctx, STA, v, i + 1)).flatten
            }
        }
        case 2 => v.typ.size match {
          case 1 =>
            ctx.log.error(s"Variable `${v.name}` cannot hold a word", position)
            Nil
          case 2 =>
            AssemblyLine.accu16 :: (AssemblyLine.variable(ctx, STA_W, v) :+ AssemblyLine.accu8)
          case s if s > 2 =>
            if (t.isSigned) {
              AssemblyLine.accu16 :: AssemblyLine.variable(ctx, STA_W, v) ++
                List(AssemblyLine.accu8, AssemblyLine.implied(XBA)) ++
                signExtendA(ctx) ++
                List.tabulate(s - 2)(i => AssemblyLine.variable(ctx, STA, v, i + 2)).flatten
            } else {
              AssemblyLine.accu16 :: AssemblyLine.variable(ctx, STA_W, v)
                List(AssemblyLine.accu8, AssemblyLine.immediate(LDA, 0)) ++
                List.tabulate(s - 2)(i => AssemblyLine.variable(ctx, STA, v, i + 2)).flatten
            }
        }
      }
      case (t, v: StackVariable) => t.size match {
        case 1 => v.typ.size match {
          case 1 =>
            AssemblyLine.tsx(ctx) :+ AssemblyLine.dataStackX(ctx, STA, v)
          case s if s > 1 =>
            AssemblyLine.tsx(ctx) ++ (if (t.isSigned) {
              List(
                AssemblyLine.dataStackX(ctx, STA, v.baseOffset)) ++
                signExtendA(ctx) ++
                List.tabulate(s - 1)(i => AssemblyLine.dataStackX(ctx, STA, v, i + 1))
            } else {
              List(
                AssemblyLine.dataStackX(ctx, STA, v.baseOffset),
                AssemblyLine.immediate(LDA, 0)) ++
                List.tabulate(s - 1)(i => AssemblyLine.dataStackX(ctx, STA, v, i + 1))
            })
        }
        case 2 => v.typ.size match {
          case 1 =>
            ctx.log.error(s"Variable `${v.name}` cannot hold a word", position)
            Nil
          case 2 =>
              AssemblyLine.tsx(ctx) ++ List(AssemblyLine.accu16, AssemblyLine.dataStackX(ctx, STA_W, v), AssemblyLine.accu8)
          case s if s > 2 => ???
        }
      }
    }
  }

  def compileAssignment(ctx: CompilationContext, source: Expression, target: LhsExpression): List[AssemblyLine] = {
    val env = ctx.env
    val sourceType = AbstractExpressionCompiler.checkAssignmentTypeAndGetSourceType(ctx, source, target)
    if (target == BlackHoleExpression) return compile(ctx, source, None, NoBranching)
    val lhsType = AbstractExpressionCompiler.getExpressionType(ctx, target)
    val b = env.get[Type]("byte")
    val w = env.get[Type]("word")
    target match {
      case VariableExpression(name) =>
        val v = env.get[Variable](name, target.position)
        // TODO check v.typ
        if (v.typ == FatBooleanType) {
          compileToFatBooleanInA(ctx, source) ++ compileByteStorage(ctx, MosRegister.A, target)
        } else {
          compile(ctx, source, Some((sourceType, v)), NoBranching)
        }
      case SeparateBytesExpression(h: LhsExpression, l: LhsExpression) =>
        compile(ctx, source, Some(w, RegisterVariable(MosRegister.AX, w)), NoBranching) ++
          compileByteStorage(ctx, MosRegister.A, l) ++ compileByteStorage(ctx, MosRegister.X, h)
      case SeparateBytesExpression(_, _) =>
        ctx.log.error("Invalid left-hand-side use of `:`")
        Nil
      case DerefExpression(inner, offset, targetType) =>
        val (prepare, addr, am) = getPhysicalPointerForDeref(ctx, inner)
        env.eval(source) match {
          case Some(constant) =>
            am match {
              case AbsoluteY =>
                prepare ++ (0 until targetType.size).flatMap(i => List(
                  AssemblyLine.immediate(LDA, constant.subbyte(i)),
                  AssemblyLine.absolute(STA, addr + offset + i)))
              case _ =>
                prepare ++ (0 until targetType.size).flatMap(i => List(
                  if (i == 0) AssemblyLine.immediate(LDY, offset) else AssemblyLine.implied(INY),
                  AssemblyLine.immediate(LDA, constant.subbyte(i)),
                  AssemblyLine(STA, am, addr)))
            }
          case None =>
            source match {
              case VariableExpression(vname) =>
                val variable = env.get[Variable](vname)
                (targetType.size, am) match {
                  case (1, AbsoluteY) =>
                    prepare ++
                      AssemblyLine.variable(ctx, LDA, variable) :+
                      AssemblyLine.absolute(STA, addr + offset)
                  case (1, _) =>
                    prepare ++
                      AssemblyLine.variable(ctx, LDA, variable) ++ List(
                      AssemblyLine.immediate(LDY, offset),
                      AssemblyLine(STA, am, addr))
                  case (_, AbsoluteY) =>
                    prepare ++ (0 until targetType.size).flatMap { i =>
                      val load = if (i >= sourceType.size) List(AssemblyLine.immediate(LDA, 0)) else AssemblyLine.variable(ctx, LDA, variable, i)
                      load ++ List(
                        AssemblyLine.absolute(STA, addr + offset + i))
                    }
                  case (_, _) =>
                    prepare ++ (0 until targetType.size).flatMap { i =>
                      val load = if (i >= sourceType.size) List(AssemblyLine.immediate(LDA, 0)) else AssemblyLine.variable(ctx, LDA, variable, i)
                      load ++ List(
                        if (i == 0) AssemblyLine.immediate(LDY, offset) else AssemblyLine.implied(INY),
                        AssemblyLine(STA, am, addr))
                    }
                  case _ =>
                    ctx.log.error("Cannot assign to a large object indirectly", target.position)
                    Nil
                }
              case DerefExpression(innerSource, sourceOffset, _) =>
                val (prepareSource, addrSource, amSource) = getPhysicalPointerForDeref(ctx, innerSource)
                (am, amSource) match {
                  case (AbsoluteY, AbsoluteY) =>
                    prepare ++ prepareSource ++ (0 until targetType.size).flatMap { i =>
                      if (i >= sourceType.size) List(
                        AssemblyLine.immediate(LDA, 0),
                        AssemblyLine.absolute(STA, addr + offset + i))
                      else List(
                        AssemblyLine.absolute(LDA, addrSource + sourceOffset + i),
                        AssemblyLine.absolute(STA, addr + offset + i))
                    }
                  case (AbsoluteY, _) =>
                    prepare ++ prepareSource ++ (0 until targetType.size).flatMap { i =>
                      if (i >= sourceType.size) List(
                        AssemblyLine.immediate(LDA, 0),
                        AssemblyLine.absolute(STA, addr + offset + i))
                      else List(
                        if (i == 0) AssemblyLine.immediate(LDY, sourceOffset) else AssemblyLine.implied(INY),
                        AssemblyLine(LDA, amSource, addrSource),
                        AssemblyLine.absolute(STA, addr + offset + i))
                    }
                  case (_, AbsoluteY) =>
                    prepare ++ prepareSource ++ (0 until targetType.size).flatMap { i =>
                      if (i >= sourceType.size) List(
                        if (i == 0) AssemblyLine.immediate(LDY, sourceOffset) else AssemblyLine.implied(INY),
                        AssemblyLine.immediate(LDA, 0),
                        AssemblyLine(STA, am, addr))
                      else List(
                        if (i == 0) AssemblyLine.immediate(LDY, sourceOffset) else AssemblyLine.implied(INY),
                        AssemblyLine.absolute(LDA, addrSource + sourceOffset + i),
                        AssemblyLine(STA, am, addr))
                    }
                  case (IndexedY, IndexedY) =>
                    val reg = env.get[ThingInMemory]("__reg.loword")
                    (addr, addrSource) match {
                      case (MemoryAddressConstant(th1: Thing), MemoryAddressConstant(th2: Thing))
                        if (th1.name == "__reg.loword" || th1.name == "__reg.b2b3" || th1.name == "__reg") && (th2.name == "__reg.loword" || th2.name == "__reg.b2b3" || th2.name == "__reg") =>
                        (MosExpressionCompiler.changesZpreg(prepareSource, 2) || MosExpressionCompiler.changesZpreg(prepareSource, 3),
                          MosExpressionCompiler.changesZpreg(prepareSource, 2) || MosExpressionCompiler.changesZpreg(prepareSource, 3)) match {
                          case (_, false) =>
                            prepare ++ List(
                              AssemblyLine.zeropage(LDA, reg),
                              AssemblyLine.zeropage(STA, reg, 2),
                              AssemblyLine.zeropage(LDA, reg, 1),
                              AssemblyLine.zeropage(STA, reg, 3)) ++ prepareSource ++ (0 until targetType.size).flatMap { i =>
                              if (i >= sourceType.size) List(
                                if (i == 0) AssemblyLine.immediate(LDY, sourceOffset) else AssemblyLine.implied(INY),
                                AssemblyLine.immediate(LDA, 0),
                                AssemblyLine.indexedY(STA, reg, 2))
                              else List(
                                if (i == 0) AssemblyLine.immediate(LDY, sourceOffset) else AssemblyLine.implied(INY),
                                AssemblyLine.indexedY(LDA, reg),
                                AssemblyLine.indexedY(STA, reg, 2))
                            }
                          case (false, true) =>
                            prepareSource ++ List(
                              AssemblyLine.zeropage(LDA, reg),
                              AssemblyLine.zeropage(STA, reg, 2),
                              AssemblyLine.zeropage(LDA, reg, 1),
                              AssemblyLine.zeropage(STA, reg, 3)) ++ prepare ++ (0 until targetType.size).flatMap { i =>
                              if (i >= sourceType.size) List(
                                if (i == 0) AssemblyLine.immediate(LDY, sourceOffset) else AssemblyLine.implied(INY),
                                AssemblyLine.immediate(LDA, 0),
                                AssemblyLine.indexedY(STA, reg))
                              else List(
                                if (i == 0) AssemblyLine.immediate(LDY, sourceOffset) else AssemblyLine.implied(INY),
                                AssemblyLine.indexedY(LDA, reg, 2),
                                AssemblyLine.indexedY(STA, reg))
                            }
                          case _ =>
                            prepare ++ List(
                              AssemblyLine.zeropage(LDA, reg, 1),
                              AssemblyLine.implied(PHA),
                              AssemblyLine.zeropage(LDA, reg),
                              AssemblyLine.implied(PHA)) ++ fixTsx(fixTsx(prepareSource)) ++ List(
                              AssemblyLine.implied(PLA),
                              AssemblyLine.zeropage(STA, reg, 2),
                              AssemblyLine.implied(PLA),
                              AssemblyLine.zeropage(STA, reg, 3)) ++ (0 until targetType.size).flatMap { i =>
                              if (i >= sourceType.size) List(
                                if (i == 0) AssemblyLine.immediate(LDY, sourceOffset) else AssemblyLine.implied(INY),
                                AssemblyLine.immediate(LDA, 0),
                                AssemblyLine.indexedY(STA, reg, 2))
                              else List(
                                if (i == 0) AssemblyLine.immediate(LDY, sourceOffset) else AssemblyLine.implied(INY),
                                AssemblyLine.indexedY(LDA, reg),
                                AssemblyLine.indexedY(STA, reg, 2))
                            }
                        }
                      case _ => ???
                    }
                  case _ => ???
                }
              case _ =>
                (targetType.size, am) match {
                  case (1, _) =>
                    compile(ctx, source, Some(targetType, RegisterVariable(MosRegister.A, targetType)), BranchSpec.None) ++ compileByteStorage(ctx, MosRegister.A, target)
                  case (2, AbsoluteY) =>
                    val someTuple = Some(targetType, RegisterVariable(MosRegister.AX, targetType))
                    // TODO: optimize if prepare is empty
                    if (prepare.isEmpty) {
                      compile(ctx, source, someTuple, BranchSpec.None) ++ List(
                        AssemblyLine.absolute(STA, addr + offset),
                        AssemblyLine.absolute(STX, addr + offset + 1))
                    } else {
                      compile(ctx, source, someTuple, BranchSpec.None) ++ List(
                        AssemblyLine.implied(PHA),
                        AssemblyLine.implied(TXA),
                        AssemblyLine.implied(PHA)) ++ prepare ++ List(
                        AssemblyLine.implied(PLA),
                        AssemblyLine.absolute(STA, addr + offset + 1),
                        AssemblyLine.implied(PLA),
                        AssemblyLine.absolute(STA, addr + offset))
                    }
                  case (2, _) =>
                    val someTuple = Some(targetType, RegisterVariable(MosRegister.AX, targetType))
                    if (prepare.isEmpty) {
                      compile(ctx, source, someTuple, BranchSpec.None) ++ List(
                        AssemblyLine.immediate(LDY, offset),
                        AssemblyLine.indexedY(STA, addr),
                        AssemblyLine.implied(TXA),
                        AssemblyLine.implied(INY),
                        AssemblyLine.indexedY(STA, addr))
                    } else {
                      compile(ctx, source, someTuple, BranchSpec.None) ++ List(
                        AssemblyLine.implied(PHA),
                        AssemblyLine.implied(TXA),
                        AssemblyLine.implied(PHA)) ++ prepare ++ List(
                        AssemblyLine.immediate(LDY, offset+1),
                        AssemblyLine.implied(PLA),
                        AssemblyLine.indexedY(STA, addr),
                        AssemblyLine.implied(PLA),
                        AssemblyLine.implied(DEY),
                        AssemblyLine.indexedY(STA, addr))
                    }
                  case _ =>
                    ctx.log.error("Cannot assign to a large object indirectly", target.position)
                    Nil
                }
            }
        }
      case i: IndexedExpression =>
        if (AbstractExpressionCompiler.getExpressionType(ctx, target).size != 1) {
          ctx.log.error("Cannot store a large object this way", target.position)
        }
        compile(ctx, source, Some(b, RegisterVariable(MosRegister.A, b)), NoBranching) ++ compileByteStorage(ctx, MosRegister.A, target)
      case _ =>
        compile(ctx, source, Some(b, RegisterVariable(MosRegister.A, b)), NoBranching) ++ compileByteStorage(ctx, MosRegister.A, target)
    }
  }

  def arrayBoundsCheck(ctx: CompilationContext, pointy: Pointy, register: MosRegister.Value, index: Expression): List[AssemblyLine] = {
    if (!ctx.options.flags(CompilationFlag.CheckIndexOutOfBounds)) return Nil
    val arrayLength:Int = pointy match {
      case _: VariablePointy => return Nil
      case p: ConstantPointy => p.sizeInBytes match {
        case None => return Nil
        case Some(s) => s
      }
    }
    ctx.env.eval(index) match {
      case Some(NumericConstant(i, _)) =>
        if (i >= 0) {
          if (i < arrayLength) return Nil
          if (i >= arrayLength) return List(
            AssemblyLine.implied(PHP),
            AssemblyLine.absoluteOrLongAbsolute(JSR, ctx.env.get[ThingInMemory]("_panic"), ctx.options))
        }
      case _ =>
    }
    if (arrayLength > 0 && arrayLength < 255) {
      val label = ctx.nextLabel("bc")
      val compare = register match {
        case MosRegister.A => CMP
        case MosRegister.X => CPX
        case MosRegister.Y => CPY
      }
      List(
        AssemblyLine.implied(PHP),
        AssemblyLine.immediate(compare, arrayLength),
        AssemblyLine.relative(BCC, label),
        AssemblyLine.absoluteOrLongAbsolute(JSR, ctx.env.get[ThingInMemory]("_panic"), ctx.options),
        AssemblyLine.label(label),
        AssemblyLine.implied(PLP))
    } else {
      Nil
    }
  }

  private def signExtendA(ctx: CompilationContext): List[AssemblyLine] = {
    val label = ctx.nextLabel("sx")
    List(
      AssemblyLine.immediate(ORA, 0x7F),
      AssemblyLine.relative(BMI, label),
      AssemblyLine.immediate(LDA, 0),
      AssemblyLine.label(label))
  }

  private def shouldCopyFromHiToLo(srcAddress: Constant, destAddress: Constant): Boolean = (srcAddress, destAddress) match {
    case (
      CompoundConstant(MathOperator.Plus, a: MemoryAddressConstant, NumericConstant(s, _)),
      CompoundConstant(MathOperator.Plus, b: MemoryAddressConstant, NumericConstant(d, _))
      ) if a == b => s < d
    case (
     a: MemoryAddressConstant,
      CompoundConstant(MathOperator.Plus, b: MemoryAddressConstant, NumericConstant(d, _))
      ) if a == b => 0 < d
    case (NumericConstant(s, _), NumericConstant(d, _)) => s < d
    case _ => false
  }
}
