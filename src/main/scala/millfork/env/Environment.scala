package millfork.env

import millfork.assembly.BranchingOpcodeMapping
import millfork.{env, _}
import millfork.assembly.mos.Opcode
import millfork.assembly.z80.{IfFlagClear, IfFlagSet, ZFlag}
import millfork.compiler.{AbstractExpressionCompiler, LabelGenerator}
import millfork.error.Logger
import millfork.node._
import millfork.output._

import scala.collection.mutable


/**
  * @author Karol Stasiak
  */
//noinspection NotImplementedCode
class Environment(val parent: Option[Environment], val prefix: String, val cpuFamily: CpuFamily.Value, val jobContext: JobContext) {

  @inline
  def log: Logger = jobContext.log
  @inline
  def nextLabel: LabelGenerator = jobContext.nextLabel

  private var baseStackOffset: Int = cpuFamily match {
    case CpuFamily.M6502 => 0x101
    case CpuFamily.I80 => 0
  }

  def errorConstant(msg: String, position: Option[Position] = None): Constant = {
    log.error(msg, position)
    Constant.Zero
  }

  def genRelativeVariable(constant: Constant, typ: Type, zeropage: Boolean): RelativeVariable = {
    val variable = RelativeVariable(nextLabel("rv"), constant, typ, zeropage = zeropage, declaredBank = None /*TODO*/, isVolatile = false)
    addThing(variable, None)
    variable
  }


  def allThings: Environment = {
    val allThings: Map[String, Thing] = things.values.map {
      case m: FunctionInMemory =>
        m.environment.getAllPrefixedThings
      case m: MacroFunction =>
        m.environment.getAllPrefixedThings
      case _ => Map[String, Thing]()
    }.fold(things.toMap)(_ ++ _)
    val e = new Environment(None, "", cpuFamily, jobContext)
    e.things.clear()
    e.things ++= allThings
    e
  }


  private def getAllPrefixedThings = {
    things.toMap.map { case (n, th) => (if (n.startsWith(".")) n else prefix + n, th) }
  }

  def getAllLocalVariables: List[Variable] = things.values.flatMap {
    case v: Variable =>
      Some(v)
    case _ => None
  }.toList

  def allPreallocatables: List[PreallocableThing] = things.values.flatMap {
    case m: NormalFunction => Some(m)
    case m: InitializedArray => Some(m)
    case m: InitializedMemoryVariable => Some(m)
    case _ => None
  }.toList

  def allConstants: List[ConstantThing] = things.values.flatMap {
    case m: NormalFunction => m.environment.allConstants
    case m: MacroFunction => m.environment.allConstants
    case m: ConstantThing => List(m)
    case _ => Nil
  }.toList

  def getAllFixedAddressObjects: List[(String, Int, Int)] = {
    things.values.flatMap {
      case RelativeArray(_, NumericConstant(addr, _), size, declaredBank, _, _) =>
        List((declaredBank.getOrElse("default"), addr.toInt, size))
      case RelativeVariable(_, NumericConstant(addr, _), typ, _, declaredBank, _) =>
        List((declaredBank.getOrElse("default"), addr.toInt, typ.size))
      case f: NormalFunction =>
        f.environment.getAllFixedAddressObjects
      case _ => Nil
    }.toList
  }

  private def isLocalVariableName(name: String): Boolean = name.contains('$') && !name.endsWith("$") && {
    val fname = name.substring(0, name.indexOf('$'))
    things.get(fname) -> parent.flatMap(_.things.get(fname)) match {
      case (Some(_: NormalFunction), _) => true
      case (_, Some(_: NormalFunction)) => true
      case _ => false
    }
  }

  def allocateVariables(nf: Option[NormalFunction],
                        mem: CompiledMemory,
                        callGraph: CallGraph,
                        allocators: Map[String, VariableAllocator],
                        options: CompilationOptions,
                        onEachVariable: (String, Int) => Unit,
                        pass: Int,
                        forZpOnly: Boolean): Unit = {
    if (forZpOnly && !options.platform.hasZeroPage) {
      return
    }
    val b = get[Type]("byte")
    val p = get[Type]("pointer")
    val params = nf.fold(List[String]()) { f =>
      f.params match {
        case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 1 && options.platform.cpuFamily == CpuFamily.M6502 =>
          Nil
        case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 1 && options.platform.cpuFamily == CpuFamily.I80 =>
          Nil
        case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 2 && options.platform.cpuFamily == CpuFamily.I80 =>
          Nil
        case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 3 && options.platform.cpuFamily == CpuFamily.I80 =>
          Nil
        case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 4 && options.platform.cpuFamily == CpuFamily.I80 =>
          Nil
        case NormalParamSignature(ps) =>
          ps.map(p => p.name)
        case _ =>
          Nil
      }
    }.toSet
    def passForAlloc(m: VariableAllocationMethod.Value): Int = if (options.platform.hasZeroPage) m match {
      case VariableAllocationMethod.Zeropage =>  1
      case VariableAllocationMethod.Register => 2
      case _ => 3
    } else 3
    val toAdd = things.values.flatMap {
      case m: UninitializedMemory if passForAlloc(m.alloc) == pass && nf.isDefined == isLocalVariableName(m.name) && !m.name.endsWith(".addr") && maybeGet[Thing](m.name + ".array").isEmpty =>
        val vertex = if (options.flag(CompilationFlag.VariableOverlap)) {
          nf.fold[VariableVertex](GlobalVertex) { f =>
            if (m.alloc == VariableAllocationMethod.Static) {
              GlobalVertex
            } else if (params(m.name)) {
              ParamVertex(f.name)
            } else {
              LocalVertex(f.name)
            }
          }
        } else GlobalVertex
        val bank = m.bank(options)
        m.alloc match {
          case VariableAllocationMethod.None =>
            Nil
          case VariableAllocationMethod.Zeropage =>
            if (forZpOnly || !options.platform.hasZeroPage) {
              val addr =
                allocators(bank).allocateBytes(mem.banks(bank), callGraph, vertex, options, m.sizeInBytes, initialized = false, writeable = true, location = AllocationLocation.Zeropage, alignment = m.alignment)
              onEachVariable(m.name, addr)
              List(
                ConstantThing(m.name.stripPrefix(prefix) + "`", NumericConstant(addr, 2), p)
              )
            } else Nil
          case VariableAllocationMethod.Auto | VariableAllocationMethod.Register | VariableAllocationMethod.Static =>
            if (m.alloc == VariableAllocationMethod.Register) {
              log.warn(s"Failed to inline variable `${m.name}` into a register", None)
            }
            if (m.sizeInBytes == 0) Nil else {
              val graveName = m.name.stripPrefix(prefix) + "`"
              if (forZpOnly) {
                if (bank == "default") {
                  allocators(bank).tryAllocateZeropageBytes(mem.banks(bank), callGraph, vertex, options, m.sizeInBytes, alignment = m.alignment) match {
                    case None => Nil
                    case Some(addr) =>
                      onEachVariable(m.name, addr)
                      List(
                        ConstantThing(m.name.stripPrefix(prefix) + "`", NumericConstant(addr, 2), p)
                      )
                  }
                } else Nil
              } else if (things.contains(graveName)) {
                Nil
              } else {
                val addr = allocators(bank).allocateBytes(mem.banks(bank), callGraph, vertex, options, m.sizeInBytes, initialized = false, writeable = true, location = AllocationLocation.Either, alignment = m.alignment)
                onEachVariable(m.name, addr)
                List(
                  ConstantThing(graveName, NumericConstant(addr, 2), p)
                )
              }
            }
        }
      case f: NormalFunction =>
        f.environment.allocateVariables(Some(f), mem, callGraph, allocators, options, onEachVariable, pass, forZpOnly)
        Nil
      case _ => Nil
    }.toList
    val tagged: List[(String, Thing)] = toAdd.map(x => x.name -> x)
    things ++= tagged
  }

  val things: mutable.Map[String, Thing] = mutable.Map()
  val removedThings: mutable.Set[String] = mutable.Set()

  private def addThing(t: Thing, position: Option[Position]): Unit = {
    assertNotDefined(t.name, position)
    things(t.name.stripPrefix(prefix)) = t
  }

  def removeVariable(str: String): Unit = {
    log.trace("Removing variable: " + str)
    removeVariableImpl(str)
  }

  private def removeVariableImpl(str: String): Unit = {
    removedThings += str
    removedThings += str + ".addr"
    removedThings += str + ".addr.lo"
    removedThings += str + ".addr.hi"
    removedThings += str + ".rawaddr"
    removedThings += str + ".rawaddr.lo"
    removedThings += str + ".rawaddr.hi"
    things -= str
    things -= str + ".addr"
    things -= str + ".addr.lo"
    things -= str + ".addr.hi"
    things -= str + ".rawaddr"
    things -= str + ".rawaddr.lo"
    things -= str + ".rawaddr.hi"
    things -= str.stripPrefix(prefix)
    things -= str.stripPrefix(prefix) + ".addr"
    things -= str.stripPrefix(prefix) + ".addr.lo"
    things -= str.stripPrefix(prefix) + ".addr.hi"
    things -= str.stripPrefix(prefix) + ".rawaddr"
    things -= str.stripPrefix(prefix) + ".rawaddr.lo"
    things -= str.stripPrefix(prefix) + ".rawaddr.hi"
    parent.foreach(_ removeVariableImpl str)
  }

  def get[T <: Thing : Manifest](name: String, position: Option[Position] = None): T = {
    val clazz = implicitly[Manifest[T]].runtimeClass
    if (things.contains(name)) {
      val t: Thing = things(name)
      if ((t ne null) && clazz.isInstance(t)) {
        t.asInstanceOf[T]
      } else {
        t match {
          case Alias(_, target, deprectated) =>
            if (deprectated) {
              log.warn(s"Alias `$name` is deprecated, use `$target` instead", position)
            }
            root.get[T](target)
          case _ => log.fatal(s"`$name` is not a ${clazz.getSimpleName}", position)
        }
      }
    } else parent.fold {
      log.fatal(s"${clazz.getSimpleName} `$name` is not defined", position)
    } {
      _.get[T](name, position)
    }
  }
  
  def root: Environment = parent.fold(this)(_.root)

  def maybeGet[T <: Thing : Manifest](name: String): Option[T] = {
    if (things.contains(name)) {
      val t: Thing = things(name)
      val clazz = implicitly[Manifest[T]].runtimeClass
      t match {
        case Alias(_, target, deprectated) =>
          if (deprectated) {
            log.warn(s"Alias `$name` is deprecated, use `$target` instead")
          }
          root.maybeGet[T](target)
        case _ =>
          if ((t ne null) && clazz.isInstance(t)) {
            Some(t.asInstanceOf[T])
          } else {
            None
          }
      }
    } else parent.flatMap {
      _.maybeGet[T](name)
    }
  }

  def getArrayOrPointer(arrayName: String): Thing = {
    maybeGet[ThingInMemory](arrayName).
      orElse(maybeGet[ThingInMemory](arrayName + ".array")).
      orElse(maybeGet[ConstantThing](arrayName)).
      getOrElse(log.fatal(s"`$arrayName` is not an array or a pointer"))
  }

  def getPointy(name: String): Pointy = {
    InitializedMemoryVariable
    UninitializedMemoryVariable
    getArrayOrPointer(name) match {
      case th@InitializedArray(_, _, cs, _, i, e, _) => ConstantPointy(th.toAddress, Some(name), Some(cs.length), i, e, th.alignment)
      case th@UninitializedArray(_, size, _, i, e, _) => ConstantPointy(th.toAddress, Some(name), Some(size), i, e, th.alignment)
      case th@RelativeArray(_, _, size, _, i, e) => ConstantPointy(th.toAddress, Some(name), Some(size), i, e, NoAlignment)
      case ConstantThing(_, value, typ) if typ.size <= 2 && typ.isPointy =>
        val b = get[VariableType]("byte")
        val w = get[VariableType]("word")
        ConstantPointy(value, None, None, w, b, NoAlignment)
      case th:VariableInMemory if th.typ.isPointy=>
        val b = get[VariableType]("byte")
        val w = get[VariableType]("word")
        VariablePointy(th.toAddress, w, b)
      case _ =>
        log.error(s"$name is not a valid pointer or array")
        val b = get[VariableType]("byte")
        val w = get[VariableType]("word")
        ConstantPointy(Constant.Zero, None, None, w, b, NoAlignment)
    }
  }

  if (parent.isEmpty) {
    addThing(VoidType, None)
    addThing(BuiltInBooleanType, None)
    val b = BasicPlainType("byte", 1)
    val w = BasicPlainType("word", 2)
    addThing(b, None)
    addThing(w, None)
    addThing(Alias("int8", "byte"), None)
    addThing(Alias("int16", "word"), None)
    addThing(BasicPlainType("int24", 3), None)
    addThing(Alias("farword", "int24", deprecated = true), None)
    addThing(BasicPlainType("int32", 4), None)
    addThing(Alias("long", "int32"), None)
    addThing(BasicPlainType("int40", 5), None)
    addThing(BasicPlainType("int48", 6), None)
    addThing(BasicPlainType("int56", 7), None)
    addThing(BasicPlainType("int64", 8), None)
    addThing(BasicPlainType("int72", 9), None)
    addThing(BasicPlainType("int80", 10), None)
    addThing(BasicPlainType("int88", 11), None)
    addThing(BasicPlainType("int96", 12), None)
    addThing(BasicPlainType("int104", 13), None)
    addThing(BasicPlainType("int112", 14), None)
    addThing(BasicPlainType("int120", 15), None)
    addThing(BasicPlainType("int128", 16), None)
    val p = DerivedPlainType("pointer", w, isSigned = false, isPointy = true)
    addThing(p, None)
    //    addThing(DerivedPlainType("farpointer", get[PlainType]("farword"), isSigned = false), None)
    addThing(DerivedPlainType("ubyte", b, isSigned = false, isPointy = false), None)
    addThing(DerivedPlainType("sbyte", b, isSigned = true, isPointy = false), None)
    addThing(Alias("unsigned8", "ubyte"), None)
    addThing(Alias("signed8", "sbyte"), None)
    val trueType = ConstantBooleanType("true$", value = true)
    val falseType = ConstantBooleanType("false$", value = false)
    addThing(trueType, None)
    addThing(falseType, None)
    addThing(ConstantThing("true", NumericConstant(0, 0), trueType), None)
    addThing(ConstantThing("false", NumericConstant(0, 0), falseType), None)
    addThing(ConstantThing("__zeropage_usage", UnexpandedConstant("__zeropage_usage", 1), b), None)
    addThing(ConstantThing("__heap_start", UnexpandedConstant("__heap_start", 1), b), None)
    addThing(ConstantThing("$0000", NumericConstant(0, 2), p), None)
    addThing(FlagBooleanType("set_carry",
      BranchingOpcodeMapping(Opcode.BCS, IfFlagSet(ZFlag.C)),
      BranchingOpcodeMapping(Opcode.BCC, IfFlagClear(ZFlag.C))),
      None)
    addThing(FlagBooleanType("clear_carry",
      BranchingOpcodeMapping(Opcode.BCC, IfFlagClear(ZFlag.C)),
      BranchingOpcodeMapping(Opcode.BCS, IfFlagSet(ZFlag.C))),
      None)
    addThing(FlagBooleanType("set_overflow",
      BranchingOpcodeMapping(Opcode.BVS, IfFlagSet(ZFlag.P)),
      BranchingOpcodeMapping(Opcode.BVC, IfFlagClear(ZFlag.P))),
      None)
    addThing(FlagBooleanType("clear_overflow",
      BranchingOpcodeMapping(Opcode.BVC, IfFlagClear(ZFlag.P)),
      BranchingOpcodeMapping(Opcode.BVS, IfFlagSet(ZFlag.P))),
      None)
    addThing(FlagBooleanType("set_zero",
      BranchingOpcodeMapping(Opcode.BEQ, IfFlagSet(ZFlag.Z)),
      BranchingOpcodeMapping(Opcode.BNE, IfFlagClear(ZFlag.Z))),
      None)
    addThing(FlagBooleanType("clear_zero",
      BranchingOpcodeMapping(Opcode.BNE, IfFlagClear(ZFlag.Z)),
      BranchingOpcodeMapping(Opcode.BEQ, IfFlagSet(ZFlag.Z))),
      None)
    addThing(FlagBooleanType("set_negative",
      BranchingOpcodeMapping(Opcode.BMI, IfFlagSet(ZFlag.S)),
      BranchingOpcodeMapping(Opcode.BPL, IfFlagClear(ZFlag.S))),
      None)
    addThing(FlagBooleanType("clear_negative",
      BranchingOpcodeMapping(Opcode.BPL, IfFlagClear(ZFlag.S)),
      BranchingOpcodeMapping(Opcode.BMI, IfFlagSet(ZFlag.S))),
      None)
  }

  def assertNotDefined(name: String, position: Option[Position]): Unit = {
    if (things.contains(name) || parent.exists(_.things.contains(name)))
      log.fatal(s"`$name` is already defined", position)
  }

  def registerType(stmt: TypeDefinitionStatement): Unit = {
    //    addThing(DerivedPlainType(stmt.name, get(stmt.parent)))
    ???
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case None :: _ => None
    case Some(r) :: t => sequence(t) map (r :: _)
  }

  def evalVariableAndConstantSubParts(e: Expression): (Option[Expression], Constant) =
    // TODO: prevent accidental negative indexing more robustly
    e match {
      case SumExpression(params, false) =>
        val (constants, variables) = params.map { case (sign, expr) => (sign, expr, eval(expr)) }.partition(_._3.isDefined)
        val constant = eval(SumExpression(constants.map(x => (x._1, x._2)), decimal = false)).get
        val variable = variables match {
          case Nil => None
          case List((false, x, _)) => Some(x)
          case _ => Some(SumExpression(variables.map(x => (x._1, x._2)), decimal = false))
        }
        variable match {
          case None => variable -> constant
          case Some(x@VariableExpression(v)) =>
            if (get[Variable](v).typ.isSigned) Some(FunctionCallExpression("^", List(x, LiteralExpression(0x80, 1)))) -> (constant - 128).quickSimplify
            else variable -> constant
          case Some(IndexedExpression(_, _)) => variable -> constant
          case Some(LiteralExpression(_, _)) => variable -> constant
          case Some(GeneratedConstantExpression(_, _)) => variable -> constant
          case Some(SumExpression(List(negative@(true, _)), false)) =>
            Some(SumExpression(List(false -> LiteralExpression(0xff, 1), negative), decimal = false)) -> (constant - 255).quickSimplify
          case Some(FunctionCallExpression(
          "<<" | ">>" |
          "<<'" | ">>'" |
          "&" | "|" | "^" |
          ">>>>" |
          "*" | "*'", _)) => variable -> constant
          case Some(FunctionCallExpression(fname, _)) =>
            maybeGet[Thing](fname) match {
              case Some(ff: MangledFunction) =>
                if (ff.returnType.isSigned) Some(e) -> Constant.Zero
                else variable -> constant
              case Some(t: Type) =>
                if (t.isSigned) Some(e) -> Constant.Zero
                else variable -> constant
              case _ =>
                // dunno what to do
                Some(e) -> Constant.Zero
            }
          case _ =>
            // dunno what to do
            Some(e) -> Constant.Zero
        }
      case _ => eval(e) match {
        case Some(c) => None -> c
        case None => Some(e) -> Constant.Zero
      }
    }

  def evalSizeof(expr: Expression): Constant = {
    val size: Int = expr match {
      case VariableExpression(name) =>
        maybeGet[Thing](name) match {
          case None =>
            log.error(s"`$name` is not defined")
            1
          case Some(thing) => thing match {
            case t: Type => t.size
            case v: Variable => v.typ.size
            case a: InitializedArray => a.elementType.size * a.contents.length
            case a: UninitializedArray => a.sizeInBytes
            case x =>
              log.error("Invalid parameter for expr: " + name)
              1
          }
        }
      case _ =>
        AbstractExpressionCompiler.getExpressionType(this, log, expr).size
    }
    NumericConstant(size, Constant.minimumSize(size))
  }

  def eval(e: Expression, vars: Map[String, Constant]): Option[Constant] = evalImpl(e, Some(vars))

  def eval(e: Expression): Option[Constant] = evalImpl(e, None)

  //noinspection ScalaUnnecessaryParentheses,ZeroIndexToHead
  private def evalImpl(e: Expression, vv: Option[Map[String, Constant]]): Option[Constant] = {
    e match {
      case LiteralExpression(value, size) => Some(NumericConstant(value, size))
      case ConstantArrayElementExpression(c) => Some(c)
      case GeneratedConstantExpression(c, t) => Some(c)
      case VariableExpression(name) =>
        vv match {
          case Some(m) if m.contains(name) => Some(m(name))
          case _ => maybeGet[ConstantThing](name).map(_.value)
        }
      case IndexedExpression(_, _) => None
      case HalfWordExpression(param, hi) => evalImpl(e, vv).map(c => if (hi) c.hiByte else c.loByte)
      case SumExpression(params, decimal) =>
        params.map {
          case (minus, param) => (minus, evalImpl(param, vv))
        }.foldLeft(Some(Constant.Zero).asInstanceOf[Option[Constant]]) { (oc, pair) =>
          oc.flatMap { c =>
            pair match {
              case (_, None) => None
              case (minus, Some(addend)) =>
                val op = if (decimal) {
                  if (minus) MathOperator.DecimalMinus else MathOperator.DecimalPlus
                } else {
                  if (minus) MathOperator.Minus else MathOperator.Plus
                }
                Some(CompoundConstant(op, c, addend))
            }
          }
        }
      case SeparateBytesExpression(h, l) => for {
        lc <- evalImpl(l, vv)
        hc <- evalImpl(h, vv)
      } yield hc.asl(8) + lc
      case FunctionCallExpression(name, params) =>
        name match {
          case "sizeof" =>
            if (params.size == 1) {
              Some(evalSizeof(params.head))
            } else {
              log.error("Invalid number of parameters for `sizeof`", e.position)
              Some(Constant.One)
            }
          case "hi" =>
            if (params.size == 1) {
              eval(params.head).map(_.hiByte.quickSimplify)
            } else {
              log.error("Invalid number of parameters for `hi`", e.position)
              None
            }
          case "lo" =>
            if (params.size == 1) {
              eval(params.head).map(_.loByte.quickSimplify)
            } else {
              log.error("Invalid number of parameters for `lo`", e.position)
              None
            }
          case "sin" =>
            if (params.size == 2) {
              (eval(params(0)) -> eval(params(1))) match {
                case (Some(NumericConstant(angle, _)), Some(NumericConstant(scale, _))) =>
                  val value = (scale * math.sin(angle * math.Pi / 128)).round.toInt
                  Some(Constant(value))
                case _ => None
              }
            } else {
              log.error("Invalid number of parameters for `sin`", e.position)
              None
            }
          case "cos" =>
            if (params.size == 2) {
              (eval(params(0)) -> eval(params(1))) match {
                case (Some(NumericConstant(angle, _)), Some(NumericConstant(scale, _))) =>
                  val value = (scale * math.cos(angle * math.Pi / 128)).round.toInt
                  Some(Constant(value))
                case _ => None
              }
            } else {
              log.error("Invalid number of parameters for `cos`", e.position)
              None
            }
          case "tan" =>
            if (params.size == 2) {
              (eval(params(0)) -> eval(params(1))) match {
                case (Some(NumericConstant(angle, _)), Some(NumericConstant(scale, _))) =>
                  val value = (scale * math.tan(angle * math.Pi / 128)).round.toInt
                  Some(Constant(value))
                case _ => None
              }
            } else {
              log.error("Invalid number of parameters for `tan`", e.position)
              None
            }
          case "nonet" =>
            params match {
              case List(FunctionCallExpression("<<", ps@List(_, _))) =>
                constantOperation(MathOperator.Shl9, ps)
              case List(FunctionCallExpression("<<'", ps@List(_, _))) =>
                constantOperation(MathOperator.DecimalShl9, ps)
              case List(SumExpression(ps@List((false,_),(false,_)), false)) =>
                constantOperation(MathOperator.Plus9, ps.map(_._2))
              case List(SumExpression(ps@List((false,_),(false,_)), true)) =>
                constantOperation(MathOperator.DecimalPlus9, ps.map(_._2))
              case List(_) =>
                None
              case _ =>
                log.error("Invalid number of parameters for `nonet`", e.position)
                None
            }
          case ">>'" =>
            constantOperation(MathOperator.DecimalShr, params)
          case "<<'" =>
            constantOperation(MathOperator.DecimalShl, params)
          case ">>" =>
            constantOperation(MathOperator.Shr, params)
          case "<<" =>
            constantOperation(MathOperator.Shl, params)
          case ">>>>" =>
            constantOperation(MathOperator.Shr9, params)
          case "*'" =>
            constantOperation(MathOperator.DecimalTimes, params)
          case "*" =>
            constantOperation(MathOperator.Times, params)
          case "&&" | "&" =>
            constantOperation(MathOperator.And, params)
          case "^" =>
            constantOperation(MathOperator.Exor, params)
          case "||" | "|" =>
            constantOperation(MathOperator.Or, params)
          case _ =>
            if (params.size == 1) {
              return maybeGet[Type](name).flatMap(_ => eval(params.head))
            }
            None
        }
    }
  }.map(_.quickSimplify)

  private def constantOperation(op: MathOperator.Value, params: List[Expression]) = {
    params.map(eval).reduceLeft[Option[Constant]] { (oc, om) =>
      for {
        c <- oc
        m <- om
      } yield CompoundConstant(op, c, m)
    }
  }

  def evalForAsm(e: Expression): Option[Constant] = {
    e match {
      case LiteralExpression(value, size) => Some(NumericConstant(value, size))
      case ConstantArrayElementExpression(c) => Some(c)
      case VariableExpression(name) =>
        maybeGet[ConstantThing](name).map(_.value).orElse(maybeGet[ThingInMemory](name).map(_.toAddress))
      case IndexedExpression(name, index) => (evalForAsm(VariableExpression(name)), evalForAsm(index)) match {
        case (Some(a), Some(b)) => Some(CompoundConstant(MathOperator.Plus, a, b).quickSimplify)
      }
      case HalfWordExpression(param, hi) => evalForAsm(e).map(c => if (hi) c.hiByte else c.loByte)
      case SumExpression(params, decimal) =>
        params.map {
          case (minus, param) => (minus, evalForAsm(param))
        }.foldLeft(Some(Constant.Zero).asInstanceOf[Option[Constant]]) { (oc, pair) =>
          oc.flatMap { c =>
            pair match {
              case (_, None) => None
              case (minus, Some(addend)) =>
                val op = if (decimal) {
                  if (minus) MathOperator.DecimalMinus else MathOperator.DecimalPlus
                } else {
                  if (minus) MathOperator.Minus else MathOperator.Plus
                }
                Some(CompoundConstant(op, c, addend).quickSimplify)
            }
          }
        }
      case SeparateBytesExpression(h, l) => for {
        lc <- evalForAsm(l)
        hc <- evalForAsm(h)
      } yield hc.asl(8) + lc
      case FunctionCallExpression(name, params) =>
        name match {
          case "*" =>
            constantOperationForAsm(MathOperator.Times, params)
          case "&&" | "&" =>
            constantOperationForAsm(MathOperator.And, params)
          case "^" =>
            constantOperationForAsm(MathOperator.Exor, params)
          case "||" | "|" =>
            constantOperationForAsm(MathOperator.Or, params)
          case _ =>
            None
        }
    }
  }

  private def constantOperationForAsm(op: MathOperator.Value, params: List[Expression]) = {
    params.map(evalForAsm).reduceLeft[Option[Constant]] { (oc, om) =>
      for {
        c <- oc
        m <- om
      } yield CompoundConstant(op, c, m)
    }
  }

  def registerAlias(stmt: AliasDefinitionStatement): Unit = {
    addThing(Alias(stmt.name, stmt.target), stmt.position)
  }

  def registerEnum(stmt: EnumDefinitionStatement): Unit = {
    val count = if (stmt.variants.nonEmpty && stmt.variants.forall(_._2.isEmpty)) {
      val size = stmt.variants.size
      addThing(ConstantThing(stmt.name + ".count", NumericConstant(size, 1), get[Type]("byte")), stmt.position)
      Some(size)
    } else None
    val t = EnumType(stmt.name, count)
    addThing(t, stmt.position)
    var value = Constant.Zero
    for((name, optValue) <- stmt.variants) {
      optValue match {
        case Some(v) =>
          value = eval(v).getOrElse(errorConstant(s"Enum constant `${stmt.name}.$name` is not a constant", stmt.position))
        case _ =>
      }
      addThing(ConstantThing(name, value, t), stmt.position)
      value += 1
    }
  }

  def registerFunction(stmt: FunctionDeclarationStatement, options: CompilationOptions): Unit = {
    val w = get[Type]("word")
    val name = stmt.name
    val resultType = get[Type](stmt.resultType)

    if (stmt.reentrant && stmt.interrupt) log.error(s"Reentrant function `$name` cannot be an interrupt handler", stmt.position)
    if (stmt.reentrant && stmt.params.nonEmpty) log.error(s"Reentrant function `$name` cannot have parameters", stmt.position)
    if (stmt.interrupt && stmt.params.nonEmpty) log.error(s"Interrupt function `$name` cannot have parameters", stmt.position)
    if (stmt.isMacro) {
      if (!stmt.assembly) {
        if (resultType != VoidType) log.error(s"Macro non-assembly function `$name` must return void", stmt.position)
      }
      if (stmt.assembly && stmt.params.exists(_.assemblyParamPassingConvention.inNonInlinedOnly))
        log.error(s"Macro function `$name` cannot have by-variable parameters", stmt.position)
    } else {
      if (!stmt.assembly) {
        if (stmt.params.exists(!_.assemblyParamPassingConvention.isInstanceOf[ByVariable]))
          log.error(s"Non-assembly function `$name` cannot have non-variable parameters", stmt.position)
      }
      if (stmt.params.exists(_.assemblyParamPassingConvention.inInlinedOnly))
        log.error(s"Non-macro function `$name` cannot have inlinable parameters", stmt.position)
    }

    val env = new Environment(Some(this), name + "$", cpuFamily, jobContext)
    stmt.params.foreach(p => env.registerParameter(p, options))
    val params = if (stmt.assembly) {
      AssemblyParamSignature(stmt.params.map {
        pd =>
          val typ = env.get[Type](pd.typ)
          pd.assemblyParamPassingConvention match {
            case ByVariable(vn) =>
              AssemblyParam(typ, env.get[MemoryVariable](vn), AssemblyParameterPassingBehaviour.Copy)
            case ByMosRegister(reg) =>
              AssemblyParam(typ, RegisterVariable(reg, typ), AssemblyParameterPassingBehaviour.Copy)
            case ByZRegister(reg) =>
              AssemblyParam(typ, ZRegisterVariable(reg, typ), AssemblyParameterPassingBehaviour.Copy)
            case ByConstant(vn) =>
              AssemblyParam(typ, Placeholder(vn, typ), AssemblyParameterPassingBehaviour.ByConstant)
            case ByReference(vn) =>
              AssemblyParam(typ, Placeholder(vn, typ), AssemblyParameterPassingBehaviour.ByReference)
          }
      })
    } else {
      NormalParamSignature(stmt.params.map { pd =>
        env.get[MemoryVariable](pd.assemblyParamPassingConvention.asInstanceOf[ByVariable].name)
      })
    }
    if (resultType.size > Cpu.getMaxSizeReturnableViaRegisters(options.platform.cpu, options)) {
      registerVariable(VariableDeclarationStatement(stmt.name + ".return", stmt.resultType, None, global = true, stack = false, constant = false, volatile = false, register = false, None, None, None), options)
    }
    stmt.statements match {
      case None =>
        stmt.address match {
          case None =>
            log.error(s"Extern function `${stmt.name}`needs an address", stmt.position)
          case Some(a) =>
            val addr = eval(a).getOrElse(errorConstant(s"Address of `${stmt.name}` is not a constant", stmt.position))
            val mangled = ExternFunction(
              name,
              resultType,
              params,
              addr,
              env,
              stmt.bank
            )
            addThing(mangled, stmt.position)
            registerAddressConstant(mangled, stmt.position, options)
            addThing(ConstantThing(name + '`', addr, w), stmt.position)
        }

      case Some(statements) =>
        statements.foreach {
          case v: VariableDeclarationStatement => env.registerVariable(v, options)
          case _ => ()
        }
        val executableStatements = statements.flatMap {
          case e: ExecutableStatement => Some(e)
          case _ => None
        }
        val needsExtraRTS = !stmt.isMacro && !stmt.assembly && (statements.isEmpty || !statements.last.isInstanceOf[ReturnStatement])
        if (stmt.isMacro) {
          if (stmt.bank.isDefined) {
            log.error("Macro functions cannot be in a defined segment", stmt.position)
          }
          val mangled = MacroFunction(
            name,
            resultType,
            params,
            env,
            executableStatements
          )
          addThing(mangled, stmt.position)
        } else {
          val stackVariablesSize = env.things.values.map {
            case StackVariable(n, t, _) if !n.contains(".") => t.size
            case _ => 0
          }.sum
          val mangled = NormalFunction(
            name,
            resultType,
            params,
            env,
            stackVariablesSize,
            stmt.address.map(a => this.eval(a).getOrElse(errorConstant(s"Address of `${stmt.name}` is not a constant"))),
            executableStatements ++ (if (needsExtraRTS) List(ReturnStatement(None)) else Nil),
            interrupt = stmt.interrupt,
            kernalInterrupt = stmt.kernalInterrupt,
            reentrant = stmt.reentrant,
            position = stmt.position,
            declaredBank = stmt.bank,
            alignment = stmt.alignment.getOrElse(if (name == "main") NoAlignment else defaultFunctionAlignment(options, hot = true)) // TODO: decide actual hotness in a smarter way
          )
          addThing(mangled, stmt.position)
          registerAddressConstant(mangled, stmt.position, options)
        }
    }
  }

  private def registerAddressConstant(thing: ThingInMemory, position: Option[Position], options: CompilationOptions): Unit = {
    if (!thing.zeropage && options.flag(CompilationFlag.LUnixRelocatableCode)) {
      val b = get[Type]("byte")
      val w = get[Type]("word")
      val relocatable = UninitializedMemoryVariable(thing.name + ".addr", w, VariableAllocationMethod.Static, None, defaultVariableAlignment(options, 2), isVolatile = false)
      val addr = relocatable.toAddress
      addThing(relocatable, position)
      addThing(RelativeVariable(thing.name + ".addr.hi", addr + 1, b, zeropage = false, None, isVolatile = false), position)
      addThing(RelativeVariable(thing.name + ".addr.lo", addr, b, zeropage = false, None, isVolatile = false), position)
      val rawaddr = thing.toAddress
      addThing(ConstantThing(thing.name + ".rawaddr", rawaddr, get[Type]("pointer")), position)
      addThing(ConstantThing(thing.name + ".rawaddr.hi", rawaddr.hiByte, get[Type]("byte")), position)
      addThing(ConstantThing(thing.name + ".rawaddr.lo", rawaddr.loByte, get[Type]("byte")), position)
    } else {
      val addr = thing.toAddress
      addThing(ConstantThing(thing.name + ".addr", addr, get[Type]("pointer")), position)
      addThing(ConstantThing(thing.name + ".addr.hi", addr.hiByte, get[Type]("byte")), position)
      addThing(ConstantThing(thing.name + ".addr.lo", addr.loByte, get[Type]("byte")), position)
      addThing(ConstantThing(thing.name + ".rawaddr", addr, get[Type]("pointer")), position)
      addThing(ConstantThing(thing.name + ".rawaddr.hi", addr.hiByte, get[Type]("byte")), position)
      addThing(ConstantThing(thing.name + ".rawaddr.lo", addr.loByte, get[Type]("byte")), position)
    }
  }

  def registerParameter(stmt: ParameterDeclaration, options: CompilationOptions): Unit = {
    val typ = get[Type](stmt.typ)
    val b = get[Type]("byte")
    val w = get[Type]("word")
    val p = get[Type]("pointer")
    stmt.assemblyParamPassingConvention match {
      case ByVariable(name) =>
        val zp = typ.name == "pointer" // TODO
        val v = UninitializedMemoryVariable(prefix + name, typ, if (zp) VariableAllocationMethod.Zeropage else VariableAllocationMethod.Auto, None, defaultVariableAlignment(options, 2), isVolatile = false)
        addThing(v, stmt.position)
        registerAddressConstant(v, stmt.position, options)
        val addr = v.toAddress
        for((suffix, offset, t) <- getSubvariables(typ)) {
          addThing(RelativeVariable(v.name + suffix, addr + offset, t, zeropage = zp, None, isVolatile = v.isVolatile), stmt.position)
        }
      case ByMosRegister(_) => ()
      case ByZRegister(_) => ()
      case ByConstant(name) =>
        val v = ConstantThing(prefix + name, UnexpandedConstant(prefix + name, typ.size), typ)
        addThing(v, stmt.position)
      case ByReference(name) =>
        val addr = UnexpandedConstant(prefix + name, typ.size)
        val v = RelativeVariable(prefix + name, addr, p, zeropage = false, None, isVolatile = false)
        addThing(v, stmt.position)
        addThing(RelativeVariable(v.name + ".hi", addr + 1, b, zeropage = false, None, isVolatile = false), stmt.position)
        addThing(RelativeVariable(v.name + ".lo", addr, b, zeropage = false, None, isVolatile = false), stmt.position)
    }
  }

  def registerUnnamedArray(array: InitializedArray): Unit = {
    val b = get[Type]("byte")
    val p = get[Type]("pointer")
    if (!array.name.endsWith(".array")) ???
    val pointerName = array.name.stripSuffix(".array")
    addThing(ConstantThing(pointerName, array.toAddress, p), None)
    addThing(ConstantThing(pointerName + ".addr", array.toAddress, p), None)
    addThing(ConstantThing(pointerName + ".rawaddr", array.toAddress, p), None)
    addThing(array, None)
  }

  def extractArrayContents(contents1: ArrayContents): List[Expression] = contents1 match {
    case LiteralContents(xs) => xs
    case CombinedContents(xs) => xs.flatMap(extractArrayContents)
    case pc@ProcessedContents(f, xs) => pc.getAllExpressions
    case ForLoopContents(v, start, end, direction, body) =>
      (eval(start), eval(end)) match {
        case (Some(NumericConstant(s, sz1)), Some(NumericConstant(e, sz2))) =>
          val size = sz1 max sz2
          val range = (direction match {
            case ForDirection.To | ForDirection.ParallelTo => s.to(e)
            case ForDirection.Until | ForDirection.ParallelUntil => s.until(e)
            case ForDirection.DownTo => s.to(e, -1)
          }).toList
          range.flatMap(i => extractArrayContents(body).map(_.replaceVariable(v, LiteralExpression(i, size))))
        case (Some(_), Some(_)) =>
          log.error("Array range bounds cannot be evaluated")
          Nil
        case _ =>
          log.error("Non-constant array range bounds")
          Nil

      }
  }

  private def defaultArrayAlignment(options: CompilationOptions, size: Long): MemoryAlignment = {
    if (options.flag(CompilationFlag.OptimizeForSpeed) && size <= 256 && size != 0) WithinPageAlignment
    else NoAlignment
  }

  private def defaultVariableAlignment(options: CompilationOptions, size: Long): MemoryAlignment = {
    if (options.flag(CompilationFlag.PreventJmpIndirectBug) && size == 2) WithinPageAlignment
    else NoAlignment
  }

  private def defaultFunctionAlignment(options: CompilationOptions, hot: Boolean): MemoryAlignment = {
    // TODO:
    if (hot && options.platform.cpuFamily == CpuFamily.M6502 &&
          options.flag(CompilationFlag.OptimizeForSonicSpeed)) WithinPageAlignment
    else NoAlignment
  }

  def registerArray(stmt: ArrayDeclarationStatement, options: CompilationOptions): Unit = {
    val b = get[VariableType]("byte")
    val w = get[VariableType]("word")
    val p = get[Type]("pointer")
    stmt.elements match {
      case None =>
        stmt.length match {
          case None => log.error(s"Array `${stmt.name}` without size nor contents", stmt.position)
          case Some(l) =>
            // array arr[...]
            val address = stmt.address.map(a => eval(a).getOrElse(log.fatal(s"Array `${stmt.name}` has non-constant address", stmt.position)))
            val (indexType, lengthConst) = l match {
              case VariableExpression(name) =>
                maybeGet[Type](name) match {
                  case Some(typ@EnumType(_, Some(count))) =>
                    typ -> NumericConstant(count, 1)
                  case Some(typ) =>
                    log.error(s"Type $name cannot be used as an array index", l.position)
                    w -> Constant.Zero
                  case _ => w -> eval(l).getOrElse(errorConstant(s"Array `${stmt.name}` has non-constant length", stmt.position))
                }
              case _ =>
                w -> eval(l).getOrElse(errorConstant(s"Array `${stmt.name}` has non-constant length", stmt.position))
            }
            lengthConst match {
              case NumericConstant(length, _) =>
                if (length > 0xffff || length < 0) log.error(s"Array `${stmt.name}` has invalid length", stmt.position)
                val alignment = stmt.alignment.getOrElse(defaultArrayAlignment(options, length))
                val array = address match {
                  case None => UninitializedArray(stmt.name + ".array", length.toInt,
                              declaredBank = stmt.bank, indexType, b, alignment)
                  case Some(aa) => RelativeArray(stmt.name + ".array", aa, length.toInt,
                              declaredBank = stmt.bank, indexType, b)
                }
                addThing(array, stmt.position)
                registerAddressConstant(UninitializedMemoryVariable(stmt.name, p, VariableAllocationMethod.None, stmt.bank, NoAlignment, isVolatile = false), stmt.position, options)
                val a = address match {
                  case None => array.toAddress
                  case Some(aa) => aa
                }
                addThing(RelativeVariable(stmt.name + ".first", a, b, zeropage = false,
                            declaredBank = stmt.bank, isVolatile = false), stmt.position)
                if (options.flag(CompilationFlag.LUnixRelocatableCode)) {
                  val b = get[Type]("byte")
                  val w = get[Type]("word")
                  val relocatable = UninitializedMemoryVariable(stmt.name, w, VariableAllocationMethod.Static, None, NoAlignment, isVolatile = false)
                  val addr = relocatable.toAddress
                  addThing(relocatable, stmt.position)
                  addThing(RelativeVariable(stmt.name + ".addr.hi", addr + 1, b, zeropage = false, None, isVolatile = false), stmt.position)
                  addThing(RelativeVariable(stmt.name + ".addr.lo", addr, b, zeropage = false, None, isVolatile = false), stmt.position)
                  addThing(RelativeVariable(stmt.name + ".array.hi", addr + 1, b, zeropage = false, None, isVolatile = false), stmt.position)
                  addThing(RelativeVariable(stmt.name + ".array.lo", addr, b, zeropage = false, None, isVolatile = false), stmt.position)
                } else {
                  addThing(ConstantThing(stmt.name, a, p), stmt.position)
                  addThing(ConstantThing(stmt.name + ".hi", a.hiByte.quickSimplify, b), stmt.position)
                  addThing(ConstantThing(stmt.name + ".lo", a.loByte.quickSimplify, b), stmt.position)
                  addThing(ConstantThing(stmt.name + ".array.hi", a.hiByte.quickSimplify, b), stmt.position)
                  addThing(ConstantThing(stmt.name + ".array.lo", a.loByte.quickSimplify, b), stmt.position)
                }
                if (length < 256) {
                  addThing(ConstantThing(stmt.name + ".length", lengthConst, b), stmt.position)
                }
              case _ => log.error(s"Array `${stmt.name}` has weird length", stmt.position)
            }
        }
      case Some(contents1) =>
        val contents = extractArrayContents(contents1)
        val indexType = stmt.length match {
          case None => // array arr = [...]
            w
          case Some(l) => // array arr[...] = [...]
            val (indexTyp, lengthConst) = l match {
              case VariableExpression(name) =>
                maybeGet[Type](name) match {
                  case Some(typ@EnumType(_, Some(count))) =>
                    if (count != contents.size)
                      log.error(s"Array `${stmt.name}` has actual length different than the number of variants in the enum `${typ.name}`", stmt.position)
                    typ -> NumericConstant(count, 1)
                  case Some(typ@EnumType(_, None)) =>
                    // using a non-enumerable enum for an array index is ok if the array is preïnitialized
                    typ -> NumericConstant(contents.length, 1)
                  case Some(_) =>
                    log.error(s"Type $name cannot be used as an array index", l.position)
                    w -> Constant.Zero
                  case _ =>
                    w -> eval(l).getOrElse(errorConstant(s"Array `${stmt.name}` has non-constant length", stmt.position))
                }
              case _ =>
                w -> eval(l).getOrElse(errorConstant(s"Array `${stmt.name}` has non-constant length", stmt.position))
            }
            lengthConst match {
              case NumericConstant(ll, _) =>
                if (ll != contents.length) log.error(s"Array `${stmt.name}` has different declared and actual length", stmt.position)
              case _ => log.error(s"Array `${stmt.name}` has weird length", stmt.position)
            }
            indexTyp
        }
        val length = contents.length
        if (length > 0xffff || length < 0) log.error(s"Array `${stmt.name}` has invalid length", stmt.position)
        val alignment = stmt.alignment.getOrElse(defaultArrayAlignment(options, length))
        val address = stmt.address.map(a => eval(a).getOrElse(errorConstant(s"Array `${stmt.name}` has non-constant address", stmt.position)))
        val array = InitializedArray(stmt.name + ".array", address, contents, declaredBank = stmt.bank, indexType, b, alignment)
        addThing(array, stmt.position)
        registerAddressConstant(UninitializedMemoryVariable(stmt.name, p, VariableAllocationMethod.None,
                    declaredBank = stmt.bank, alignment, isVolatile = false), stmt.position, options)
        val a = address match {
          case None => array.toAddress
          case Some(aa) => aa
        }
        addThing(RelativeVariable(stmt.name + ".first", a, b, zeropage = false,
                    declaredBank = stmt.bank, isVolatile = false), stmt.position)
        if (options.flag(CompilationFlag.LUnixRelocatableCode)) {
          val b = get[Type]("byte")
          val w = get[Type]("word")
          val relocatable = UninitializedMemoryVariable(stmt.name, w, VariableAllocationMethod.Static, None, NoAlignment, isVolatile = false)
          val addr = relocatable.toAddress
          addThing(relocatable, stmt.position)
          addThing(RelativeVariable(stmt.name + ".array.hi", addr + 1, b, zeropage = false, None, isVolatile = false), stmt.position)
          addThing(RelativeVariable(stmt.name + ".array.lo", addr, b, zeropage = false, None, isVolatile = false), stmt.position)
        } else {
          addThing(ConstantThing(stmt.name, a, p), stmt.position)
          addThing(ConstantThing(stmt.name + ".hi", a.hiByte.quickSimplify, b), stmt.position)
          addThing(ConstantThing(stmt.name + ".lo", a.loByte.quickSimplify, b), stmt.position)
          addThing(ConstantThing(stmt.name + ".array.hi", a.hiByte.quickSimplify, b), stmt.position)
          addThing(ConstantThing(stmt.name + ".array.lo", a.loByte.quickSimplify, b), stmt.position)
        }
        if (length < 256) {
          addThing(ConstantThing(stmt.name + ".length", NumericConstant(length, 1), b), stmt.position)
        }
    }
  }

  def registerVariable(stmt: VariableDeclarationStatement, options: CompilationOptions): Unit = {
    val name = stmt.name
    val position = stmt.position
    if (stmt.stack && parent.isEmpty) {
      if (stmt.stack && stmt.global) log.error(s"`$name` is static or global and cannot be on stack", position)
    }
    val b = get[Type]("byte")
    val w = get[Type]("word")
    val typ = get[VariableType](stmt.typ)
    if (stmt.typ == "pointer" || stmt.typ == "farpointer") {
      //      if (stmt.constant) {
      //        log.error(s"Pointer `${stmt.name}` cannot be constant")
      //      }
      stmt.address.flatMap(eval) match {
        case Some(NumericConstant(a, _)) =>
          if ((a & 0xff00) != 0)
            log.error(s"Pointer `${stmt.name}` cannot be located outside the zero page")
        case _ => ()
      }
    }
    val alignment = stmt.alignment.getOrElse(defaultVariableAlignment(options, typ.size))
    if (stmt.constant) {
      if (stmt.stack) log.error(s"`$name` is a constant and cannot be on stack", position)
      if (stmt.register) log.error(s"`$name` is a constant and cannot be in a register", position)
      if (stmt.address.isDefined) log.error(s"`$name` is a constant and cannot have an address", position)
      if (stmt.initialValue.isEmpty) log.error(s"`$name` is a constant and requires a value", position)
      val constantValue: Constant = stmt.initialValue.flatMap(eval).getOrElse(errorConstant(s"`$name` has a non-constant value", position))
      if (constantValue.requiredSize > typ.size) log.error(s"`$name` is has an invalid value: not in the range of `$typ`", position)
      addThing(ConstantThing(prefix + name, constantValue, typ), stmt.position)
      for((suffix, offset, t) <- getSubvariables(typ)) {
        addThing(ConstantThing(prefix + name + suffix, constantValue.subconstant(offset, t.size), t), stmt.position)
      }
    } else {
      if (stmt.stack && stmt.global) log.error(s"`$name` is static or global and cannot be on stack", position)
      if (stmt.register && typ.size != 1) log.error(s"A register variable `$name` is too large", position)
      if (stmt.register && stmt.global) log.error(s"`$name` is static or global and cannot be in a register", position)
      if (stmt.register && stmt.stack) log.error(s"`$name` cannot be simultaneously on stack and in a register", position)
      if (stmt.volatile && stmt.stack) log.error(s"`$name` cannot be simultaneously on stack and volatile", position)
      if (stmt.volatile && stmt.register) log.error(s"`$name` cannot be simultaneously volatile and in a register", position)
      if (stmt.initialValue.isDefined && parent.isDefined) log.error(s"`$name` is local and not a constant and therefore cannot have a value", position)
      if (stmt.initialValue.isDefined && stmt.address.isDefined) log.warn(s"`$name` has both address and initial value - this may not work as expected!", position)
      if (stmt.register && stmt.address.isDefined) log.error(s"`$name` cannot by simultaneously at an address and in a register", position)
      if (stmt.stack) {
        val v = StackVariable(prefix + name, typ, this.baseStackOffset)
        addThing(v, stmt.position)
        for((suffix, offset, t) <- getSubvariables(typ)) {
          addThing(StackVariable(prefix + name + suffix, t, baseStackOffset + offset), stmt.position)
        }
        baseStackOffset += typ.size
      } else {
        val (v, addr) = stmt.address.fold[(VariableInMemory, Constant)]({
          val alloc =
            if (typ.name == "pointer" || typ.name == "__reg$type") VariableAllocationMethod.Zeropage
            else if (stmt.global) VariableAllocationMethod.Static
            else if (stmt.register) VariableAllocationMethod.Register
            else VariableAllocationMethod.Auto
          if (alloc != VariableAllocationMethod.Static && stmt.initialValue.isDefined) {
            log.error(s"`$name` cannot be preinitialized`", position)
          }
          val v = stmt.initialValue.fold[MemoryVariable](UninitializedMemoryVariable(prefix + name, typ, alloc,
                      declaredBank = stmt.bank, alignment, isVolatile = stmt.volatile)){ive =>
            if (options.flags(CompilationFlag.ReadOnlyArrays)) {
              log.warn("Initialized variable in read-only segment", position)
            }
            InitializedMemoryVariable(name, None, typ, ive, declaredBank = stmt.bank, alignment, isVolatile = stmt.volatile)
          }
          registerAddressConstant(v, stmt.position, options)
          (v, v.toAddress)
        })(a => {
          val addr = eval(a).getOrElse(errorConstant(s"Address of `$name` has a non-constant value", position))
          val zp = addr match {
            case NumericConstant(n, _) => n < 0x100
            case _ => false
          }
          val v = RelativeVariable(prefix + name, addr, typ, zeropage = zp,
                      declaredBank = stmt.bank, isVolatile = stmt.volatile)
          registerAddressConstant(v, stmt.position, options)
          (v, addr)
        })
        addThing(v, stmt.position)
        if (!v.isInstanceOf[MemoryVariable]) {
          addThing(ConstantThing(v.name + "`", addr, b), stmt.position)
        }
        for((suffix, offset, t) <- getSubvariables(typ)) {
          addThing(RelativeVariable(prefix + name + suffix, addr + offset, t, zeropage = v.zeropage, declaredBank = stmt.bank, isVolatile = v.isVolatile), stmt.position)
        }
      }
    }
  }

  def getSubvariables(typ: Type): List[(String, Int, VariableType)] = {
    val b = get[VariableType]("byte")
    val w = get[VariableType]("word")
    if (typ.name == "__reg$type") {
      return (".lo", 0, b) ::
        (".hi", 1, b) ::
        (".loword", 0, w) ::
        (".b2b3", 2, w) ::
        List.tabulate(typ.size) { i => (".b" + i, i, b) }
    }
    typ match {
      case _: PlainType => typ.size match {
        case 2 => List(
          (".lo", 0, b),
          (".hi", 1, b))
        case 3 => List(
          (".loword", 0, w),
          (".hiword", 1, w),
          (".b0", 0, b),
          (".b1", 1, b),
          (".b2", 2, b))
        case 4 => List(
          (".loword", 0, w),
          (".hiword", 2, w),
          (".b0", 0, b),
          (".b1", 1, b),
          (".b2", 2, b),
          (".b3", 3, b))
        case sz if sz > 4 => List.tabulate(sz){ i => (".b" + i, i, b) }
        case _ => Nil
      }
      case _ => Nil
    }
  }

  def lookup[T <: Thing : Manifest](name: String): Option[T] = {
    if (things.contains(name)) {
      maybeGet(name)
    } else {
      parent.flatMap(_.lookup[T](name))
    }
  }

  def lookupFunction(name: String, actualParams: List[(Type, Expression)]): Option[MangledFunction] = {
    if (things.contains(name)) {
      val function = get[MangledFunction](name)
      if (function.params.length != actualParams.length) {
        log.error(s"Invalid number of parameters for function `$name`", actualParams.headOption.flatMap(_._2.position))
      }
      function.params match {
        case NormalParamSignature(params) =>
          function.params.types.zip(actualParams).zip(params).foreach { case ((required, (actual, expr)), m) =>
            if (!actual.isAssignableTo(required)) {
              log.error(s"Invalid value for parameter `${m.name}` of function `$name`", expr.position)
            }
          }
        case AssemblyParamSignature(params) =>
          function.params.types.zip(actualParams).zipWithIndex.foreach { case ((required, (actual, expr)), ix) =>
            if (!actual.isAssignableTo(required)) {
              log.error(s"Invalid value for parameter ${ix + 1} of function `$name`", expr.position)
            }
          }
      }
      Some(function)
    } else {
      parent.flatMap(_.lookupFunction(name, actualParams))
    }
  }

  def collectDeclarations(program: Program, options: CompilationOptions): Unit = {
    val b = get[VariableType]("byte")
    if (options.flag(CompilationFlag.OptimizeForSonicSpeed)) {
      addThing(InitializedArray("identity$", None, List.tabulate(256)(n => LiteralExpression(n, 1)), declaredBank = None, b, b, defaultArrayAlignment(options, 256)), None)
    }
    program.declarations.foreach {
      case a: AliasDefinitionStatement => registerAlias(a)
      case _ =>
    }
    program.declarations.foreach {
      case e: EnumDefinitionStatement => registerEnum(e)
      case _ =>
    }
    program.declarations.foreach {
      case f: FunctionDeclarationStatement => registerFunction(f, options)
      case v: VariableDeclarationStatement => registerVariable(v, options)
      case a: ArrayDeclarationStatement => registerArray(a, options)
      case _ =>
    }
    if (options.zpRegisterSize > 0 && !things.contains("__reg")) {
      addThing(BasicPlainType("__reg$type", options.zpRegisterSize), None)
      registerVariable(VariableDeclarationStatement(
        name = "__reg",
        bank = None,
        typ = "__reg$type",
        global = true,
        stack = false,
        constant = false,
        volatile = false,
        register = false,
        initialValue = None,
        address = None,
        alignment = None), options)
    }
    if (CpuFamily.forType(options.platform.cpu) == CpuFamily.M6502) {
      if (!things.contains("__constant8")) {
        things("__constant8") = InitializedArray("__constant8", None, List(LiteralExpression(8, 1)), declaredBank = None, b, b, NoAlignment)
      }
      if (options.flag(CompilationFlag.SoftwareStack)) {
        if (!things.contains("__sp")) {
          things("__sp") = UninitializedMemoryVariable("__sp", b, VariableAllocationMethod.Auto, None, NoAlignment, isVolatile = false)
          things("__stack") = UninitializedArray("__stack", 256, None, b, b, WithinPageAlignment)
        }
      }
    }
  }

  private def checkName[T <: Thing : Manifest](objType: String, name: String, pos: Option[Position]): Unit = {
    if (maybeGet[T](name).isEmpty) {
      log.error(s"$objType `$name` is not defined", pos)
    }
  }

  def nameCheck(nodes: List[_ <:Node]): Unit = nodes.foreach(nameCheck)

  def nameCheck(node: Node): Unit = node match {
    case _:MosAssemblyStatement => ()
    case _:Z80AssemblyStatement => ()
    case _:DeclarationStatement => ()
    case s:ForStatement =>
      checkName[Variable]("Variable", s.variable, s.position)
      nameCheck(s.start)
      nameCheck(s.end)
      nameCheck(s.body)
    case s:IfStatement =>
      nameCheck(s.condition)
      nameCheck(s.thenBranch)
      nameCheck(s.elseBranch)
    case s:WhileStatement =>
      nameCheck(s.condition)
      nameCheck(s.body)
    case s:DoWhileStatement =>
      nameCheck(s.body)
      nameCheck(s.condition)
    case s:Statement => nameCheck(s.getAllExpressions)
    case BlackHoleExpression => ()
    case _:BooleanLiteralExpression => ()
    case _:LiteralExpression => ()
    case _:GeneratedConstantExpression => ()
    case _:TextLiteralExpression => ()
    case VariableExpression(name) =>
      checkName[VariableLikeThing]("Variable or constant", name, node.position)
    case IndexedExpression(name, index) =>
      checkName[IndexableThing]("Array or pointer", name, node.position)
      nameCheck(index)
    case SeparateBytesExpression(h, l) =>
      nameCheck(h)
      nameCheck(l)
    case SumExpression(params, _) =>
      nameCheck(params.map(_._2))
    case FunctionCallExpression("sizeof", List(ve@VariableExpression(e))) =>
      checkName[Thing]("Type, variable or constant", e, ve.position)
    case FunctionCallExpression(name, params) =>
      if (name.exists(_.isLetter) && !Environment.predefinedFunctions(name)) {
        checkName[CallableThing]("Function or type", name, node.position)
      }
      nameCheck(params)
  }
}

object Environment {
  val predefinedFunctions = Set("not", "hi", "lo", "nonet", "sizeof")
}
