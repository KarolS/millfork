package millfork.env

import java.util.concurrent.atomic.AtomicLong

import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly.{Opcode, OpcodeClasses}
import millfork.compiler._
import millfork.error.ErrorReporting
import millfork.node._
import millfork.output.{CompiledMemory, MemoryBank, VariableAllocator}

import scala.collection.mutable


/**
  * @author Karol Stasiak
  */
//noinspection NotImplementedCode
class Environment(val parent: Option[Environment], val prefix: String) {


  private var baseStackOffset = 0x101
  private val relVarId = new AtomicLong

  def genRelativeVariable(constant: Constant, typ: Type, zeropage: Boolean): RelativeVariable = {
    val variable = RelativeVariable(".rv__" + relVarId.incrementAndGet().formatted("%06d"), constant, typ, zeropage = zeropage, declaredBank = None /*TODO*/)
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
    val e = new Environment(None, "")
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
      case RelativeArray(_, NumericConstant(addr, _), size, declaredBank) =>
        List((declaredBank.getOrElse("default"), addr.toInt, size))
      case RelativeVariable(_, NumericConstant(addr, _), typ, _, declaredBank) =>
        List((declaredBank.getOrElse("default"), addr.toInt, typ.size))
      case f: NormalFunction =>
        f.environment.getAllFixedAddressObjects
      case _ => Nil
    }.toList
  }

  def allocateVariables(nf: Option[NormalFunction], mem: CompiledMemory, callGraph: CallGraph, allocators: Map[String, VariableAllocator], options: CompilationOptions, onEachVariable: (String, Int) => Unit): Unit = {
    val b = get[Type]("byte")
    val p = get[Type]("pointer")
    val params = nf.fold(List[String]()) { f =>
      f.params match {
        case NormalParamSignature(ps) =>
          ps.map(p => p.name)
        case _ =>
          Nil
      }
    }.toSet
    val toAdd = things.values.flatMap {
      case m: UninitializedMemory =>
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
            m.sizeInBytes match {
              case 2 =>
                val addr =
                  allocators(bank).allocatePointer(mem.banks(bank), callGraph, vertex)
                onEachVariable(m.name, addr)
                List(
                  ConstantThing(m.name.stripPrefix(prefix) + "`", NumericConstant(addr, 2), p)
                )
            }
          case VariableAllocationMethod.Auto | VariableAllocationMethod.Register | VariableAllocationMethod.Static =>
            if (m.alloc == VariableAllocationMethod.Register) {
              ErrorReporting.warn(s"Failed to inline variable `${m.name}` into a register", options, None)
            }
            m.sizeInBytes match {
              case 0 => Nil
              case 2 =>
                val addr =
                  allocators(bank).allocateBytes(mem.banks(bank), callGraph, vertex, options, 2, initialized = false, writeable = true)
                onEachVariable(m.name, addr)
                List(
                  ConstantThing(m.name.stripPrefix(prefix) + "`", NumericConstant(addr, 2), p)
                )
              case count =>
                val addr = allocators(bank).allocateBytes(mem.banks(bank), callGraph, vertex, options, count, initialized = false, writeable = true)
                onEachVariable(m.name, addr)
                List(
                  ConstantThing(m.name.stripPrefix(prefix) + "`", NumericConstant(addr, 2), p)
                )
            }
        }
      case f: NormalFunction =>
        f.environment.allocateVariables(Some(f), mem, callGraph, allocators, options, onEachVariable)
        Nil
      case _ => Nil
    }.toList
    val tagged: List[(String, Thing)] = toAdd.map(x => x.name -> x)
    things ++= tagged
  }

  val things: mutable.Map[String, Thing] = mutable.Map()

  private def addThing(t: Thing, position: Option[Position]): Unit = {
    assertNotDefined(t.name, position)
    things(t.name.stripPrefix(prefix)) = t
  }

  def removeVariable(str: String): Unit = {
    things -= str
    things -= str + ".addr"
  }

  def get[T <: Thing : Manifest](name: String, position: Option[Position] = None): T = {
    val clazz = implicitly[Manifest[T]].runtimeClass
    if (things.contains(name)) {
      val t: Thing = things(name)
      if ((t ne null) && clazz.isInstance(t)) {
        t.asInstanceOf[T]
      } else {
        ErrorReporting.fatal(s"`$name` is not a ${clazz.getSimpleName}", position)
      }
    } else parent.fold {
      ErrorReporting.fatal(s"${clazz.getSimpleName} `$name` is not defined", position)
    } {
      _.get[T](name, position)
    }
  }

  def maybeGet[T <: Thing : Manifest](name: String): Option[T] = {
    if (things.contains(name)) {
      val t: Thing = things(name)
      val clazz = implicitly[Manifest[T]].runtimeClass
      if ((t ne null) && clazz.isInstance(t)) {
        Some(t.asInstanceOf[T])
      } else {
        None
      }
    } else parent.flatMap {
      _.maybeGet[T](name)
    }
  }

  def getArrayOrPointer(arrayName: String): Thing = {
    maybeGet[ThingInMemory](arrayName).
      orElse(maybeGet[ThingInMemory](arrayName + ".array")).
      orElse(maybeGet[ConstantThing](arrayName)).
      getOrElse(ErrorReporting.fatal(s"`$arrayName` is not an array or a pointer"))
  }

  def getPointy(name: String): Pointy = {
    InitializedMemoryVariable
    UninitializedMemoryVariable
    getArrayOrPointer(name) match {
      case th@InitializedArray(_, _, cs, _) => ConstantPointy(th.toAddress, Some(cs.length))
      case th@UninitializedArray(_, size, _) => ConstantPointy(th.toAddress, Some(size))
      case th@RelativeArray(_, _, size, _) => ConstantPointy(th.toAddress, Some(size))
      case ConstantThing(_, value, typ) if typ.size <= 2 => ConstantPointy(value, None)
      case th:VariableInMemory => VariablePointy(th.toAddress)
      case _ =>
        ErrorReporting.error(s"$name is not a valid pointer or array")
        ConstantPointy(Constant.Zero, None)
    }
  }

  if (parent.isEmpty) {
    addThing(VoidType, None)
    addThing(BuiltInBooleanType, None)
    addThing(BasicPlainType("byte", 1), None)
    addThing(BasicPlainType("word", 2), None)
    addThing(BasicPlainType("long", 4), None)
    addThing(DerivedPlainType("pointer", get[PlainType]("word"), isSigned = false), None)
    addThing(DerivedPlainType("ubyte", get[PlainType]("byte"), isSigned = false), None)
    addThing(DerivedPlainType("sbyte", get[PlainType]("byte"), isSigned = true), None)
    addThing(DerivedPlainType("cent", get[PlainType]("byte"), isSigned = false), None)
    val trueType = ConstantBooleanType("true$", value = true)
    val falseType = ConstantBooleanType("false$", value = false)
    addThing(trueType, None)
    addThing(falseType, None)
    addThing(ConstantThing("true", NumericConstant(0, 0), trueType), None)
    addThing(ConstantThing("false", NumericConstant(0, 0), falseType), None)
    addThing(FlagBooleanType("set_carry", Opcode.BCS, Opcode.BCC), None)
    addThing(FlagBooleanType("clear_carry", Opcode.BCC, Opcode.BCS), None)
    addThing(FlagBooleanType("set_overflow", Opcode.BVS, Opcode.BVC), None)
    addThing(FlagBooleanType("clear_overflow", Opcode.BVC, Opcode.BVS), None)
    addThing(FlagBooleanType("set_zero", Opcode.BEQ, Opcode.BNE), None)
    addThing(FlagBooleanType("clear_zero", Opcode.BNE, Opcode.BEQ), None)
    addThing(FlagBooleanType("set_negative", Opcode.BMI, Opcode.BPL), None)
    addThing(FlagBooleanType("clear_negative", Opcode.BPL, Opcode.BMI), None)
  }

  def assertNotDefined(name: String, position: Option[Position]): Unit = {
    if (things.contains(name) || parent.exists(_.things.contains(name)))
      ErrorReporting.fatal(s"`$name` is already defined", position)
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

  def eval(e: Expression): Option[Constant] = {
    e match {
      case LiteralExpression(value, size) => Some(NumericConstant(value, size))
      case VariableExpression(name) =>
        maybeGet[ConstantThing](name).map(_.value)
      case IndexedExpression(_, _) => None
      case HalfWordExpression(param, hi) => eval(e).map(c => if (hi) c.hiByte else c.loByte)
      case SumExpression(params, decimal) =>
        params.map {
          case (minus, param) => (minus, eval(param))
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
        lc <- eval(l)
        hc <- eval(h)
      } yield hc.asl(8) + lc
      case FunctionCallExpression(name, params) =>
        name match {
          case "nonet" =>
            params match {
              case List(FunctionCallExpression("<<", ps@List(_, _))) =>
                constantOperation(MathOperator.Shl9, ps)
              case List(FunctionCallExpression("<<'", ps@List(_, _))) =>
                constantOperation(MathOperator.DecimalShl9, ps)
              case List(SumExpression(ps@List((true,_),(true,_)), false)) =>
                constantOperation(MathOperator.Plus9, ps.map(_._2))
              case List(SumExpression(ps@List((true,_),(true,_)), true)) =>
                constantOperation(MathOperator.DecimalPlus9, ps.map(_._2))
              case _ =>
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
                Some(CompoundConstant(op, c, addend))
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

  def registerFunction(stmt: FunctionDeclarationStatement, options: CompilationOptions): Unit = {
    val w = get[Type]("word")
    val name = stmt.name
    val resultType = get[Type](stmt.resultType)

    if (stmt.reentrant && stmt.interrupt) ErrorReporting.error(s"Reentrant function `$name` cannot be an interrupt handler", stmt.position)
    if (stmt.reentrant && stmt.params.nonEmpty) ErrorReporting.error(s"Reentrant function `$name` cannot have parameters", stmt.position)
    if (stmt.interrupt && stmt.params.nonEmpty) ErrorReporting.error(s"Interrupt function `$name` cannot have parameters", stmt.position)
    if (stmt.isMacro) {
      if (!stmt.assembly) {
        if (resultType != VoidType) ErrorReporting.error(s"Macro non-assembly function `$name` must return void", stmt.position)
      }
      if (stmt.assembly && stmt.params.exists(_.assemblyParamPassingConvention.inNonInlinedOnly))
        ErrorReporting.error(s"Macro function `$name` cannot have by-variable parameters", stmt.position)
    } else {
      if (!stmt.assembly) {
        if (stmt.params.exists(!_.assemblyParamPassingConvention.isInstanceOf[ByVariable]))
          ErrorReporting.error(s"Non-assembly function `$name` cannot have non-variable parameters", stmt.position)
      }
      if (stmt.params.exists(_.assemblyParamPassingConvention.inInlinedOnly))
        ErrorReporting.error(s"Non-macro function `$name` cannot have inlinable parameters", stmt.position)
    }

    val env = new Environment(Some(this), name + "$")
    stmt.params.foreach(p => env.registerParameter(p))
    val params = if (stmt.assembly) {
      AssemblyParamSignature(stmt.params.map {
        pd =>
          val typ = env.get[Type](pd.typ)
          pd.assemblyParamPassingConvention match {
            case ByVariable(vn) =>
              AssemblyParam(typ, env.get[MemoryVariable](vn), AssemblyParameterPassingBehaviour.Copy)
            case ByRegister(reg) =>
              AssemblyParam(typ, RegisterVariable(reg, typ), AssemblyParameterPassingBehaviour.Copy)
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
    stmt.statements match {
      case None =>
        stmt.address match {
          case None =>
            ErrorReporting.error(s"Extern function `${stmt.name}`needs an address", stmt.position)
          case Some(a) =>
            val addr = eval(a).getOrElse(Constant.error(s"Address of `${stmt.name}` is not a constant", stmt.position))
            val mangled = ExternFunction(
              name,
              resultType,
              params,
              addr,
              env,
              stmt.bank
            )
            addThing(mangled, stmt.position)
            registerAddressConstant(mangled, stmt.position)
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
            ErrorReporting.error("Macro functions cannot be in a defined segment", stmt.position)
          }
          val mangled = MacroFunction(
            name,
            resultType,
            params,
            env,
            executableStatements ++ (if (needsExtraRTS) List(AssemblyStatement.implied(Opcode.RTS, elidable = true)) else Nil)
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
            stmt.address.map(a => this.eval(a).getOrElse(Constant.error(s"Address of `${stmt.name}` is not a constant"))),
            executableStatements ++ (if (needsExtraRTS) List(ReturnStatement(None)) else Nil),
            interrupt = stmt.interrupt,
            kernalInterrupt = stmt.kernalInterrupt,
            reentrant = stmt.reentrant,
            position = stmt.position,
            declaredBank = stmt.bank
          )
          addThing(mangled, stmt.position)
          registerAddressConstant(mangled, stmt.position)
        }
    }
  }

  private def registerAddressConstant(thing: ThingInMemory, position: Option[Position]): Unit = {
    val addr = thing.toAddress
    addThing(ConstantThing(thing.name + ".addr", addr, get[Type]("pointer")), position)
    addThing(ConstantThing(thing.name + ".addr.hi", addr.hiByte, get[Type]("byte")), position)
    addThing(ConstantThing(thing.name + ".addr.lo", addr.loByte, get[Type]("byte")), position)
  }

  def registerParameter(stmt: ParameterDeclaration): Unit = {
    val typ = get[Type](stmt.typ)
    val b = get[Type]("byte")
    val p = get[Type]("pointer")
    stmt.assemblyParamPassingConvention match {
      case ByVariable(name) =>
        val zp = typ.name == "pointer" // TODO
      val v = UninitializedMemoryVariable(prefix + name, typ, if (zp) VariableAllocationMethod.Zeropage else VariableAllocationMethod.Auto, None)
        addThing(v, stmt.position)
        registerAddressConstant(v, stmt.position)
        if (typ.size == 2) {
          val addr = v.toAddress
          addThing(RelativeVariable(v.name + ".hi", addr + 1, b, zeropage = zp, None), stmt.position)
          addThing(RelativeVariable(v.name + ".lo", addr, b, zeropage = zp, None), stmt.position)
        }
      case ByRegister(_) => ()
      case ByConstant(name) =>
        val v = ConstantThing(prefix + name, UnexpandedConstant(prefix + name, typ.size), typ)
        addThing(v, stmt.position)
      case ByReference(name) =>
        val addr = UnexpandedConstant(prefix + name, typ.size)
        val v = RelativeVariable(prefix + name, addr, p, zeropage = false, None)
        addThing(v, stmt.position)
        addThing(RelativeVariable(v.name + ".hi", addr + 1, b, zeropage = false, None), stmt.position)
        addThing(RelativeVariable(v.name + ".lo", addr, b, zeropage = false, None), stmt.position)
    }
  }

  def registerUnnamedArray(array: InitializedArray): Unit = {
    val b = get[Type]("byte")
    val p = get[Type]("pointer")
    if (!array.name.endsWith(".array")) ???
    val pointerName = array.name.stripSuffix(".array")
    addThing(ConstantThing(pointerName, array.toAddress, p), None)
    addThing(ConstantThing(pointerName + ".addr", array.toAddress, p), None)
    addThing(array, None)
  }

  def registerArray(stmt: ArrayDeclarationStatement): Unit = {
    val b = get[Type]("byte")
    val p = get[Type]("pointer")
    stmt.elements match {
      case None =>
        stmt.length match {
          case None => ErrorReporting.error(s"Array `${stmt.name}` without size nor contents", stmt.position)
          case Some(l) =>
            val address = stmt.address.map(a => eval(a).getOrElse(ErrorReporting.fatal(s"Array `${stmt.name}` has non-constant address", stmt.position)))
            val lengthConst = eval(l).getOrElse(Constant.error(s"Array `${stmt.name}` has non-constant length", stmt.position))
            lengthConst match {
              case NumericConstant(length, _) =>
                if (length > 0xffff || length < 0) ErrorReporting.error(s"Array `${stmt.name}` has invalid length", stmt.position)
                val array = address match {
                  case None => UninitializedArray(stmt.name + ".array", length.toInt,
                              declaredBank = stmt.bank)
                  case Some(aa) => RelativeArray(stmt.name + ".array", aa, length.toInt,
                              declaredBank = stmt.bank)
                }
                addThing(array, stmt.position)
                registerAddressConstant(UninitializedMemoryVariable(stmt.name, p, VariableAllocationMethod.None, stmt.bank), stmt.position)
                val a = address match {
                  case None => array.toAddress
                  case Some(aa) => aa
                }
                addThing(RelativeVariable(stmt.name + ".first", a, b, zeropage = false,
                            declaredBank = stmt.bank), stmt.position)
                addThing(ConstantThing(stmt.name, a, p), stmt.position)
                addThing(ConstantThing(stmt.name + ".hi", a.hiByte.quickSimplify, b), stmt.position)
                addThing(ConstantThing(stmt.name + ".lo", a.loByte.quickSimplify, b), stmt.position)
                addThing(ConstantThing(stmt.name + ".array.hi", a.hiByte.quickSimplify, b), stmt.position)
                addThing(ConstantThing(stmt.name + ".array.lo", a.loByte.quickSimplify, b), stmt.position)
                if (length < 256) {
                  addThing(ConstantThing(stmt.name + ".length", lengthConst, b), stmt.position)
                }
              case _ => ErrorReporting.error(s"Array `${stmt.name}` has weird length", stmt.position)
            }
        }
      case Some(contents) =>
        stmt.length match {
          case None =>
          case Some(l) =>
            val lengthConst = eval(l).getOrElse(Constant.error(s"Array `${stmt.name}` has non-constant length", stmt.position))
            lengthConst match {
              case NumericConstant(ll, _) =>
                if (ll != contents.length) ErrorReporting.error(s"Array `${stmt.name}` has different declared and actual length", stmt.position)
              case _ => ErrorReporting.error(s"Array `${stmt.name}` has weird length", stmt.position)
            }
        }
        val length = contents.length
        if (length > 0xffff || length < 0) ErrorReporting.error(s"Array `${stmt.name}` has invalid length", stmt.position)
        val address = stmt.address.map(a => eval(a).getOrElse(Constant.error(s"Array `${stmt.name}` has non-constant address", stmt.position)))
        val data = contents.map(x => eval(x).getOrElse(Constant.error(s"Array `${stmt.name}` has non-constant contents", stmt.position)))
        val array = InitializedArray(stmt.name + ".array", address, data,
                    declaredBank = stmt.bank)
        addThing(array, stmt.position)
        registerAddressConstant(UninitializedMemoryVariable(stmt.name, p, VariableAllocationMethod.None,
                    declaredBank = stmt.bank), stmt.position)
        val a = address match {
          case None => array.toAddress
          case Some(aa) => aa
        }
        addThing(RelativeVariable(stmt.name + ".first", a, b, zeropage = false,
                    declaredBank = stmt.bank), stmt.position)
        addThing(ConstantThing(stmt.name, a, p), stmt.position)
        addThing(ConstantThing(stmt.name + ".hi", a.hiByte.quickSimplify, b), stmt.position)
        addThing(ConstantThing(stmt.name + ".lo", a.loByte.quickSimplify, b), stmt.position)
        addThing(ConstantThing(stmt.name + ".array.hi", a.hiByte.quickSimplify, b), stmt.position)
        addThing(ConstantThing(stmt.name + ".array.lo", a.loByte.quickSimplify, b), stmt.position)
        if (length < 256) {
          addThing(ConstantThing(stmt.name + ".length", NumericConstant(length, 1), b), stmt.position)
        }
    }
  }

  def registerVariable(stmt: VariableDeclarationStatement, options: CompilationOptions): Unit = {
    if (stmt.volatile) {
      ErrorReporting.warn("`volatile` not yet supported", options)
    }
    val name = stmt.name
    val position = stmt.position
    if (stmt.stack && parent.isEmpty) {
      if (stmt.stack && stmt.global) ErrorReporting.error(s"`$name` is static or global and cannot be on stack", position)
    }
    val b = get[Type]("byte")
    val typ = get[PlainType](stmt.typ)
    if (stmt.typ == "pointer") {
      //      if (stmt.constant) {
      //        ErrorReporting.error(s"Pointer `${stmt.name}` cannot be constant")
      //      }
      stmt.address.flatMap(eval) match {
        case Some(NumericConstant(a, _)) =>
          if ((a & 0xff00) != 0)
            ErrorReporting.error(s"Pointer `${stmt.name}` cannot be located outside the zero page")
        case _ => ()
      }
    }
    if (stmt.constant) {
      if (stmt.stack) ErrorReporting.error(s"`$name` is a constant and cannot be on stack", position)
      if (stmt.register) ErrorReporting.error(s"`$name` is a constant and cannot be in a register", position)
      if (stmt.address.isDefined) ErrorReporting.error(s"`$name` is a constant and cannot have an address", position)
      if (stmt.initialValue.isEmpty) ErrorReporting.error(s"`$name` is a constant and requires a value", position)
      val constantValue: Constant = stmt.initialValue.flatMap(eval).getOrElse(Constant.error(s"`$name` has a non-constant value", position))
      if (constantValue.requiredSize > typ.size) ErrorReporting.error(s"`$name` is has an invalid value: not in the range of `$typ`", position)
      addThing(ConstantThing(prefix + name, constantValue, typ), stmt.position)
      if (typ.size >= 2) {
        addThing(ConstantThing(prefix + name + ".hi", constantValue.hiByte, b), stmt.position)
        addThing(ConstantThing(prefix + name + ".lo", constantValue.loByte, b), stmt.position)
      }
    } else {
      if (stmt.stack && stmt.global) ErrorReporting.error(s"`$name` is static or global and cannot be on stack", position)
      if (stmt.register && typ.size != 1) ErrorReporting.error(s"A register variable `$name` is too large", position)
      if (stmt.register && stmt.global) ErrorReporting.error(s"`$name` is static or global and cannot be in a register", position)
      if (stmt.register && stmt.stack) ErrorReporting.error(s"`$name` cannot be simultaneously on stack and in a register", position)
      if (stmt.initialValue.isDefined && parent.isDefined) ErrorReporting.error(s"`$name` is local and not a constant and therefore cannot have a value", position)
      if (stmt.initialValue.isDefined && stmt.address.isDefined) ErrorReporting.warn(s"`$name` has both address and initial value - this may not work as expected!", options, position)
      if (stmt.register && stmt.address.isDefined) ErrorReporting.error(s"`$name` cannot by simultaneously at an address and in a register", position)
      if (stmt.stack) {
        val v = StackVariable(prefix + name, typ, this.baseStackOffset)
        baseStackOffset += typ.size
        addThing(v, stmt.position)
        if (typ.size == 2) {
          addThing(StackVariable(prefix + name + ".lo", b, baseStackOffset), stmt.position)
          addThing(StackVariable(prefix + name + ".hi", b, baseStackOffset + 1), stmt.position)
        }
      } else {
        val (v, addr) = stmt.address.fold[(VariableInMemory, Constant)]({
          val alloc =
            if (typ.name == "pointer") VariableAllocationMethod.Zeropage
            else if (stmt.global) VariableAllocationMethod.Static
            else if (stmt.register) VariableAllocationMethod.Register
            else VariableAllocationMethod.Auto
          if (alloc != VariableAllocationMethod.Static && stmt.initialValue.isDefined) {
            ErrorReporting.error(s"`$name` cannot be preinitialized`", position)
          }
          val v = stmt.initialValue.fold[MemoryVariable](UninitializedMemoryVariable(prefix + name, typ, alloc,
                      declaredBank = stmt.bank)){ive =>
            if (options.flags(CompilationFlag.ReadOnlyArrays)) {
              ErrorReporting.warn("Initialized variable in read-only segment", options, position)
            }
            val ivc = eval(ive).getOrElse(Constant.error(s"Initial value of `$name` is not a constant", position))
            InitializedMemoryVariable(name, None, typ, ivc,
                        declaredBank = stmt.bank)
          }
          registerAddressConstant(v, stmt.position)
          (v, v.toAddress)
        })(a => {
          val addr = eval(a).getOrElse(Constant.error(s"Address of `$name` has a non-constant value", position))
          val zp = addr match {
            case NumericConstant(n, _) => n < 0x100
            case _ => false
          }
          val v = RelativeVariable(prefix + name, addr, typ, zeropage = zp,
                      declaredBank = stmt.bank)
          registerAddressConstant(v, stmt.position)
          (v, addr)
        })
        addThing(v, stmt.position)
        if (!v.isInstanceOf[MemoryVariable]) {
          addThing(ConstantThing(v.name + "`", addr, b), stmt.position)
        }
        if (typ.size == 2) {
          addThing(RelativeVariable(prefix + name + ".hi", addr + 1, b, zeropage = v.zeropage,
                      declaredBank = stmt.bank), stmt.position)
          addThing(RelativeVariable(prefix + name + ".lo", addr, b, zeropage = v.zeropage,
                      declaredBank = stmt.bank), stmt.position)
        }
      }
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
        ErrorReporting.error(s"Invalid number of parameters for function `$name`", actualParams.headOption.flatMap(_._2.position))
      }
      function.params match {
        case NormalParamSignature(params) =>
          function.params.types.zip(actualParams).zip(params).foreach { case ((required, (actual, expr)), m) =>
            if (!actual.isAssignableTo(required)) {
              ErrorReporting.error(s"Invalid value for parameter `${m.name}` of function `$name`", expr.position)
            }
          }
        case AssemblyParamSignature(params) =>
          function.params.types.zip(actualParams).zipWithIndex.foreach { case ((required, (actual, expr)), ix) =>
            if (!actual.isAssignableTo(required)) {
              ErrorReporting.error(s"Invalid value for parameter ${ix + 1} of function `$name`", expr.position)
            }
          }
      }
      Some(function)
    } else {
      parent.flatMap(_.lookupFunction(name, actualParams))
    }
  }

  def collectDeclarations(program: Program, options: CompilationOptions): Unit = {
    if (options.flag(CompilationFlag.OptimizeForSonicSpeed)) {
      addThing(InitializedArray("identity$", None, List.tabulate(256)(n => NumericConstant(n, 1)),
                  declaredBank = None), None)
    }
    program.declarations.foreach {
      case f: FunctionDeclarationStatement => registerFunction(f, options)
      case v: VariableDeclarationStatement => registerVariable(v, options)
      case a: ArrayDeclarationStatement => registerArray(a)
      case i: ImportStatement => ()
    }
    if (options.flag(CompilationFlag.ZeropagePseudoregister) && !things.contains("__reg")) {
      registerVariable(VariableDeclarationStatement(
        name = "__reg",
        bank = None,
        typ = "pointer",
        global = true,
        stack = false,
        constant = false,
        volatile = false,
        register = false,
        initialValue = None,
        address = None), options)
    }
    if (!things.contains("__constant8")) {
      things("__constant8") = InitializedArray("__constant8", None, List(NumericConstant(8, 1)),
                  declaredBank = None)
    }
  }

  private def checkName[T <: Thing : Manifest](objType: String, name: String, pos: Option[Position]): Unit = {
    if (maybeGet[T](name).isEmpty) {
      ErrorReporting.error(s"$objType `$name` is not defined", pos)
    }
  }

  def nameCheck(nodes: List[_ <:Node]): Unit = nodes.foreach(nameCheck)

  def nameCheck(node: Node): Unit = node match {
    case _:AssemblyStatement => ()
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
    case FunctionCallExpression(name, params) =>
      if (name.exists(_.isLetter) && !Environment.predefinedFunctions(name)) {
        checkName[CallableThing]("Function or type", name, node.position)
      }
      nameCheck(params)
  }
}

object Environment {
  val predefinedFunctions = Set("not", "hi", "lo", "nonet")
}
