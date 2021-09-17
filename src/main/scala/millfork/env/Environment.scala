package millfork.env

import millfork.assembly.m6809.{MOpcode, NonExistent}
import millfork.assembly.{BranchingOpcodeMapping, Elidability}
import millfork.{env, _}
import millfork.assembly.mos.{AddrMode, Opcode}
import millfork.assembly.z80._
import millfork.compiler.{AbstractExpressionCompiler, LabelGenerator}
import millfork.error.Logger
import millfork.node._
import millfork.output._
import org.apache.commons.lang3.StringUtils

import java.nio.file.Files
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/**
  * @author Karol Stasiak
  */
//noinspection NotImplementedCode
class Environment(val parent: Option[Environment], val prefix: String, val cpuFamily: CpuFamily.Value, val options: CompilationOptions) {

  @inline
  def jobContext: JobContext = options.jobContext
  @inline
  def log: Logger = jobContext.log
  @inline
  def nextLabel: LabelGenerator = jobContext.nextLabel

  private val constantsThatShouldHaveBeenImportedEarlier = new MutableMultimap[String, Option[Position]]()

  private var baseStackOffset: Int = cpuFamily match {
    case CpuFamily.M6502 => 0x101
    case CpuFamily.I80 => 0
    case CpuFamily.I86 => 0
    case CpuFamily.M6809 => 0
  }

  def errorConstant(msg: String, invalidExpression: Option[Expression], position: Option[Position] = None): Constant = {
    log.error(msg, position.orElse(invalidExpression.flatMap(_.position)))
    log.info("Did you forget to import an appropriate module?")
    invalidExpression.foreach(markAsConstantsThatShouldHaveBeenImportedEarlier)
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
    val e = new Environment(None, "", cpuFamily, options)
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
      case RelativeArray(_, NumericConstant(addr, _), size, declaredBank, _, _, _) =>
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
                        onEachVariable: (String, (Int, Int)) => Unit,
                        pass: Int,
                        forZpOnly: Boolean): Unit = {
    if (forZpOnly && !options.platform.hasZeroPage) {
      return
    }
    if (nf.exists(_.name.endsWith(".trampoline"))) {
      return
    }
    if (log.traceEnabled) log.trace("Allocating variables in " + nf.map(f => "function " + f.name).getOrElse("global scope"))
    val b = get[Type]("byte")
    val p = get[Type]("pointer")
    val params = nf.fold(List[String]()) { f =>
      f.params match {
        case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 1 && options.platform.cpuFamily == CpuFamily.M6502 =>
          Nil
        case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 1 && options.platform.cpuFamily == CpuFamily.I80 =>
          Nil
        case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 1 && options.platform.cpuFamily == CpuFamily.M6809 =>
          Nil
        case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 2 && options.platform.cpuFamily == CpuFamily.I80 =>
          Nil
        case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 2 && options.platform.cpuFamily == CpuFamily.M6809 =>
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

    def prioritize[T](list: List[T])(hasPriority: T => Boolean): List[T] = list.filter(hasPriority) ++ list.filterNot(hasPriority)

    val toAdd = prioritize(things.values.toList)(th => th.name =="__reg").flatMap {
      case m: UninitializedMemory if passForAlloc(m.alloc) == pass && nf.isDefined == isLocalVariableName(m.name) && !m.name.endsWith(".addr") && maybeGet[Thing](m.name + ".array").isEmpty =>
        if (log.traceEnabled) log.trace("Allocating " + m.name)
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
        val bank0 = mem.banks(bank)
        m.alloc match {
          case VariableAllocationMethod.None =>
            Nil
          case VariableAllocationMethod.Zeropage =>
            if (forZpOnly || !options.platform.hasZeroPage) {
              val addr =
                allocators(bank).allocateBytes(bank0, callGraph, vertex, options, m.sizeInBytes, initialized = false, writeable = true, location = AllocationLocation.Zeropage, alignment = m.alignment)
              if (log.traceEnabled) log.trace("addr $" + addr.toHexString)
              onEachVariable(m.name, bank0.index -> addr)
              List(
                ConstantThing(m.name.stripPrefix(prefix) + "`", NumericConstant(addr, 2), p)
              )
            } else Nil
          case VariableAllocationMethod.Auto | VariableAllocationMethod.Register | VariableAllocationMethod.Static =>
            if (m.alloc == VariableAllocationMethod.Register && options.flag(CompilationFlag.FallbackValueUseWarning)) {
              options.platform.cpuFamily match {
                case CpuFamily.M6502 if m.sizeInBytes == 1 =>
                  log.warn(s"Failed to inline variable `${m.name}` into a register", None)
                case CpuFamily.I80 | CpuFamily.I80 | CpuFamily.I86 | CpuFamily.M6809 if m.sizeInBytes <= 2 =>
                  log.warn(s"Failed to inline variable `${m.name}` into a register", None)
                case _ =>
              }
            }
            if (m.sizeInBytes == 0) Nil else {
              val graveName = m.name.stripPrefix(prefix) + "`"
              if (forZpOnly) {
                if (bank == "default") {
                  allocators(bank).tryAllocateZeropageBytes(bank0, callGraph, vertex, options, m.sizeInBytes, alignment = m.alignment) match {
                    case None => Nil
                    case Some(addr) =>
                      if (log.traceEnabled) log.trace("addr $" + addr.toHexString)
                      onEachVariable(m.name, bank0.index -> addr)
                      List(
                        ConstantThing(m.name.stripPrefix(prefix) + "`", NumericConstant(addr, 2), p)
                      )
                  }
                } else Nil
              } else if (things.contains(graveName)) {
                Nil
              } else {
                val addr = allocators(bank).allocateBytes(bank0, callGraph, vertex, options, m.sizeInBytes, initialized = false, writeable = true, location = AllocationLocation.Either, alignment = m.alignment)
                if (log.traceEnabled) log.trace("addr $" + addr.toHexString)
                onEachVariable(m.name, bank0.index -> addr)
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

  var builtinsAdded: Boolean = false
  val things: mutable.Map[String, Thing] = mutable.Map()
  val pointiesUsed: mutable.Map[String, Set[String]] = mutable.Map()
  val removedThings: mutable.Set[String] = mutable.Set()
  val knownLocalLabels: mutable.Set[(String, Option[Position])] = mutable.Set()

  private def addThing(t: Thing, position: Option[Position]): Unit = {
    if (assertNotDefined(t.name, position)) {
      things(t.name.stripPrefix(prefix)) = t
    }
  }

  private def addThing(localName: String, t: Thing, position: Option[Position]): Unit = {
    if (assertNotDefined(t.name, position)) {
      things(localName) = t
    }
  }

  def getReturnedVariables(statements: Seq[Statement]): Set[String] = {
    statements.flatMap {
      case ReturnStatement(Some(VariableExpression(v))) => Set(v)
      case ReturnStatement(_) => Set("/none/", "|none|")
      case x: CompoundStatement => getReturnedVariables(x.getChildStatements)
      case _ => Set.empty[String]
    }.toSet
  }

  def coerceLocalVariableIntoGlobalVariable(localVarToRelativize: String, concreteGlobalTarget: String): Unit = {
    log.trace(s"Coercing $localVarToRelativize to $concreteGlobalTarget")
    def removeVariableImpl2(e: Environment, str: String): Unit = {
      e.removeVariable(str)
      e.removeVariable(str.stripPrefix(prefix))
      e.parent.foreach(x => removeVariableImpl2(x,str))
    }
    removeVariableImpl2(this, prefix + localVarToRelativize)
    val namePrefix = concreteGlobalTarget + '.'
    root.things.filter { entry =>
      entry._1 == concreteGlobalTarget || entry._1.startsWith(namePrefix)
    }.foreach { entry =>
      val name = entry._1
      val thing = entry._2
      val newName = if (name == concreteGlobalTarget) localVarToRelativize else localVarToRelativize + '.' + name.stripPrefix(namePrefix)
      val newThing = thing match {
        case t: VariableInMemory => RelativeVariable(prefix + newName, t.toAddress, t.typ, t.zeropage, t.declaredBank, t.isVolatile)
        case t: ConstantThing => t.copy(name = prefix + newName)
        case t => println(t); ???
      }
      addThing(newThing, None)
    }
  }

  def removeVariable(str: String): Unit = {
    log.trace("Removing variable: " + str)
    removeVariableImpl(str)
  }

  private def removeVariableImpl(str: String): Unit = {
    def extractThingName(fullName: String): String = {
      var result = fullName.takeWhile(_ != '.')
      if (result.length == fullName.length) return result
      val suffix = fullName.drop(result.length)
      if (suffix == ".return" || suffix.startsWith(".return.")) {
        result += ".return"
      }
      result
    }

    val toRemove = things.keys.filter { n =>
      val baseName = extractThingName(n)
      baseName == str || baseName == str.stripPrefix(prefix)
    }.toSet
    removedThings ++= toRemove.map(_.stripPrefix(prefix))
    things --= toRemove
    parent.foreach(_ removeVariableImpl str)
  }

  def get[T <: Thing : Manifest](name: String, position: Option[Position] = None): T = {
    if (name.startsWith("function.") && implicitly[Manifest[T]].runtimeClass.isAssignableFrom(classOf[PointerType])) {
      val tokens = name.stripPrefix("function.").split("\\.to\\.", 2)
      if (tokens.length == 2) {
        return FunctionPointerType(name, tokens(0), tokens(1), maybeGet[Type](tokens(0)), maybeGet[Type](tokens(1))).asInstanceOf[T]
      }
    }
    if (name.startsWith("pointer.") && implicitly[Manifest[T]].runtimeClass.isAssignableFrom(classOf[PointerType])) {
      val targetName = name.stripPrefix("pointer.")
      targetName match {
        case "interrupt" => return InterruptPointerType.asInstanceOf[T]
        case "kernal_interrupt" => return KernalInterruptPointerType.asInstanceOf[T]
        case _ =>
          val target = maybeGet[VariableType](targetName)
          return PointerType(name, targetName, target).asInstanceOf[T]
      }
    }
    val clazz = implicitly[Manifest[T]].runtimeClass
    if (things.contains(name)) {
      val t: Thing = things(name)
      if ((t ne null) && clazz.isInstance(t) && !t.isInstanceOf[Alias]) {
        t.asInstanceOf[T]
      } else {
        t match {
          case Alias(_, target, deprectated) =>
            if (deprectated && options.flag(CompilationFlag.DeprecationWarning)) {
              log.warn(s"Alias `$name` is deprecated, use `$target` instead", position)
            }
            root.get[T](target)
          case _ => throw IdentifierHasWrongTypeOfThingException(clazz, name, position)
        }
      }
    } else parent.fold {
      hintTypo(name)
      throw UndefinedIdentifierException(clazz, name, position)
    } {
      _.get[T](name, position)
    }
  }
  
  def root: Environment = parent.fold(this)(_.root)

  def maybeGet[T <: Thing : Manifest](name: String): Option[T] = {
    if (name.startsWith("pointer.") && implicitly[Manifest[T]].runtimeClass.isAssignableFrom(classOf[PointerType])) {
      val targetName = name.stripPrefix("pointer.")
      val target = maybeGet[VariableType](targetName)
      return Some(PointerType(name, targetName, target)).asInstanceOf[Option[T]]
    }
    if (things.contains(name)) {
      val t: Thing = things(name)
      val clazz = implicitly[Manifest[T]].runtimeClass
      t match {
        case Alias(_, target, deprectated) =>
          if (deprectated && options.flag(CompilationFlag.DeprecationWarning)) {
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
    maybeGet[StackVariable](arrayName).
      orElse(maybeGet[ThingInMemory](arrayName)).
      orElse(maybeGet[ThingInMemory](arrayName + ".array")).
      orElse(maybeGet[ConstantThing](arrayName)).
      getOrElse{
        log.error(s"`$arrayName` is not an array or a pointer")
        get[Thing]("nullptr")
      }
  }

  def getPointy(name: String): Pointy = {
    InitializedMemoryVariable
    UninitializedMemoryVariable
    getArrayOrPointer(name) match {
      case th@InitializedArray(_, _, cs, _, i, e, ro, _, _) => ConstantPointy(th.toAddress, Some(name), Some(e.alignedSize * cs.length), Some(cs.length), i, e, th.alignment, readOnly = ro)
      case th@UninitializedArray(_, elementCount, _, i, e, ro, _, _) => ConstantPointy(th.toAddress, Some(name), Some(elementCount * e.alignedSize), Some(elementCount / e.size), i, e, th.alignment, readOnly = ro)
      case th@RelativeArray(_, _, elementCount, _, i, e, ro) => ConstantPointy(th.toAddress, Some(name), Some(elementCount * e.alignedSize), Some(elementCount / e.size), i, e, NoAlignment, readOnly = ro)
      case ConstantThing(_, value, typ) if typ.size <= 2 && typ.isPointy =>
        val e = get[VariableType](typ.pointerTargetName)
        val w = get[VariableType]("word")
        ConstantPointy(value, None, None, None, w, e, NoAlignment, readOnly = false)
      case th:VariableInMemory if th.typ.isPointy=>
        val e = get[VariableType](th.typ.pointerTargetName)
        val w = get[VariableType]("word")
        VariablePointy(th.toAddress, w, e, th.zeropage, th.isVolatile)
      case th:StackVariable if th.typ.isPointy =>
        val e = get[VariableType](th.typ.pointerTargetName)
        val w = get[VariableType]("word")
        StackVariablePointy(th.baseOffset, w, e)
      case _ =>
        log.error(s"$name is not a valid pointer or array")
        val b = get[VariableType]("byte")
        val w = get[VariableType]("word")
        ConstantPointy(Constant.Zero, None, None, None, w, b, NoAlignment, readOnly = false)
    }
  }

  if (parent.isEmpty) {
    addThing(VoidType, None)
    addThing(NullType, None)
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
    addThing(DerivedPlainType("ubyte", b, isSigned = false, isPointy = false), None)
    addThing(DerivedPlainType("sbyte", b, isSigned = true, isPointy = false), None)
    addThing(Alias("unsigned8", "ubyte"), None)
    addThing(Alias("signed8", "sbyte"), None)
    addThing(DerivedPlainType("unsigned16", w, isSigned = false, isPointy = false), None)
    addThing(DerivedPlainType("signed16", w, isSigned = true, isPointy = false), None)
    addThing(InterruptPointerType, None)
    addThing(KernalInterruptPointerType, None)
    for (bits <- Seq(24, 32, 40, 48, 56, 64, 72, 80, 88, 96, 104, 112, 120, 128)) {
      addThing(DerivedPlainType("unsigned" + bits, get[BasicPlainType]("int" + bits), isSigned = false, isPointy = false), None)
    }
    if (options.flag(CompilationFlag.EnableInternalTestSyntax)) {
      for (bits <- Seq(24, 32, 40, 48, 56, 64, 72, 80, 88, 96, 104, 112, 120, 128)) {
        addThing(DerivedPlainType("signed" + bits, get[BasicPlainType]("int" + bits), isSigned = true, isPointy = false), None)
      }
    }
    val trueType = ConstantBooleanType("true$", value = true)
    val falseType = ConstantBooleanType("false$", value = false)
    addThing(trueType, None)
    addThing(falseType, None)
    addThing(ConstantThing("true", NumericConstant(1, 0), trueType), None)
    addThing(ConstantThing("false", NumericConstant(0, 0), falseType), None)
    addThing(FatBooleanType, None)
    val nullptrValue = options.features.getOrElse("NULLPTR", 0L)
    val nullptrConstant = NumericConstant(nullptrValue, 2)
    addThing(ConstantThing("nullptr", nullptrConstant, NullType), None)
    addThing(ConstantThing("nullptr.hi", nullptrConstant.hiByte.quickSimplify, b), None)
    addThing(ConstantThing("nullptr.lo", nullptrConstant.loByte.quickSimplify, b), None)
    addThing(ConstantThing("nullptr.raw", nullptrConstant, p), None)
    addThing(ConstantThing("nullptr.raw.hi", nullptrConstant.hiByte.quickSimplify, b), None)
    addThing(ConstantThing("nullptr.raw.lo", nullptrConstant.loByte.quickSimplify, b), None)
    val nullcharValue = options.features.getOrElse("NULLCHAR", options.platform.defaultCodec.stringTerminator.head.toLong)
    val nullcharScrValue = options.features.getOrElse("NULLCHAR_SCR", options.platform.screenCodec.stringTerminator.head.toLong)
    val nullcharConstant = NumericConstant(nullcharValue, 1)
    val nullcharScrConstant = NumericConstant(nullcharScrValue, 1)
    addThing(ConstantThing("nullchar", nullcharConstant, b), None)
    addThing(ConstantThing("nullchar_scr", nullcharScrConstant, b), None)
    val __zeropage_usage = UnexpandedConstant("__zeropage_usage", 1)
    addThing(ConstantThing("__zeropage_usage", __zeropage_usage, b), None)
    def addUnexpandedWordConstant(name: String): Unit = {
      val c = UnexpandedConstant(name, 2)
      addThing(ConstantThing(name, c, w), None)
      addThing(ConstantThing(name + ".hi", c.hiByte, b), None)
      addThing(ConstantThing(name + ".lo", c.loByte, b), None)
    }
    def addUnexpandedByteConstant(name: String): Unit = {
      val c = UnexpandedConstant(name, 1)
      addThing(ConstantThing(name, c, b), None)
    }
    def addUnexpandedPointerConstant(name: String): Unit = {
      val c = UnexpandedConstant(name, 2)
      addThing(ConstantThing(name, c, p), None)
      addThing(ConstantThing(name + ".hi", c.hiByte, b), None)
      addThing(ConstantThing(name + ".lo", c.loByte, b), None)
    }
    addUnexpandedPointerConstant("__rwdata_start")
    addUnexpandedPointerConstant("__rwdata_end")
    addUnexpandedPointerConstant("__heap_start")
    if (options.platform.ramInitialValuesBank.isDefined) {
      addUnexpandedPointerConstant("__rwdata_init_start")
      addUnexpandedPointerConstant("__rwdata_init_end")
      addUnexpandedWordConstant("__rwdata_size")
    }
    for(segment <- options.platform.bankNumbers.keys) {
      addUnexpandedPointerConstant(s"segment.$segment.start")
      addUnexpandedPointerConstant(s"segment.$segment.codeend")
      addUnexpandedPointerConstant(s"segment.$segment.datastart")
      addUnexpandedPointerConstant(s"segment.$segment.heapstart")
      addUnexpandedPointerConstant(s"segment.$segment.end")
      addUnexpandedWordConstant(s"segment.$segment.length")
      addUnexpandedByteConstant(s"segment.$segment.bank")
      addUnexpandedByteConstant(s"segment.$segment.fill")
    }
    addThing(ConstantThing("$0000", Constant.WordZero, p), None)
    addThing(FlagBooleanType("set_carry",
      BranchingOpcodeMapping(Opcode.BCS, IfFlagSet(ZFlag.C), MOpcode.BCS),
      BranchingOpcodeMapping(Opcode.BCC, IfFlagClear(ZFlag.C), MOpcode.BCC)),
      None)
    addThing(FlagBooleanType("clear_carry",
      BranchingOpcodeMapping(Opcode.BCC, IfFlagClear(ZFlag.C), MOpcode.BCC),
      BranchingOpcodeMapping(Opcode.BCS, IfFlagSet(ZFlag.C), MOpcode.BCS)),
      None)
    addThing(FlagBooleanType("set_overflow",
      BranchingOpcodeMapping(Opcode.BVS, IfFlagSet(ZFlag.P), MOpcode.BVS),
      BranchingOpcodeMapping(Opcode.BVC, IfFlagClear(ZFlag.P), MOpcode.BVC)),
      None)
    addThing(FlagBooleanType("clear_overflow",
      BranchingOpcodeMapping(Opcode.BVC, IfFlagClear(ZFlag.P), MOpcode.BVC),
      BranchingOpcodeMapping(Opcode.BVS, IfFlagSet(ZFlag.P), MOpcode.BVS)),
      None)
    addThing(FlagBooleanType("set_zero",
      BranchingOpcodeMapping(Opcode.BEQ, IfFlagSet(ZFlag.Z), MOpcode.BEQ),
      BranchingOpcodeMapping(Opcode.BNE, IfFlagClear(ZFlag.Z), MOpcode.BNE)),
      None)
    addThing(FlagBooleanType("clear_zero",
      BranchingOpcodeMapping(Opcode.BNE, IfFlagClear(ZFlag.Z), MOpcode.BNE),
      BranchingOpcodeMapping(Opcode.BEQ, IfFlagSet(ZFlag.Z), MOpcode.BEQ)),
      None)
    addThing(FlagBooleanType("set_negative",
      BranchingOpcodeMapping(Opcode.BMI, IfFlagSet(ZFlag.S), MOpcode.BMI),
      BranchingOpcodeMapping(Opcode.BPL, IfFlagClear(ZFlag.S), MOpcode.BPL)),
      None)
    addThing(FlagBooleanType("clear_negative",
      BranchingOpcodeMapping(Opcode.BPL, IfFlagClear(ZFlag.S), MOpcode.BPL),
      BranchingOpcodeMapping(Opcode.BMI, IfFlagSet(ZFlag.S), MOpcode.BMI)),
      None)
    val byte_and_pointer$ = StructType("byte_and_pointer$", List(FieldDesc("byte", "zp", false, None), FieldDesc("pointer", "branch", false, None)), NoAlignment)
    val hudson_transfer$ = StructType("hudson_transfer$", List(FieldDesc("word", "a", false, None), FieldDesc("word", "b", false, None), FieldDesc("word", "c", false, None)), NoAlignment)
    addThing(byte_and_pointer$, None)
    addThing(hudson_transfer$, None)
    Environment.constOnlyBuiltinFunction.foreach(n => addThing(ConstOnlyCallable(n), None))
    builtinsAdded = true
  }

  def assertNotDefined(name: String, position: Option[Position]): Boolean = {
    if (builtinsAdded && Environment.invalidNewIdentifiers(name)) {
      if (Environment.predefinedFunctions(name)) {
        log.error(s"Cannot redefine a builtin predefined function `$name`", position)
      } else if (Environment.neverIdentifiers(name)) {
        log.error(s"Cannot the keyword `$name` as a name", position)
      } else if (Environment.invalidNewIdentifiers(name)) {
        log.error(s"Cannot redefine a builtin predefined identifier `$name`", position)
      }
      false
    } else if (things.contains(name) || parent.exists(_.things.contains(name)) || things.contains(name.stripPrefix(prefix))) {
      if (!name.contains('.')){
        if (parent.isDefined) {
          log.error(s"`${name.stripPrefix(prefix)}` is already defined in this function", position)
        } else {
          log.error(s"`$name` is already defined", position)
        }
      }
      false
    } else {
      true
    }
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
              case Some(t: StructType) =>
                // dunno what to do
                variable -> constant
              case Some(t: UnionType) =>
                // dunno what to do
                None -> Constant.Zero
              case Some(t: Type) =>
                if (t.isSigned) Some(e) -> Constant.Zero
                else variable -> constant.fitInto(t)
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
            log.error(s"`$name` is not defined", expr.position)
            hintTypo(name)
            1
          case Some(thing) => thing match {
            case t: Type => t.alignedSize
            case v: Variable => v.typ.alignedSize
            case a: MfArray => a.sizeInBytes
            case ConstantThing(_,  MemoryAddressConstant(a: MfArray), _) => a.sizeInBytes
            case x =>
              log.error("Invalid parameter for expr: " + name)
              1
          }
        }
      case _ =>
        AbstractExpressionCompiler.getExpressionType(this, log, expr).alignedSize
    }
    NumericConstant(size, Constant.minimumSize(size))
  }

  def evalTypeof(expr: Expression): Constant = {
    val typeof: Int = expr match {
      case VariableExpression(name) =>
        maybeGet[Thing](name) match {
          case None =>
            log.error(s"`$name` is not defined", expr.position)
            hintTypo(name)
            1
          case Some(thing) => thing match {
            case t: Type => t.name.hashCode & 0xffff
            case v: Variable => v.typ.name.hashCode & 0xffff
            case a: MfArray => a.elementType.name.hashCode & 0xffff
            case ConstantThing(_,  MemoryAddressConstant(a: MfArray), _) => a.elementType.name.hashCode & 0xffff
            case x =>
              log.error("Invalid parameter for expr: " + name)
              1
          }
        }
      case _ =>
        AbstractExpressionCompiler.getExpressionType(this, log, expr).name.hashCode & 0xffff
    }
    NumericConstant(typeof, 2)
  }

  def eval(e: Expression, vars: Map[String, Constant]): Option[Constant] = evalImpl(e, Some(vars))

  def eval(e: Expression): Option[Constant] = {
    if (e.constantValueCache ne null) return e.constantValueCache
    val cv = evalImpl(e, None)
    e.constantValueCache = cv
    cv
  }

  //noinspection ScalaUnnecessaryParentheses,ZeroIndexToHead
  private def evalImpl(e: Expression, vv: Option[Map[String, Constant]]): Option[Constant] = try{{
    e match {
      case LiteralExpression(value, size) => Some(NumericConstant(value, size))
      case tl:TextLiteralExpression => Some(getPointy(getTextLiteralArrayName(tl)).asInstanceOf[ConstantPointy].value)
      case ConstantArrayElementExpression(c) => Some(c)
      case GeneratedConstantExpression(c, t) => Some(c)
      case VariableExpression(name) =>
        if (name.startsWith(".")) return Some(MemoryAddressConstant(Label(prefix + name)))
        vv match {
          case Some(m) if m.contains(name) => Some(m(name))
          case _ => maybeGet[ConstantThing](name).map(_.value)
        }
      case IndexedExpression(arrName, index) =>
        getPointy(arrName) match {
          case ConstantPointy(MemoryAddressConstant(arr:InitializedArray), _, _, _, _, _, _, _) if arr.readOnly && arr.elementType.alignedSize == 1 =>
            evalImpl(index, vv).flatMap {
              case NumericConstant(constIndex, _) =>
                if (constIndex >= 0 && constIndex < arr.sizeInBytes) {
                  evalImpl(arr.contents(constIndex.toInt), vv)
                } else None
              case _ => None
            }
          case _ => None
        }
      case _: DerefExpression => None
      case _: IndirectFieldExpression => None
      case _: DerefDebuggingExpression => None
      case HalfWordExpression(param, hi) => evalImpl(param, vv).map(c => if (hi) c.hiByte else c.loByte)
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
      case fce@FunctionCallExpression(name, params) =>
        name match {
          case "sizeof" =>
            if (params.size == 1) {
              Some(evalSizeof(params.head))
            } else {
              log.error("Invalid number of parameters for `sizeof`", e.position)
              Some(Constant.One)
            }
          case "typeof" =>
            if (params.size == 1) {
              Some(evalTypeof(params.head))
            } else {
              log.error("Invalid number of parameters for `typeof`", e.position)
              Some(Constant.Zero)
            }
          case "hi" =>
            if (params.size == 1) {
              evalImpl(params.head, vv).map(_.hiByte.quickSimplify)
            } else {
              log.error("Invalid number of parameters for `hi`", e.position)
              None
            }
          case "lo" =>
            if (params.size == 1) {
              evalImpl(params.head, vv).map(_.loByte.quickSimplify)
            } else {
              log.error("Invalid number of parameters for `lo`", e.position)
              None
            }
          case "sin" =>
            if (params.size == 2) {
              (evalImpl(params(0), vv) -> evalImpl(params(1), vv)) match {
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
              (evalImpl(params(0), vv) -> evalImpl(params(1), vv)) match {
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
              (evalImpl(params(0), vv) -> evalImpl(params(1), vv)) match {
                case (Some(NumericConstant(angle, _)), Some(NumericConstant(scale, _))) =>
                  val value = (scale * math.tan(angle * math.Pi / 128)).round.toInt
                  Some(Constant(value))
                case _ => None
              }
            } else {
              log.error("Invalid number of parameters for `tan`", e.position)
              None
            }
          case "min" =>
            constantOperation(MathOperator.Minimum, fce, vv)
          case "max" =>
            constantOperation(MathOperator.Maximum, fce, vv)
          case "if" =>
            if (params.size == 3) {
              eval(params(0)).map(_.quickSimplify) match {
                case Some(NumericConstant(cond, _)) =>
                  evalImpl(params(if (cond != 0) 1 else 2), vv)
                case Some(c) =>
                  if (c.isProvablyGreaterOrEqualThan(1)) evalImpl(params(1), vv)
                  else if (c.isProvablyZero) evalImpl(params(2), vv)
                  else None
                case _ => None
              }
            } else {
              log.error("Invalid number of parameters for `if`", e.position)
              None
            }
          case "nonet" =>
            params match {
              case List(FunctionCallExpression("<<", ps@List(_, _))) =>
                constantOperation(MathOperator.Shl9, ps, vv)
              case List(FunctionCallExpression("<<'", ps@List(_, _))) =>
                constantOperation(MathOperator.DecimalShl9, ps, vv)
              case List(SumExpression(ps@List((false,_),(false,_)), false)) =>
                constantOperation(MathOperator.Plus9, ps.map(_._2), vv)
              case List(SumExpression(ps@List((false,_),(false,_)), true)) =>
                constantOperation(MathOperator.DecimalPlus9, ps.map(_._2), vv)
              case List(_) =>
                None
              case _ =>
                log.error("Invalid number of parameters for `nonet`", e.position)
                None
            }
          case ">>'" =>
            constantOperation(MathOperator.DecimalShr, params, vv)
          case "<<'" =>
            constantOperation(MathOperator.DecimalShl, params, vv)
          case ">>" =>
            constantOperation(MathOperator.Shr, params, vv)
          case "<<" =>
            constantOperation(MathOperator.Shl, params, vv)
          case ">>>>" =>
            constantOperation(MathOperator.Shr9, params, vv)
          case "*'" =>
            constantOperation(MathOperator.DecimalTimes, params, vv)
          case "*" =>
            constantOperation(MathOperator.Times, params, vv)
          case "/" =>
            constantOperation(MathOperator.Divide, params, vv)
          case "%%" =>
            constantOperation(MathOperator.Modulo, params, vv)
          case "&&" | "&" =>
            constantOperation(MathOperator.And, params, vv)
          case "^" =>
            constantOperation(MathOperator.Exor, params, vv)
          case "||" | "|" =>
            constantOperation(MathOperator.Or, params, vv)
          case ">" => evalComparisons(params, vv, _ > _)
          case "<" => evalComparisons(params, vv, _ < _)
          case ">=" => evalComparisons(params, vv, _ >= _)
          case "<=" => evalComparisons(params, vv, _ <= _)
          case "==" => evalComparisons(params, vv, _ == _)
          case "!=" =>
            sequence(params.map(p => evalImpl(p, vv))) match {
              case Some(List(NumericConstant(n1, _), NumericConstant(n2, _))) =>
                Some(if (n1 != n2) Constant.One else Constant.Zero)
              case _ => None
            }
          case _ =>
            maybeGet[Thing](name) match {
              case Some(t: StructType) =>
                if (params.size == t.fields.size) {
                  sequence(params.map(p => evalImpl(p, vv))).map(fields => StructureConstant(t, fields.zip(t.fields).map{
                    case (fieldConst, fieldDesc) =>
                      if (fieldDesc.arraySize.isDefined) {
                        log.error(s"Cannot define a struct literal for a struct type ${t.name} with array fields", fce.position)
                      }
                      fieldConst.fitInto(get[Type](fieldDesc.typeName))
                  }))
                } else None
              case Some(n: NormalFunction) if n.isConstPure =>
                if (params.size == n.params.length) {
                  sequence(params.map(p => evalImpl(p, vv))) match {
                    case Some(args) => ConstPureFunctions.eval(this, n, args)
                    case _ => None
                  }
                } else None
              case Some(_: UnionType) =>
                None
              case Some(t: Type) =>
                if (params.size == 1) {
                  eval(params.head).map{ c =>
                     c.fitInto(t)
                  }
                } else None
              case _ => None
            }
        }
    }
  }.map(_.quickSimplify)} catch {
    case ez:NonFatalCompilationException =>
      log.error(ez.getMessage, e.position)
      None
  }

  def debugConstness(item: Expression): Unit = {
    if (!log.debugEnabled) return
    if (eval(item).isEmpty) {
      log.debug(s"$item is not const!")
      item match {
        case FunctionCallExpression(_, expressions) => expressions.foreach(debugConstness)
        case SumExpression(expressions, _) => expressions.map(_._2).foreach(debugConstness)
        case SeparateBytesExpression(b1, b2) =>
          debugConstness(b1)
          debugConstness(b2)
        case _ =>
      }
    }
  }

  private def evalComparisons(params: List[Expression], vv: Option[Map[String, Constant]], cond: (Long, Long) => Boolean): Option[Constant] = {
    if (params.size < 2) return None
    val numbers = sequence(params.map{ e =>
      evalImpl(e, vv) match {
        case Some(NumericConstant(n, _)) => Some(n)
        case _ => None
      }
    })
    numbers.map { ns =>
      if (ns.init.zip(ns.tail).forall(cond.tupled)) Constant.One else Constant.Zero
    }
  }

  private def constantOperation(op: MathOperator.Value, fce: FunctionCallExpression, vv: Option[Map[String, Constant]]): Option[Constant] = {
    val params = fce.expressions
    if (params.isEmpty) {
      log.error(s"Invalid number of parameters for `${fce.functionName}`", fce.position)
      None
    }
    constantOperation(op, fce.expressions, vv)
  }

  private def constantOperation(op: MathOperator.Value, params: List[Expression], vv: Option[Map[String, Constant]]): Option[Constant] = {
    params.map(p => evalImpl(p, vv)).reduceLeft[Option[Constant]] { (oc, om) =>
      for {
        c <- oc
        m <- om
      } yield CompoundConstant(op, c, m)
    }
  }

  def evalForAsm(e: Expression, silent: Boolean = false): Option[Constant] = {
    e match {
      case FunctionCallExpression("label", List(VariableExpression(name))) if (!name.contains(".")) =>
        return Some(Label(name).toAddress)
      case FunctionCallExpression("label", List(VariableExpression(name))) if (name.startsWith(".")) =>
        return Some(Label(prefix + name).toAddress)
      case FunctionCallExpression("label", _) =>
        log.error("Invalid label reference", e.position)
        return Some(Constant.Zero)
      case _ =>
    }
    e match {
      case LiteralExpression(value, size) => Some(NumericConstant(value, size))
      case ConstantArrayElementExpression(c) => Some(c)
      case VariableExpression(name) =>
        val result = maybeGet[ConstantThing](name).map(_.value).orElse(maybeGet[ThingInMemory](name).map(_.toAddress))
        result match {
          case Some(x) => result
          case None =>
            if (name.startsWith(".")) Some(Label(prefix + name).toAddress)
            else {
              if (!silent) log.warn(s"$name is not known. If it is a label, consider wrapping it in label(...).", e.position)
              None
            }
        }
      case IndexedExpression(name, index) => (evalForAsm(VariableExpression(name)), evalForAsm(index)) match {
        case (Some(a), Some(b)) => Some(CompoundConstant(MathOperator.Plus, a, b).quickSimplify)
      }
      case HalfWordExpression(param, hi) => evalForAsm(param).map(c => if (hi) c.hiByte else c.loByte)
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
          case "/" =>
            constantBinaryOperationForAsm("/", MathOperator.Divide, params)
          case "%%" =>
            constantBinaryOperationForAsm("%%", MathOperator.Modulo, params)
          case ">>" =>
            constantBinaryOperationForAsm(">>", MathOperator.Shr, params)
          case "<<" =>
            constantBinaryOperationForAsm("<<", MathOperator.Shl, params)
          case "hi" =>
            oneArgFunctionForAsm(_.hiByte, params)
          case "lo" =>
            oneArgFunctionForAsm(_.loByte, params)
          case ">>>>" | ">>'" | "<<'" | ">" | "<" =>
            if (!silent) log.error(s"Operator `$name` not supported in inline assembly", e.position)
            None
          case _ if name.endsWith("=") =>
            if (!silent) log.error(s"Operator `$name` not supported in inline assembly", e.position)
            None
          case "nonet" | "sin" | "cos" | "tan" =>
            if (!silent) log.error("Function not supported in inline assembly", e.position)
            None
          case "sizeof" =>
            if (params.size == 1) {
              Some(evalSizeof(params.head))
            } else {
              log.error("Invalid number of parameters for `sizeof`", e.position)
              Some(Constant.One)
            }
          case "typeof" =>
            if (params.size == 1) {
              Some(evalTypeof(params.head))
            } else {
              log.error("Invalid number of parameters for `typeof`", e.position)
              Some(Constant.Zero)
            }
          case _ =>
            None
        }
    }
  }

  private def constantBinaryOperationForAsm(symbol: String, op: MathOperator.Value, params: List[Expression]) = {
    if (params.length != 2) {
      log.error(s"Too many operands for the $symbol operator", params.head.position)
    }
    params.map(e => evalForAsm(e)).reduceLeft[Option[Constant]] { (oc, om) =>
      for {
        c <- oc
        m <- om
      } yield CompoundConstant(op, c, m)
    }
  }

  private def constantOperationForAsm(op: MathOperator.Value, params: List[Expression]) = {
    params.map(e => evalForAsm(e)).reduceLeft[Option[Constant]] { (oc, om) =>
      for {
        c <- oc
        m <- om
      } yield CompoundConstant(op, c, m)
    }
  }

  private def oneArgFunctionForAsm(f: Constant => Constant, params: List[Expression]): Option[Constant] = {
    if (params.size != 1) {
      log.error("Too many arguments", params.headOption.flatMap(_.position))
      return None
    }
    evalForAsm(params.head).map(f).map(_.quickSimplify)
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
    if (count.exists(_ > 256)) {
      log.error(s"Enum `${stmt.name} has more than 256 constants.", stmt.position)
    }
    val t = EnumType(stmt.name, count)
    addThing(t, stmt.position)
    var value = Constant.Zero
    for((name, optValue) <- stmt.variants) {
      optValue match {
        case Some(v) =>
          value = eval(v).getOrElse(errorConstant(s"Enum constant `${stmt.name}.$name` is not a constant", Some(v), stmt.position))
        case _ =>
      }
      addThing(ConstantThing(name, value.fitInto(t), t), stmt.position)
      value += 1
    }
  }

  def registerStruct(stmt: StructDefinitionStatement): Unit = {
    stmt.fields.foreach{ f =>
      if (Environment.invalidFieldNames.contains(f.fieldName)) {
        log.error(s"Invalid field name: `${f.fieldName}`", stmt.position)
      }
    }
    addThing(StructType(stmt.name, stmt.fields, stmt.alignment.getOrElse(NoAlignment)), stmt.position)
  }

  def registerUnion(stmt: UnionDefinitionStatement): Unit = {
    stmt.fields.foreach{ f =>
      if (Environment.invalidFieldNames.contains(f.fieldName)) {
        log.error(s"Invalid field name: `${f.fieldName}`", stmt.position)
      }
    }
    addThing(UnionType(stmt.name, stmt.fields, stmt.alignment.getOrElse(NoAlignment)), stmt.position)
  }

  def getTypeAlignment(t: VariableType, path: Set[String]): MemoryAlignment = {
    val name = t.name
    if (path.contains(name)) return null
    t match {
      case s: CompoundVariableType =>
        if (s.mutableAlignment ne null) return s.mutableAlignment
        var alignment = s.baseAlignment
        for( ResolvedFieldDesc(fieldType, _, _, _) <- s.mutableFieldsWithTypes) {
          val a = getTypeAlignment(fieldType, path + name)
          if (a eq null) return null
          alignment = alignment & a
        }
        s.mutableAlignment = alignment
        alignment
      case _ => t.alignment
    }
  }

  def getTypeSize(t: VariableType, path: Set[String]): Int = {
    val name = t.name
    if (path.contains(name)) return -1
    t match {
      case s: StructType =>
        if (s.mutableSize >= 0) s.mutableSize
        else {
          val newPath = path + name
          var sum = 0
          for( ResolvedFieldDesc(fieldType, _, _, indexTypeAndCount) <- s.mutableFieldsWithTypes) {
            val fieldSize = getTypeSize(fieldType, newPath) * indexTypeAndCount.fold(1)(_._2)
            if (fieldSize < 0) return -1
            sum = fieldType.alignment.roundSizeUp(sum)
            sum += fieldSize
            sum = fieldType.alignment.roundSizeUp(sum)
          }
          s.mutableSize = sum
          if (sum > 0xff) {
            log.error(s"Struct `$name` is larger than 255 bytes")
          }
          val b = get[Type]("byte")
          var offset = 0
          for( ResolvedFieldDesc(fieldType, fieldName, _, indexTypeAndCount) <- s.mutableFieldsWithTypes) {
            offset = fieldType.alignment.roundSizeUp(offset)
            addThing(ConstantThing(s"$name.$fieldName.offset", NumericConstant(offset, 1), b), None)
            offset += getTypeSize(fieldType, newPath) * indexTypeAndCount.fold(1)(_._2)
            offset = fieldType.alignment.roundSizeUp(offset)
          }
          sum
        }
      case s: UnionType =>
        if (s.mutableSize >= 0) s.mutableSize
        else {
          val newPath = path + name
          var max = 0
          for( ResolvedFieldDesc(fieldType, _, _, indexTypeAndCount) <- s.mutableFieldsWithTypes) {
            val fieldSize = getTypeSize(fieldType, newPath) * indexTypeAndCount.fold(1)(_._2)
            if (fieldSize < 0) return -1
            max = max max fieldSize
          }
          s.mutableSize = max
          if (max > 0xff) {
            log.error(s"Union `$name` is larger than 255 bytes")
          }
          val b = get[Type]("byte")
          for (ResolvedFieldDesc(fieldType, fieldName, _, _) <- s.mutableFieldsWithTypes) {
            addThing(ConstantThing(s"$name.$fieldName.offset", NumericConstant(0, 1), b), None)
          }
          max
        }
      case _ => t.size
    }
  }

  def collectPointies(stmts: Seq[Statement]): Set[String] = {
    val pointies: mutable.Set[String] = new mutable.HashSet()
    pointies ++= stmts.flatMap(_.getAllPointies)
    pointies ++= getAliases.filterKeys(pointies).values
    log.trace("Collected pointies: " + pointies)
    pointies.toSet
  }

  def registerFunction(stmt: FunctionDeclarationStatement, options: CompilationOptions): Unit = {
    val pointies = collectPointies(stmt.statements.getOrElse(Seq.empty))
    pointiesUsed(stmt.name) = pointies
    val w = get[Type]("word")
    val p = get[Type]("pointer")
    val name = stmt.name
    if (Environment.constOnlyBuiltinFunction(name)) {
      log.error(s"Cannot redefine a built-in function `$name`", stmt.position)
    }
    val resultType = get[Type](stmt.resultType)
    if (stmt.name == "main") {
      if (stmt.resultType != "void" && options.flag(CompilationFlag.UselessCodeWarning)) {
        log.warn("`main` should return `void`.", stmt.position)
      }
      if (stmt.params.nonEmpty && options.flag(CompilationFlag.BuggyCodeWarning)) {
        log.warn("`main` shouldn't have parameters.", stmt.position)
      }
    }

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

    val isTrampoline = stmt.name.endsWith(".trampoline")
    val env = if (isTrampoline) {
      // let's hope nothing goes wrong with this:
      get[FunctionInMemory](stmt.name.stripSuffix(".trampoline")).environment
    } else {
      new Environment(Some(this), name + "$", cpuFamily, options)
    }
    stmt.params.foreach(p => env.registerParameter(p, options, pointies))
    def params: ParamSignature = if (stmt.assembly || stmt.isMacro) {
      AssemblyOrMacroParamSignature(stmt.params.map {
        pd =>
          val typ = env.get[Type](pd.typ)
          pd.assemblyParamPassingConvention match {
            case ByVariable(vn) =>
              AssemblyOrMacroParam(typ, env.get[MemoryVariable](vn), AssemblyParameterPassingBehaviour.Copy)
            case ByMosRegister(reg) =>
              AssemblyOrMacroParam(typ, RegisterVariable(reg, typ), AssemblyParameterPassingBehaviour.Copy)
            case ByZRegister(reg) =>
              AssemblyOrMacroParam(typ, ZRegisterVariable(reg, typ), AssemblyParameterPassingBehaviour.Copy)
            case ByM6809Register(reg) =>
              AssemblyOrMacroParam(typ, M6809RegisterVariable(reg, typ), AssemblyParameterPassingBehaviour.Copy)
            case ByConstant(vn) =>
              AssemblyOrMacroParam(typ, Placeholder(vn, typ), AssemblyParameterPassingBehaviour.ByConstant)
            case ByReference(vn) =>
              AssemblyOrMacroParam(typ, Placeholder(vn, typ), AssemblyParameterPassingBehaviour.ByReference)
            case ByLazilyEvaluableExpressionVariable(vn) =>
              AssemblyOrMacroParam(typ, Placeholder(vn, typ), AssemblyParameterPassingBehaviour.Eval)
          }
      })
    } else {
      NormalParamSignature(stmt.params.map { pd =>
        env.get[VariableInMemory](pd.assemblyParamPassingConvention.asInstanceOf[ByVariable].name)
      })
    }
    var hasElidedReturnVariable = false
    val hasReturnVariable = resultType.size > Cpu.getMaxSizeReturnableViaRegisters(options.platform.cpu, options)
    if (hasReturnVariable) {
      registerVariable(VariableDeclarationStatement(stmt.name + ".return", stmt.resultType, None, global = true, stack = false, constant = false, volatile = false, register = false, None, None, Set.empty, None), options, isPointy = false)
    }
    stmt.statements match {
      case None =>
        stmt.address match {
          case None =>
            log.error(s"Extern function `${stmt.name}`needs an address", stmt.position)
          case Some(a) =>
            val addr = eval(a).getOrElse(errorConstant(s"Address of `${stmt.name}` is not a constant", Some(a), stmt.position))
            val mangled = ExternFunction(
              name,
              resultType,
              params,
              addr,
              env,
              prepareFunctionOptimizationHints(options, stmt),
              stmt.bank
            )
            addThing(mangled, stmt.position)
            registerAddressConstant(mangled, stmt.position, options, None)
            addThing(ConstantThing(name + '`', addr, w), stmt.position)
        }

      case Some(statements) =>
        statements.foreach {
          case v: VariableDeclarationStatement => env.registerVariable(v, options, pointies(v.name))
          case a: ArrayDeclarationStatement => env.registerArray(a, options)
          case _ => ()
        }
        def scanForLabels(statement: Statement): Unit = statement match {
          case c: CompoundStatement => c.getChildStatements.foreach(scanForLabels)
          case LabelStatement(labelName) => env.knownLocalLabels += (labelName -> statement.position)
          case _ => ()
        }
        statements.foreach(scanForLabels)
        for ((knownLabel, position) <- env.knownLocalLabels) {
          env.addThing(knownLabel, ConstantThing(env.prefix + knownLabel, Label(env.prefix + knownLabel).toAddress, p), position)
        }

        // not all in-function gotos are allowed; warn about the provably wrong ones:
        def checkLabels(statements: Seq[Statement]) = {
          def getAllSafeLabels(statements: Seq[Statement]): Seq[String] = statements.flatMap {
            case _: ForEachStatement => Nil
            case c: CompoundStatement => getAllSafeLabels(c.getChildStatements)
            case LabelStatement(labelName) => Seq(labelName)
            case _ => Nil
          }
          def getAllSafeGotos(statements: Seq[Statement]):Seq[String] = statements.flatMap {
            case _: ForEachStatement => Nil
            case c: CompoundStatement => getAllSafeGotos(c.getChildStatements)
            case GotoStatement(VariableExpression(labelName)) if env.knownLocalLabels.exists(_._1.==(labelName)) => Seq(labelName)
            case _ => Nil
          }
          def doOnlyCheck(position: Option[Position], statements: Seq[Statement]): Unit = {
            val l = getAllSafeLabels(statements).toSet
            val g = getAllSafeGotos(statements).toSet
            val bad = g.&(env.knownLocalLabels.map(_._1)).--(l)
            if (bad.nonEmpty) {
              if (options.flag(CompilationFlag.BuggyCodeWarning)) log.warn("Detected cross-loop gotos to labels " + bad.mkString(", "), position)
            }
          }
          def recurse(statements: Seq[Statement]):Unit = statements.foreach {
            case c: ForEachStatement =>
              doOnlyCheck(c.position, c.body)
              recurse(c.body)
            case c: CompoundStatement => recurse(c.getChildStatements)
            case _ => Nil
          }
          doOnlyCheck(stmt.position, statements)
          recurse(statements)
        }
        checkLabels(statements)

        val executableStatements = statements.flatMap {
          case e: ExecutableStatement => Some(e)
          case _ => None
        }
        if (hasReturnVariable) {
          val set = getReturnedVariables(executableStatements)
          if (set.size == 1) {
            env.maybeGet[Variable](set.head) match {
              case Some(v: MemoryVariable) =>
                if (!v.isVolatile && v.typ == resultType && v.alloc == VariableAllocationMethod.Auto) {
                  env.coerceLocalVariableIntoGlobalVariable(set.head, stmt.name + ".return")
                  hasElidedReturnVariable = true
                }
              case _ =>
            }
          }
        }
        val paramForAutomaticReturn: List[Option[Expression]] = if (stmt.isMacro || stmt.assembly) {
          Nil
        } else if (executableStatements.isEmpty) {
          List(None)
        } else {
          executableStatements.last match {
            case s if s.isValidFunctionEnd => Nil
            case WhileStatement(VariableExpression(tr), _, _, _) =>
              if (resultType.size > 0 && env.getBooleanConstant(tr).contains(true)) {
                List(Some(LiteralExpression(0, 1))) // TODO: what if the loop is breakable?
              } else List(None)
            case DoWhileStatement(_, _, VariableExpression(tr), _) =>
              if (resultType.size > 0 && env.getBooleanConstant(tr).contains(true)) {
                List(Some(LiteralExpression(0, 1))) // TODO: what if the loop is breakable?
              } else List(None)
            case _ =>
              // None so the compiler warns
              List(None)
          }
        }
        if (stmt.isMacro) {
          if (stmt.bank.isDefined) {
            log.error("Macro functions cannot be in a defined segment", stmt.position)
          }
          val mangled = MacroFunction(
            name,
            resultType,
            params.asInstanceOf[AssemblyOrMacroParamSignature],
            stmt.assembly,
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
            stmt.address.map(a => this.eval(a).getOrElse(errorConstant(s"Address of `${stmt.name}` is not a constant", Some(a), a.position))),
            executableStatements ++ paramForAutomaticReturn.map(param => ReturnStatement(param).pos(executableStatements.lastOption.fold(stmt.position)(_.position))),
            hasElidedReturnVariable = hasElidedReturnVariable,
            interrupt = stmt.interrupt,
            kernalInterrupt = stmt.kernalInterrupt,
            inAssembly = stmt.assembly,
            reentrant = stmt.reentrant,
            isConstPure = stmt.constPure,
            position = stmt.position,
            declaredBank = stmt.bank,
            optimizationHints = prepareFunctionOptimizationHints(options, stmt),
            alignment = stmt.alignment.getOrElse(if (name == "main") NoAlignment else defaultFunctionAlignment(options, hot = true)) // TODO: decide actual hotness in a smarter way
          )
          addThing(mangled, stmt.position)
          registerAddressConstant(mangled, stmt.position, options, None)
          if (mangled.isConstPure) {
            ConstPureFunctions.checkConstPure(env, mangled)
          }
        }
    }
  }

  private def getFunctionPointerType(f: FunctionInMemory) = f.params.types match {
    case List() =>
      if (f.returnType == VoidType) get[Type]("pointer.kernal_interrupt")
      else get[Type]("function.void.to." + f.returnType.name)
    case p :: _ => // TODO: this only handles one type though!
      get[Type]("function." + p.name + ".to." + f.returnType.name)
  }

  def getTextLiteralArrayName(literal: TextLiteralExpression): String = {
    val name = "textliteral$" ++ literal.characters.flatMap {
      case LiteralExpression(n, _) =>
        f"$n%02x"
      case _ => ???
    }
    if (maybeGet[Thing](name).isEmpty) {
      root.registerArray(ArrayDeclarationStatement(name, None, None, "byte", None, const = true, Some(LiteralContents(literal.characters)), Set.empty, None, options.isBigEndian).pos(literal.position), options)
    }
    name
  }

  private def registerAddressConstant(thing: ThingInMemory, position: Option[Position], options: CompilationOptions, targetType: Option[Type]): Unit = {
    val b = get[Type]("byte")
    val w = get[Type]("word")
    val ptr = get[Type]("pointer")
    val segment = thing.bank(options)
    for (bankNumber <- options.platform.bankNumbers.get(segment)) {
      addThing(ConstantThing(thing.name + ".segment", NumericConstant(bankNumber, 1), b), position)
      addThing(ConstantThing(thing.name + ".segment.bank", NumericConstant(bankNumber, 1), b), position)
    }
    for (bankFill <- options.platform.bankFill.get(segment)) {
      addThing(ConstantThing(thing.name + ".segment.fill", NumericConstant(bankFill, 1), b), position)
    }
    addThing(ConstantThing(thing.name + ".segment.start",  UnexpandedConstant(s"segment.$segment.start", 2), ptr), position)
    addThing(ConstantThing(thing.name + ".segment.codeend", UnexpandedConstant(s"segment.$segment.codeend", 2), ptr), position)
    addThing(ConstantThing(thing.name + ".segment.datastart",  UnexpandedConstant(s"segment.$segment.datastart", 2), ptr), position)
    addThing(ConstantThing(thing.name + ".segment.heapstart",  UnexpandedConstant(s"segment.$segment.heapstart", 2), ptr), position)
    addThing(ConstantThing(thing.name + ".segment.end", UnexpandedConstant(s"segment.$segment.end", 2), ptr), position)
    addThing(ConstantThing(thing.name + ".segment.length", UnexpandedConstant(s"segment.$segment.length", 2), w), position)
    if (!thing.zeropage && options.flag(CompilationFlag.LUnixRelocatableCode)) {
      val relocatable = UninitializedMemoryVariable(thing.name + ".addr", w, VariableAllocationMethod.Static, None, Set.empty, defaultVariableAlignment(options, 2), isVolatile = false)
      val addr = relocatable.toAddress
      addThing(relocatable, position)
      addThing(RelativeVariable(thing.name + ".addr.hi", addr + 1, b, zeropage = false, None, isVolatile = false), position)
      addThing(RelativeVariable(thing.name + ".addr.lo", addr, b, zeropage = false, None, isVolatile = false), position)
      targetType.foreach {tt =>
        val typedPointer = RelativeVariable(thing.name + ".pointer", addr, PointerType("pointer."+tt.name, tt.name, Some(tt)), zeropage = false, None, isVolatile = false)
        addThing(typedPointer, position)
        addThing(RelativeVariable(thing.name + ".pointer.hi", addr + 1, b, zeropage = false, None, isVolatile = false), position)
        addThing(RelativeVariable(thing.name + ".pointer.lo", addr, b, zeropage = false, None, isVolatile = false), position)
      }
      val rawaddr = thing.toAddress
      addThing(ConstantThing(thing.name + ".rawaddr", rawaddr, ptr), position)
      addThing(ConstantThing(thing.name + ".rawaddr.hi", rawaddr.hiByte, b), position)
      addThing(ConstantThing(thing.name + ".rawaddr.lo", rawaddr.loByte, b), position)
      thing match {
        case f: FunctionInMemory if f.canBePointedTo =>
          val actualAddr = if (f.requiresTrampoline(options)) {
            registerFunctionTrampoline(f).toAddress
          } else {
            addr
          }
          val typedPointer = RelativeVariable(thing.name + ".pointer", actualAddr, getFunctionPointerType(f), zeropage = false, None, isVolatile = false)
          addThing(typedPointer, position)
          addThing(RelativeVariable(thing.name + ".pointer.hi", actualAddr + 1, b, zeropage = false, None, isVolatile = false), position)
          addThing(RelativeVariable(thing.name + ".pointer.lo", actualAddr, b, zeropage = false, None, isVolatile = false), position)
        case f: FunctionInMemory if f.interrupt =>
          val typedPointer = RelativeVariable(thing.name + ".pointer", f.toAddress, InterruptPointerType, zeropage = false, None, isVolatile = false)
          addThing(typedPointer, position)
          addThing(RelativeVariable(thing.name + ".pointer.hi", f.toAddress + 1, b, zeropage = false, None, isVolatile = false), position)
          addThing(RelativeVariable(thing.name + ".pointer.lo", f.toAddress, b, zeropage = false, None, isVolatile = false), position)
        case _ =>
      }
    } else {
      val addr = thing.toAddress
      addThing(ConstantThing(thing.name + ".addr", addr, ptr), position)
      addThing(ConstantThing(thing.name + ".addr.hi", addr.hiByte, b), position)
      addThing(ConstantThing(thing.name + ".addr.lo", addr.loByte, b), position)
      addThing(ConstantThing(thing.name + ".rawaddr", addr, ptr), position)
      addThing(ConstantThing(thing.name + ".rawaddr.hi", addr.hiByte, b), position)
      addThing(ConstantThing(thing.name + ".rawaddr.lo", addr.loByte, b), position)
      targetType.foreach { tt =>
        val pointerType = PointerType("pointer." + tt.name, tt.name, Some(tt))
        addThing(ConstantThing(thing.name + ".pointer", addr, pointerType), position)
        addThing(ConstantThing(thing.name + ".pointer.hi", addr.hiByte, b), position)
        addThing(ConstantThing(thing.name + ".pointer.lo", addr.loByte, b), position)
      }
      thing match {
        case f: FunctionInMemory if f.canBePointedTo =>
          val pointerType = getFunctionPointerType(f)
          val actualAddr = if (f.requiresTrampoline(options)) {
            registerFunctionTrampoline(f).toAddress
          } else {
            addr
          }
          addThing(ConstantThing(thing.name + ".pointer", actualAddr, pointerType), position)
          addThing(ConstantThing(thing.name + ".pointer.hi", actualAddr.hiByte, b), position)
          addThing(ConstantThing(thing.name + ".pointer.lo", actualAddr.loByte, b), position)
        case f: FunctionInMemory if f.interrupt =>
          addThing(ConstantThing(thing.name + ".pointer", f.toAddress, InterruptPointerType), position)
          addThing(ConstantThing(thing.name + ".pointer.hi", f.toAddress.hiByte, b), position)
          addThing(ConstantThing(thing.name + ".pointer.lo", f.toAddress.loByte, b), position)
        case _ =>
      }
    }
  }

  def registerFunctionTrampoline(function: FunctionInMemory): FunctionInMemory = {
    options.platform.cpuFamily match {
      case CpuFamily.M6502 =>
        function.params match {
          case NormalParamSignature(List(param)) =>
            import Opcode._
            import AddrMode._
            val localNameForParam = param.name.stripPrefix(function.name + '$')
            root.registerFunction(FunctionDeclarationStatement(
              function.name + ".trampoline",
              function.returnType.name,
              List(ParameterDeclaration(param.typ.name, ByMosRegister(MosRegister.AX))),
              Some(function.bank(options)),
              None, Set.empty, None,
              Some(List(
                MosAssemblyStatement(STA, Absolute, VariableExpression(localNameForParam), Elidability.Volatile),
                MosAssemblyStatement(STX, Absolute, VariableExpression(localNameForParam) #+# 1, Elidability.Volatile),
                MosAssemblyStatement(JMP, Absolute, VariableExpression(function.name + ".addr"), Elidability.Elidable)
              )),
              isMacro = false,
              inlinable = Some(false),
              assembly = true,
              interrupt = false,
              kernalInterrupt = false,
              reentrant = false,
              constPure = function.isConstPure
            ), options)
            get[FunctionInMemory](function.name + ".trampoline")
        }
      case _ => function
    }
  }

  def registerParameter(stmt: ParameterDeclaration, options: CompilationOptions, pointies: Set[String]): Unit = {
    val typ = get[Type](stmt.typ)
    val b = get[Type]("byte")
    val w = get[Type]("word")
    val p = get[Type]("pointer")
    stmt.assemblyParamPassingConvention match {
      case ByVariable(name) =>
        val zp = pointies(name) // TODO
        val allocationMethod =
          if (pointies(name)) VariableAllocationMethod.Zeropage
          else if (typ.isPointy && options.platform.cpuFamily == CpuFamily.M6502) VariableAllocationMethod.Register
          else VariableAllocationMethod.Auto
        val v = UninitializedMemoryVariable(prefix + name, typ, allocationMethod, None, Set.empty, defaultVariableAlignment(options, 2), isVolatile = false)
        addThing(v, stmt.position)
        registerAddressConstant(v, stmt.position, options, Some(typ))
        val addr = v.toAddress
        for(Subvariable(suffix, offset, vol, t, arraySize) <- getSubvariables(typ)) {
          if (arraySize.isDefined) ??? // TODO
          val subv = RelativeVariable(v.name + suffix, addr + offset, t, zeropage = zp, None, isVolatile = v.isVolatile || vol)
          addThing(subv, stmt.position)
          registerAddressConstant(subv, stmt.position, options, Some(t))
        }
      case ByLazilyEvaluableExpressionVariable(_) => ()
      case ByMosRegister(_) => ()
      case ByZRegister(_) => ()
      case ByM6809Register(_) => ()
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

  def extractStructArrayContents(expr: Expression, targetType: Option[Type]): List[Expression] = {
    (targetType, expr) match {

      case (Some(tt: StructType), FunctionCallExpression(fname, fieldValues)) =>
        maybeGet[Thing](fname) match {
          case Some(tt2:StructType) if tt2.name == tt.name =>
            if (tt.fields.length != fieldValues.length) {
              log.error(s"Invalid number of struct fields for struct const `${tt.name}`", fieldValues.headOption.flatMap(_.position))
              List.fill(tt.size)(LiteralExpression(0, 1))
            } else {
              tt.fields.zip(fieldValues).flatMap {
                case (FieldDesc(fieldTypeName, _, _, count), expr) =>
                  // TODO: handle array fields
                  if (count.isDefined) ???
                  extractStructArrayContents(expr, Some(get[Type](fieldTypeName)))
              }
            }
          case _ =>
            log.error(s"Invalid struct type: `$fname`", expr.position)
            List.fill(tt.size)(LiteralExpression(0, 1))
        }

      case (Some(tt: StructType), _) =>
        log.error(s"Invalid struct initializer for type `${tt.name}`", expr.position)
        List.fill(tt.size)(LiteralExpression(0, 1))

      case (Some(tt: PlainType), _) =>
        tt.size match {
          case 1 => List(expr)
          case 2 => List(FunctionCallExpression("lo", List(expr)), FunctionCallExpression("hi", List(expr)))
          case n => List.tabulate(n)(i => FunctionCallExpression("lo", List(FunctionCallExpression(">>", List(expr, LiteralExpression(8 * i, 1))))))
        }

      case (Some(tt: PointerType), _) => List(FunctionCallExpression("lo", List(expr)), FunctionCallExpression("hi", List(expr)))
      case (Some(tt: EnumType), _) => List(FunctionCallExpression("byte", List(expr)))

      case (Some(tt), _) =>
        log.error("Invalid field type for use in array initializers", expr.position)
        List.fill(tt.size)(LiteralExpression(0, 1))

      case (None, FunctionCallExpression(fname, fieldValues)) =>
        maybeGet[Thing](fname) match {
          case Some(tt:StructType) =>
            if (tt.fields.length != fieldValues.length) {
              log.error(s"Invalid number of struct fields for struct const `${tt.name}`", fieldValues.headOption.flatMap(_.position))
              List.fill(tt.size)(LiteralExpression(0, 1))
            } else {
              tt.fields.zip(fieldValues).flatMap {
                case (FieldDesc(fieldTypeName, _, _, count), expr) =>
                  // TODO: handle array fields
                  if (count.isDefined) ???
                  extractStructArrayContents(expr, Some(get[Type](fieldTypeName)))
              }
            }
          case _ =>
            log.error(s"Invalid struct type: `$fname`", expr.position)
            Nil
        }

      case _ =>
        log.error(s"Invalid struct initializer for unknown type", expr.position)
        Nil
    }
  }

  def checkIfArrayContentsAreSimple(xs: CombinedContents): Unit = {
    xs.contents.foreach{
      case x:CombinedContents => checkIfArrayContentsAreSimple(x)
      case x:LiteralContents => ()
      case x => log.error(s"Invalid struct array contents", x.position)
    }
  }

  //noinspection ScalaUnusedExpression
  private def markAsConstantsThatShouldHaveBeenImportedEarlier(node: Expression): Unit = {
    node match {
      case VariableExpression(v) =>
        if (eval(node).isEmpty && evalForAsm(node, silent = true).isEmpty) {
          log.error(s"The constant $v is undefined", node.position)
          log.info("Did you forget to import an appropriate module?")
          constantsThatShouldHaveBeenImportedEarlier.addBinding(v.takeWhile(_ != '.').mkString(""), node.position)
        }
      case FunctionCallExpression(_, params) =>
        params.foreach(markAsConstantsThatShouldHaveBeenImportedEarlier)
      case SumExpression(params, _) =>
        params.foreach(i => markAsConstantsThatShouldHaveBeenImportedEarlier(i._2))
      case SeparateBytesExpression(h, l) =>
        markAsConstantsThatShouldHaveBeenImportedEarlier(h)
        markAsConstantsThatShouldHaveBeenImportedEarlier(l)
      case IndexedExpression(a, i) =>
        constantsThatShouldHaveBeenImportedEarlier.addBinding(a, node.position)
        markAsConstantsThatShouldHaveBeenImportedEarlier(i)
      case DerefExpression(p, _, _, _) =>
        markAsConstantsThatShouldHaveBeenImportedEarlier(p)
      case DerefDebuggingExpression(p, _) =>
        markAsConstantsThatShouldHaveBeenImportedEarlier(p)
      case _ =>
        // not a variable
    }
  }

  def extractArrayContents(contents1: ArrayContents): List[Expression] = contents1 match {
    case LiteralContents(xs) => xs
    case FileChunkContents(filePath, startE, lengthE) =>
      val data = Files.readAllBytes(filePath)
      val p = contents1.position
      val slice = (eval(startE).map(_.quickSimplify), lengthE.map(l => eval(l).map(_.quickSimplify))) match {
        case (Some(NumericConstant(start, _)), Some(Some(NumericConstant(length, _)))) =>
          if (data.length < start) {
            log.error(s"File $filePath is shorter (${data.length} B) that the start offset $start", p)
            Array.fill(length.toInt)(0.toByte)
          } else if (data.length < start + length) {
            log.error(s"File $filePath is shorter (${data.length} B) that the start offset plus length ${start + length}", p)
            Array.fill(length.toInt)(0.toByte)
          } else {
            data.slice(start.toInt, start.toInt + length.toInt)
          }
        case (Some(NumericConstant(start, _)), None) =>
          if (data.length < start) {
            log.error(s"File $filePath is shorter (${data.length} B) that the start offset $start", p)
            Array[Byte](0)
          } else {
            data.drop(start.toInt)
          }
        case (None, Some(Some(_))) =>
          log.error(s"Start offset is not a constant", p)
          Array[Byte](0)
        case (_, Some(None)) =>
          log.error(s"Length is not a constant", p)
          Array[Byte](0)
        case (None, Some(None)) =>
          log.error(s"Start offset and length are not constants", p)
          Array[Byte](0)
      }
      slice.map(c => LiteralExpression(c & 0xff, 1)).toList
    case CombinedContents(xs) => xs.flatMap(extractArrayContents)
    case pc@ProcessedContents("struct", xs: CombinedContents) =>
      checkIfArrayContentsAreSimple(xs)
      xs.getAllExpressions(options.isBigEndian).flatMap(x => extractStructArrayContents(x, None))
    case pc@ProcessedContents("struct", _) =>
      log.error(s"Invalid struct array contents", pc.position)
      Nil
    case pc@ProcessedContents(f, xs) => pc.getAllExpressions(options.isBigEndian)
    case flc@ForLoopContents(v, start, end, direction, body) =>
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
          log.error("Array range bounds cannot be evaluated", flc.position.orElse(flc.start.position))
          Nil
        case (a, b) =>
          if (a.isEmpty) {
            log.error("Non-constant array range bounds",  flc.start.position)
          }
          if (b.isEmpty) {
            log.error("Non-constant array range bounds",  flc.`end`.position)
          }
          if (a.isEmpty){
            markAsConstantsThatShouldHaveBeenImportedEarlier(flc.start)
          }
          if (b.isEmpty){
            markAsConstantsThatShouldHaveBeenImportedEarlier(flc.`end`)
          }
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

  def prepareFunctionOptimizationHints(options: CompilationOptions, stmt: FunctionDeclarationStatement): Set[String] = {
    if (!options.flag(CompilationFlag.UseOptimizationHints)) return Set.empty
    def warn(msg: String): Unit = {
      if (options.flag(CompilationFlag.UnsupportedOptimizationHintWarning)){
        log.warn(msg, stmt.position)
      }
    }
    val filteredFlags = stmt.optimizationHints.flatMap{
      case f@("hot" | "cold" | "idempotent" | "preserves_memory" | "inline" | "odd" | "even") =>
        Seq(f)
      case f@("preserves_a" | "preserves_x" | "preserves_y" | "preserves_c")
        if options.platform.cpuFamily == CpuFamily.M6502 =>
        if (stmt.statements.isDefined && !stmt.assembly) {
          warn(s"Cannot use the $f optimization hint on non-assembly functions")
          Nil
        } else {
          Seq(f)
        }
      case f@("preserves_a" | "preserves_b" | "preserves_d" | "preserves_c" | "preserves_x" | "preserves_y" | "preserves_u")
        if options.platform.cpuFamily == CpuFamily.M6809 =>
        if (stmt.statements.isDefined && !stmt.assembly) {
          warn(s"Cannot use the $f optimization hints on non-assembly functions")
          Nil
        } else {
          Seq(f)
        }
      case f@("preserves_a" | "preserves_bc" | "preserves_de" | "preserves_hl" | "preserves_cf")
        if options.platform.cpuFamily == CpuFamily.I80 =>
        if (stmt.statements.isDefined && !stmt.assembly) {
          warn(s"Cannot use the $f optimization hints on non-assembly functions")
          Nil
        } else {
          Seq(f)
        }
      case f@("preserves_dp")
        if options.platform.cpuFamily == CpuFamily.M6809 =>
        Seq(f)
      case f =>
        warn(s"Unsupported function optimization hint: $f")
        Nil
    }
    if (filteredFlags("hot") && filteredFlags("cold")) {
      warn(s"Conflicting optimization hints used: `hot` and `cold`")
    }
    if (filteredFlags("even") && filteredFlags("odd")) {
      warn(s"Conflicting optimization hints used: `even` and `odd`")
    }
    if (filteredFlags("even") || filteredFlags("odd")) {
      maybeGet[Type](stmt.resultType) match {
        case Some(t) if t.size < 1 =>
          warn(s"Cannot use `even` or `odd` hints with an empty return type")
        case Some(t: CompoundVariableType) =>
          warn(s"Cannot use `even` or `odd` hints with a compound return type")
        case _ =>
      }
    }
    filteredFlags
  }

  def prepareVariableOptimizationHints(options: CompilationOptions, stmt: VariableDeclarationStatement): Set[String] = {
    if (!options.flag(CompilationFlag.UseOptimizationHints)) return Set.empty
    def warn(msg: String): Unit = {
      if (options.flag(CompilationFlag.UnsupportedOptimizationHintWarning)){
        log.warn(msg, stmt.position)
      }
    }
    val filteredFlags = stmt.optimizationHints.flatMap{
      case f@("odd" | "even") =>
        Seq(f)
      case f =>
        warn(s"Unsupported variable optimization hint: $f")
        Nil
    }
    if (filteredFlags("even") && filteredFlags("odd")) {
      warn(s"Conflicting optimization hints used: `even` and `odd`")
    }
    filteredFlags
  }

  //noinspection UnnecessaryPartialFunction
  def prepareArrayOptimizationHints(options: CompilationOptions, stmt: ArrayDeclarationStatement): Set[String] = {
    if (!options.flag(CompilationFlag.UseOptimizationHints)) return Set.empty
    val filteredFlags: Set[String] = stmt.optimizationHints.flatMap{
      case f =>
        log.warn(s"Unsupported array optimization hint: $f", stmt.position)
        Nil
    }
    filteredFlags
  }

  def registerArray(stmt: ArrayDeclarationStatement, options: CompilationOptions): Unit = {
    new OverflowDetector(this, options).detectOverflow(stmt)
    if (options.flag(CompilationFlag.LUnixRelocatableCode) && stmt.alignment.exists(_.isMultiplePages)) {
      log.error("Invalid alignment for LUnix code", stmt.position)
    }
    if (stmt.elements.isDefined && !stmt.const && parent.isDefined) {
      log.error(s"Local array `${stmt.name}` cannot be initialized if it's not const", stmt.position)
    }
    val arrayName = prefix + stmt.name
    val b = get[VariableType]("byte")
    val w = get[VariableType]("word")
    val p = get[Type]("pointer")
    val e = get[VariableType](stmt.elementType)
    if (e.size < 1 && e.size > 127) {
      log.error(s"Array elements should be of size between 1 and 127, `${e.name}` is of size ${e.size}", stmt.position)
    }
    stmt.elements match {
      case None =>
        if (stmt.const && stmt.address.isEmpty) {
          log.error(s"Constant array `${stmt.name}` without contents nor address", stmt.position)
        }
        val l = stmt.length match {
          case None =>
            log.error(s"Array `${stmt.name}` without size nor contents", stmt.position)
            LiteralExpression(1,1)
          case Some(l) => l
        }
        // array arr[...]
        val address = stmt.address.map(a => eval(a).getOrElse(log.fatal(s"Array `${stmt.name}` has non-constant address", stmt.position)))
        val (indexType, lengthConst) = l match {
          case VariableExpression(name) =>
            maybeGet[Type](name) match {
              case Some(typ@EnumType(_, Some(count))) =>
                typ -> NumericConstant(count, Constant.minimumSize(count))
              case Some(typ) =>
                log.error(s"Type $name cannot be used as an array index", l.position)
                w -> Constant.Zero
              case _ =>
                val constant = eval(l).getOrElse(errorConstant(s"Array `${stmt.name}` has non-constant length", Some(l), stmt.position))
                w -> constant
            }
          case _ =>
            val constant = eval(l).getOrElse(errorConstant(s"Array `${stmt.name}` has non-constant length", Some(l), stmt.position))
            w -> constant
        }
        lengthConst match {
              case NumericConstant(length, _) =>
                if (length > 0xffff || length < 0) log.error(s"Array `${stmt.name}` has invalid length", stmt.position)
                val alignment = stmt.alignment.getOrElse(defaultArrayAlignment(options, length))
                val array = address match {
                  case None => UninitializedArray(arrayName + ".array", length.toInt,
                              declaredBank = stmt.bank, indexType, e, stmt.const, prepareArrayOptimizationHints(options, stmt), alignment)
                  case Some(aa) => RelativeArray(arrayName + ".array", aa, length.toInt,
                              declaredBank = stmt.bank, indexType, e, stmt.const)
                }
                addThing(array, stmt.position)
                registerAddressConstant(UninitializedMemoryVariable(arrayName, p, VariableAllocationMethod.None, stmt.bank, Set.empty, alignment, isVolatile = false), stmt.position, options, Some(e))
                val a = address match {
                  case None => array.toAddress
                  case Some(aa) => aa
                }
                registerArrayAddresses(arrayName, stmt.bank, a, indexType, e, length.toInt, alignment, stmt.position)
              case _ => log.error(s"Array `${stmt.name}` has weird length", stmt.position)
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
                    w -> eval(l).getOrElse(errorConstant(s"Array `${stmt.name}` has non-constant length", Some(l), stmt.position))
                }
              case _ =>
                w -> eval(l).getOrElse(errorConstant(s"Array `${stmt.name}` has non-constant length", Some(l), stmt.position))
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
        val alignment = stmt.alignment.getOrElse(defaultArrayAlignment(options, length)) & e.alignment
        val address = stmt.address.map(a => eval(a).getOrElse(errorConstant(s"Array `${stmt.name}` has non-constant address", Some(a), stmt.position)))
        for (element <- contents) {
          AbstractExpressionCompiler.checkAssignmentTypeLoosely(this, element, e)
        }
        val array = InitializedArray(arrayName + ".array", address, contents, declaredBank = stmt.bank, indexType, e, readOnly = stmt.const, prepareArrayOptimizationHints(options, stmt), alignment)
        if (!stmt.const && options.platform.ramInitialValuesBank.isDefined && array.bank(options) != "default") {
          log.error(s"Preinitialized writable array `${stmt.name}` has to be in the default segment.", stmt.position)
        }
        addThing(array, stmt.position)
        val a = address match {
          case None => array.toAddress
          case Some(aa) => aa
        }
        registerArrayAddresses(arrayName, stmt.bank, a, indexType, e, length, alignment, stmt.position)
    }
  }

  def registerArrayAddresses(
                              arrayName: String,
                              declaredBank: Option[String],
                              address: Constant,
                              indexType: Type,
                              elementType: Type,
                              length: Int,
                              alignment: MemoryAlignment,
                              position: Option[Position]): Unit = {
    val p = get[Type]("pointer")
    val b = get[Type]("byte")
    val w = get[Type]("word")
    registerAddressConstant(UninitializedMemoryVariable(arrayName, p, VariableAllocationMethod.None,
      declaredBank = declaredBank, Set.empty, alignment, isVolatile = false), position, options, Some(elementType))
    addThing(RelativeVariable(arrayName + ".first", address, elementType, zeropage = false,
      declaredBank = declaredBank, isVolatile = false), position)
    if (options.flag(CompilationFlag.LUnixRelocatableCode)) {
      val b = get[Type]("byte")
      val w = get[Type]("word")
      val relocatable = UninitializedMemoryVariable(arrayName, w, VariableAllocationMethod.Static, None, Set.empty, NoAlignment, isVolatile = false)
      val addr = relocatable.toAddress
      addThing(relocatable, position)
      addThing(RelativeVariable(arrayName + ".array.hi", addr + 1, b, zeropage = false, None, isVolatile = false), position)
      addThing(RelativeVariable(arrayName + ".array.lo", addr, b, zeropage = false, None, isVolatile = false), position)
    } else {
      addThing(ConstantThing(arrayName, address, p), position)
      addThing(ConstantThing(arrayName + ".hi", address.hiByte.quickSimplify, b), position)
      addThing(ConstantThing(arrayName + ".lo", address.loByte.quickSimplify, b), position)
      addThing(ConstantThing(arrayName + ".array.hi", address.hiByte.quickSimplify, b), position)
      addThing(ConstantThing(arrayName + ".array.lo", address.loByte.quickSimplify, b), position)
    }
    if (length < 256) {
      addThing(ConstantThing(arrayName + ".length", NumericConstant(length, 1), b), position)
    } else {
      addThing(ConstantThing(arrayName + ".length", NumericConstant(length, 2), w), position)
    }
    if (length > 0 && indexType.isArithmetic) {
      if (length <= 256) {
        addThing(ConstantThing(arrayName + ".lastindex", NumericConstant(length - 1, 1), b), position)
      } else {
        addThing(ConstantThing(arrayName + ".lastindex", NumericConstant(length - 1, 2), w), position)
      }
    }
  }

  def registerVariable(stmt: VariableDeclarationStatement, options: CompilationOptions, isPointy: Boolean): Unit = {
    new OverflowDetector(this, options).detectOverflow(stmt)
    val name = stmt.name
    val position = stmt.position
    if (name == "" || name.contains(".") && !name.contains(".return")) {
      log.warn(s"Invalid variable name: $name. Please report a bug.", position)
    }
    if (stmt.stack && parent.isEmpty) {
      if (stmt.stack && stmt.global) log.error(s"`$name` is static or global and cannot be on stack", position)
    }
    val b = get[Type]("byte")
    val w = get[Type]("word")
    val typ = get[VariableType](stmt.typ)
    val alignment = stmt.alignment.getOrElse(defaultVariableAlignment(options, typ.size)) & typ.alignment
    if (stmt.constant) {
      val invalidUsesBefore = constantsThatShouldHaveBeenImportedEarlier.get(stmt.name)
      if (invalidUsesBefore.nonEmpty) {
        log.info(s"The constant ${stmt.name} has been used before it was defined in a way that requires a definition beforehand", stmt.position)
        invalidUsesBefore.foreach{use =>
          if (use.isDefined) {
            log.info(s"here:", use)
          }
        }
        if (invalidUsesBefore(None)) {
          log.info("and in some other place or places.")
        }
      }
      if (stmt.stack) log.error(s"`$name` is a constant and cannot be on stack", position)
      if (stmt.register) log.error(s"`$name` is a constant and cannot be in a register", position)
      if (stmt.address.isDefined) log.error(s"`$name` is a constant and cannot have an address", position)
      if (stmt.initialValue.isEmpty) log.error(s"`$name` is a constant and requires a value", position)
      val rawConstantValue = stmt.initialValue.flatMap(eval).getOrElse(errorConstant(s"`$name` has a non-constant value", stmt.initialValue, position)).quickSimplify
      rawConstantValue match {
        case NumericConstant(nv, _) if nv >= 2 && typ.size < 8  =>
          if (nv >= 1L.<<(8*typ.size)) {
            log.error(s"Constant value $nv too big for type ${typ.name}", stmt.position)
          }
        case _ => // ignore
      }
      val constantValue = rawConstantValue.fitInto(typ)
      if (constantValue.requiredSize > typ.size) log.error(s"`$name` is has an invalid value: not in the range of `$typ`", position)
      addThing(ConstantThing(prefix + name, constantValue, typ), stmt.position)
      for(Subvariable(suffix, offset, vol, t, arraySize) <- getSubvariables(typ)) {
        if (arraySize.isDefined) {
          log.error(s"Constants of type ${t.name} that contains array fields are not supported", stmt.position)
        } else {
          addThing(ConstantThing(prefix + name + suffix, constantValue.subconstant(options, offset, t.size), t), stmt.position)
        }
      }
    } else {
      if (stmt.stack && stmt.global) log.error(s"`$name` is static or global and cannot be on stack", position)
      if (stmt.register && typ.size != 1) log.error(s"A register variable `$name` is too large", position)
      if (stmt.register && stmt.global) log.error(s"`$name` is static or global and cannot be in a register", position)
      if (stmt.register && stmt.stack) log.error(s"`$name` cannot be simultaneously on stack and in a register", position)
      if (stmt.volatile && stmt.stack) log.error(s"`$name` cannot be simultaneously on stack and volatile", position)
      if (stmt.volatile && stmt.register) log.error(s"`$name` cannot be simultaneously volatile and in a register", position)
      if (stmt.initialValue.isDefined && parent.isDefined) log.error(s"`$name` is local and not a constant and therefore cannot have a value", position)
      if (stmt.initialValue.isDefined && stmt.address.isDefined) {
        if (options.platform.ramInitialValuesBank.isDefined) {
          log.error(s"`$name` has both address and initial value, which is unsupported on this target", position)
        } else if (options.flag(CompilationFlag.BuggyCodeWarning)) {
          log.warn(s"`$name` has both address and initial value - this may not work as expected!", position)
        }
      }
      if (stmt.register && stmt.address.isDefined) log.error(s"`$name` cannot by simultaneously at an address and in a register", position)
      if (stmt.stack) {
        val v = StackVariable(prefix + name, typ, this.baseStackOffset)
        addVariable(options, name, v, stmt.position)
        addThing(StackOffsetThing(v.name + ".addr", this.baseStackOffset, get[Type]("pointer"), None), stmt.position)
        addThing(StackOffsetThing(v.name + ".addr.lo", this.baseStackOffset, b, Some(0)), stmt.position)
        addThing(StackOffsetThing(v.name + ".addr.hi", this.baseStackOffset, b, Some(1)), stmt.position)
        addThing(StackOffsetThing(v.name + ".pointer", this.baseStackOffset, PointerType("pointer."+v.typ.name, v.typ.name, Some(v.typ)), None), stmt.position)
        addThing(StackOffsetThing(v.name + ".pointer.lo", this.baseStackOffset, b, Some(0)), stmt.position)
        addThing(StackOffsetThing(v.name + ".pointer.hi", this.baseStackOffset, b, Some(1)), stmt.position)
        baseStackOffset += typ.size
      } else {
        val (v, addr) = stmt.address.fold[(VariableInMemory, Constant)]({
          val alloc =
            if (isPointy && stmt.bank.isEmpty) VariableAllocationMethod.Zeropage
            else if (typ.name == "__reg$type") VariableAllocationMethod.Zeropage
            else if (stmt.global) VariableAllocationMethod.Static
            else if (stmt.register) VariableAllocationMethod.Register
            else VariableAllocationMethod.Auto
          if (stmt.volatile && !stmt.global && options.flag(CompilationFlag.FallbackValueUseWarning)) {
            log.warn(s"Volatile variable `$name` assumed to be static", position)
          }
          if (stmt.volatile && stmt.stack) {
            log.error(s"Volatile variable `$name` cannot be allocated on stack", position)
          }
          if (stmt.volatile && stmt.register) {
            log.error(s"Volatile variable `$name` cannot be allocated in a register", position)
          }
          if (alloc != VariableAllocationMethod.Static && stmt.initialValue.isDefined) {
            log.error(s"`$name` cannot be preinitialized`", position)
          }
          val optimizationHints = prepareVariableOptimizationHints(options, stmt)
          val v = stmt.initialValue.fold[MemoryVariable](UninitializedMemoryVariable(prefix + name, typ, alloc,
                      declaredBank = stmt.bank, optimizationHints, alignment, isVolatile = stmt.volatile)){ive =>
            InitializedMemoryVariable(name, None, typ, ive, declaredBank = stmt.bank, optimizationHints, alignment, isVolatile = stmt.volatile)
          }
          registerAddressConstant(v, stmt.position, options, Some(typ))
          (v, v.toAddress)
        })(a => {
          val addr = eval(a).getOrElse(errorConstant(s"Address of `$name` has a non-constant value", Some(a), position))
          val zp = addr match {
            case NumericConstant(n, _) => n < 0x100
            case _ => false
          }
          val v = RelativeVariable(prefix + name, addr, typ, zeropage = zp,
                      declaredBank = stmt.bank, isVolatile = stmt.volatile)
          registerAddressConstant(v, stmt.position, options, Some(typ))
          (v, addr)
        })
        addVariable(options, name, v, stmt.position)
      }
    }
  }

  def addVariable(options: CompilationOptions, localName: String, variable: Variable, position: Option[Position]): Unit = {
    val b = get[VariableType]("byte")
    variable match {
      case v: StackVariable =>
        addThing(localName, v, position)
        for (Subvariable(suffix, offset, vol, t, arraySize) <- getSubvariables(v.typ)) {
          if (arraySize.isDefined) {
            log.error(s"Cannot create a stack variable $localName of compound type ${v.typ.name} that contains an array member", position)
          } else {
            addThing(StackVariable(prefix + localName + suffix, t, baseStackOffset + offset), position)
          }
        }
      case v: MemoryVariable =>
        addThing(localName, v, position)
        for (Subvariable(suffix, offset, vol, t, arrayIndexTypeAndSize) <- getSubvariables(v.typ)) {
          arrayIndexTypeAndSize match {
            case None =>
              val subv = RelativeVariable(prefix + localName + suffix, v.toAddress + offset, t, zeropage = v.zeropage, declaredBank = v.declaredBank, isVolatile = v.isVolatile || vol)
              addThing(subv, position)
              registerAddressConstant(subv, position, options, Some(t))
            case Some((indexType, elemCount)) =>
              val suba = RelativeArray(prefix + localName + suffix + ".array", v.toAddress + offset, elemCount, v.declaredBank, indexType, t, false)
              addThing(suba, position)
              registerArrayAddresses(prefix + localName + suffix, v.declaredBank, v.toAddress + offset, indexType, t, elemCount, NoAlignment, position)
          }
        }
      case v: VariableInMemory =>
        addThing(localName, v, position)
        addThing(ConstantThing(v.name + "`", v.toAddress, get[Type]("word")), position)
        for (Subvariable(suffix, offset, vol, t, arrayIndexTypeAndSize) <- getSubvariables(v.typ)) {
          arrayIndexTypeAndSize match {
            case None =>
              val subv = RelativeVariable(prefix + localName + suffix, v.toAddress + offset, t, zeropage = v.zeropage, declaredBank = v.declaredBank, isVolatile = v.isVolatile || vol)
              addThing(subv, position)
              registerAddressConstant(subv, position, options, Some(t))
            case Some((indexType, elemCount)) =>
              val suba = RelativeArray(prefix + localName + suffix + ".array", v.toAddress + offset, elemCount, v.declaredBank, indexType, t, false)
              addThing(suba, position)
              registerArrayAddresses(prefix + localName + suffix, v.declaredBank, v.toAddress + offset, indexType, t, elemCount, NoAlignment, position)
          }
        }
      case _ => ???
    }
  }

  //noinspection NameBooleanParameters
  def getSubvariables(typ: Type): List[Subvariable] = {
    val b = get[VariableType]("byte")
    val w = get[VariableType]("word")
    if (typ.name == "__reg$type") {
      if (options.isBigEndian) {
        throw new IllegalArgumentException("__reg$type on 6809???")
      }
      return Subvariable(".lo", 0, false, b) ::
        Subvariable(".hi", 1, false, b) ::
        Subvariable(".loword", 0, false, w) ::
        Subvariable(".loword.lo", 0, false, b) ::
        Subvariable(".loword.hi", 1, false, b) ::
        Subvariable(".b2b3", 2, false, w) ::
        Subvariable(".b2b3.lo", 2, false, b) ::
        Subvariable(".b2b3.hi", 3, false, b) ::
        List.tabulate(typ.size) { i => Subvariable(".b" + i, i, false, b) }
    }
    typ match {
      case _: PlainType => typ.size match {
        case 2 => if (options.isBigEndian) List(
          Subvariable(".lo", 1, false, b),
          Subvariable(".hi", 0, false, b)
        ) else List(
          Subvariable(".lo", 0, false, b),
          Subvariable(".hi", 1, false, b))
        case 3 => if (options.isBigEndian) List(
          Subvariable(".loword", 1, false, w),
          Subvariable(".loword.lo", 2, false, b),
          Subvariable(".loword.hi", 1, false, b),
          Subvariable(".hiword", 0, false, w),
          Subvariable(".hiword.lo", 1, false, b),
          Subvariable(".hiword.hi", 0, false, b),
          Subvariable(".lo", 2, false, b),
          Subvariable(".b0", 2, false, b),
          Subvariable(".b1", 1, false, b),
          Subvariable(".b2", 0, false, b)
        ) else List(
          Subvariable(".loword", 0, false, w),
          Subvariable(".loword.lo", 0, false, b),
          Subvariable(".loword.hi", 1, false, b),
          Subvariable(".hiword", 1, false, w),
          Subvariable(".hiword.lo", 1, false, b),
          Subvariable(".hiword.hi", 2, false, b),
          Subvariable(".lo", 0, false, b),
          Subvariable(".b0", 0, false, b),
          Subvariable(".b1", 1, false, b),
          Subvariable(".b2", 2, false, b))
        case 4 => if (options.isBigEndian) List(
          Subvariable(".loword", 2, false, w),
          Subvariable(".hiword", 0, false, w),
          Subvariable(".loword.lo", 3, false, b),
          Subvariable(".loword.hi", 2, false, b),
          Subvariable(".hiword.lo", 1, false, b),
          Subvariable(".hiword.hi", 0, false, b),
          Subvariable(".lo", 3, false, b),
          Subvariable(".b0", 3, false, b),
          Subvariable(".b1", 2, false, b),
          Subvariable(".b2", 1, false, b),
          Subvariable(".b3", 0, false, b)
        ) else List(
          Subvariable(".loword", 0, false, w),
          Subvariable(".hiword", 2, false, w),
          Subvariable(".loword.lo", 0, false, b),
          Subvariable(".loword.hi", 1, false, b),
          Subvariable(".hiword.lo", 2, false, b),
          Subvariable(".hiword.hi", 3, false, b),
          Subvariable(".lo", 0, false, b),
          Subvariable(".b0", 0, false, b),
          Subvariable(".b1", 1, false, b),
          Subvariable(".b2", 2, false, b),
          Subvariable(".b3", 3, false, b)
        )
        case sz if sz > 4 =>
          if (options.isBigEndian) {
            Subvariable(".lo", sz - 1, false, b) ::
              Subvariable(".loword", sz - 2, false, w) ::
              Subvariable(".loword.lo", sz - 1, false, b) ::
              Subvariable(".loword.hi", sz - 2, false, b) ::
              List.tabulate(sz){ i => Subvariable(".b" + i, sz - 1 - i, false, b) }
          } else {
            Subvariable(".lo", 0, false, b) ::
              Subvariable(".loword", 0, false, w) ::
              Subvariable(".loword.lo", 0, false, b) ::
              Subvariable(".loword.hi", 1, false, b) ::
              List.tabulate(sz){ i => Subvariable(".b" + i, i, false, b) }
          }
        case _ => Nil
      }
      case InterruptPointerType | _: FunctionPointerType | _: PointerType => if (options.isBigEndian) List(
        Subvariable(".raw", 0, false, get[VariableType]("pointer")),
        Subvariable(".raw.lo", 1, false, b),
        Subvariable(".raw.hi", 0, false, b),
        Subvariable(".lo", 1, false, b),
        Subvariable(".hi", 0, false, b)
      ) else List(
        Subvariable(".raw", 0, false, get[VariableType]("pointer")),
        Subvariable(".raw.lo", 0, false, b),
        Subvariable(".raw.hi", 1, false, b),
        Subvariable(".lo", 0, false, b),
        Subvariable(".hi", 1, false, b))
      case s: StructType =>
        val builder = new ListBuffer[Subvariable]
        var offset = 0
        for(ResolvedFieldDesc(typ, fieldName, vol, indexTypeAndCount) <- s.mutableFieldsWithTypes) {
          offset = getTypeAlignment(typ, Set()).roundSizeUp(offset)
          val suffix = "." + fieldName
          builder += Subvariable(suffix, offset, vol, typ, indexTypeAndCount)
          if (indexTypeAndCount.isEmpty) {
            builder ++= getSubvariables(typ).map {
              case Subvariable(innerSuffix, innerOffset, innerVolatile, innerType, innerSize) => Subvariable(suffix + innerSuffix, offset + innerOffset, vol || innerVolatile, innerType, innerSize)
            }
          }
          offset += typ.size * indexTypeAndCount.fold(1)(_._2)
          offset = getTypeAlignment(typ, Set()).roundSizeUp(offset)
        }
        builder.toList
      case s: UnionType =>
        val builder = new ListBuffer[Subvariable]
        for(ResolvedFieldDesc(typ, fieldName, vol1, indexTypeAndCount) <- s.mutableFieldsWithTypes) {
          val suffix = "." + fieldName
          builder += Subvariable(suffix, 0, vol1, typ)
          if (indexTypeAndCount.isEmpty) {
            builder ++= getSubvariables(typ).map {
              case Subvariable(innerSuffix, innerOffset, vol2, innerType, innerSize) => Subvariable(suffix + innerSuffix, innerOffset, vol1 || vol2, innerType, innerSize)
            }
          }
        }
        builder.toList
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
      val thing = get[Thing](name)
      if (!thing.isInstanceOf[MangledFunction]) {
        return None
      }
      val function = thing.asInstanceOf[MangledFunction]
      if (function.name == "call") {
        if (actualParams.isEmpty || actualParams.length > 2) {
          log.error("Invalid number of parameters for function `call`", actualParams.headOption.flatMap(_._2.position))
        }
      } else {
        if (function.params.length != actualParams.length && function.name != "call") {
          log.error(s"Invalid number of parameters for function `$name`", actualParams.headOption.flatMap(_._2.position))
        }
      }
      if (name == "call") return Some(function)
      function.params match {
        case NormalParamSignature(params) =>
          function.params.types.zip(actualParams).zip(params).foreach { case ((required, (actual, expr)), m) =>
            if (!actual.isAssignableTo(required)) {
              log.error(s"Invalid value for parameter `${m.name}` of function `$name`", expr.position)
            }
          }
        case AssemblyOrMacroParamSignature(params) =>
          params.zip(actualParams).zipWithIndex.foreach { case ((AssemblyOrMacroParam(requiredType, variable, behaviour), (actual, expr)), ix) =>
            function match {
              case m: MacroFunction =>
                behaviour match {
                  case AssemblyParameterPassingBehaviour.ByReference =>
                    if (!m.isInAssembly) {
                      if (requiredType != VoidType && actual != requiredType) {
                        log.error(s"Invalid argument type for parameter `${variable.name}` of macro function `$name`: required: ${requiredType.name}, actual: ${actual.name}", expr.position)
                      }
                    }
                  case AssemblyParameterPassingBehaviour.Copy if m.isInAssembly =>
                    if (!actual.isAssignableTo(requiredType)) {
                      log.error(s"Invalid value for parameter #${ix + 1} of macro function `$name`", expr.position)
                    }
                  case _ =>
                    if (!actual.isAssignableTo(requiredType)) {
                      log.error(s"Invalid value for parameter #${ix + 1} `${variable.name}` of macro function `$name`", expr.position)
                    }
                }
              case _ =>
                if (!actual.isAssignableTo(requiredType)) {
                  log.error(s"Invalid value for parameter #${ix + 1} `${variable.name}` of function `$name`", expr.position)
                }
            }
          }
      }
      Some(function)
    } else {
      parent.flatMap(_.lookupFunction(name, actualParams))
    }
  }

  private def expandAliases(): Unit = {
    val aliasesToAdd = mutable.ListBuffer[Alias]()
    things.values.foreach{
      case Alias(aliasName, target, deprecated) =>
        val prefix = target + "."
        things.foreach{
          case (thingName, thing) =>
            if (thingName.startsWith(prefix)) {
              aliasesToAdd += Alias(aliasName + "." + thingName.stripPrefix(prefix), thingName, deprecated)
            }
        }
      case _ => ()
    }
    aliasesToAdd.foreach(a => things += a.name -> a)
  }

  def fixStructAlignments(): Unit = {
    val allStructTypes: Iterable[CompoundVariableType] = things.values.flatMap {
      case s@StructType(name, _, _) => Some(s)
      case s@UnionType(name, _, _) => Some(s)
      case _ => None
    }
    for (t <- allStructTypes) {
      t.baseAlignment match {
        case DivisibleAlignment(n) if n < 1 =>
          log.error(s"Type ${t.name} has invalid alignment ${t.alignment}")
        case WithinPageAlignment =>
          log.error(s"Type ${t.name} has invalid alignment ${t.alignment}")
        case _ =>
      }
    }
    var iterations = allStructTypes.size
    while (iterations >= 0) {
      var ok = true
      for (t <- allStructTypes) {
        if (getTypeAlignment(t, Set()) eq null) ok = false
      }
      if (ok) return
      iterations -= 1
    }
    log.error("Cycles in struct definitions found")
  }

  def fixStructSizes(): Unit = {
    val allStructTypes: Iterable[CompoundVariableType] = things.values.flatMap {
      case s@StructType(name, _, _) => Some(s)
      case s@UnionType(name, _, _) => Some(s)
      case _ => None
    }
    var iterations = allStructTypes.size
    while (iterations >= 0) {
      var ok = true
      for (t <- allStructTypes) {
        if (getTypeSize(t, Set()) < 0) ok = false
      }
      if (ok) return
      iterations -= 1
    }
    log.error("Cycles in struct definitions found")
  }

  def fixAlignedSizes(): Unit = {
    val allTypes: Iterable[VariableType] = things.values.flatMap {
      case s:VariableType => Some(s)
      case _ => None
    }
    for (t <- allTypes) {
      t.alignedSize = getTypeAlignment(t, Set()).roundSizeUp(getTypeSize(t, Set()))
    }
  }

  def getArrayFieldIndexTypeAndSize(expr: Expression): (VariableType, Int) = {
    val b = get[VariableType]("byte")
    expr match {
      case VariableExpression(name) =>
        maybeGet[Type](name) match {
          case Some(typ@EnumType(_, Some(count))) =>
            return typ -> count
          case Some(typ) =>
            log.error(s"Type $name cannot be used as an array index", expr.position)
            return b -> 0
          case _ =>
        }
      case _ =>
    }
    val constant: Int = eval(expr).map(_.quickSimplify) match {
      case Some(NumericConstant(n, _)) if n >= 0 && n <= 127 =>
        n.toInt
      case Some(NumericConstant(n, _)) =>
        log.error(s"Array size too large", expr.position)
        1
      case Some(_) =>
        log.error(s"Array size cannot be fully resolved", expr.position)
        1
      case _ =>
        errorConstant(s"Array has non-constant length", Some(expr), expr.position)
        1
    }
    if (constant <= 256) {
      b -> constant
    } else {
      get[VariableType]("word") -> constant
    }
  }

  def fixStructFields(): Unit = {
    // TODO: handle arrays?
    things.values.foreach {
      case st@StructType(_, fields, _) =>
        st.mutableFieldsWithTypes = fields.map {
          case FieldDesc(tn, name, vol, arraySize) => ResolvedFieldDesc(get[VariableType](tn), name, vol, arraySize.map(getArrayFieldIndexTypeAndSize))
        }
      case ut@UnionType(_, fields, _) =>
        ut.mutableFieldsWithTypes = fields.map {
          case FieldDesc(tn, name, vol, arraySize) => ResolvedFieldDesc(get[VariableType](tn), name, vol, arraySize.map(getArrayFieldIndexTypeAndSize))
        }
      case _ => ()
    }

  }

  def collectDeclarations(program: Program, options: CompilationOptions): Unit = {
    val b = get[VariableType]("byte")
    val v = get[Type]("void")
    if (options.flag(CompilationFlag.OptimizeForSonicSpeed)) {
      addThing(InitializedArray("identity$", None, IndexedSeq.tabulate(256)(n => LiteralExpression(n, 1)), declaredBank = None, b, b, readOnly = true, Set.empty, defaultArrayAlignment(options, 256)), None)
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
      case s: StructDefinitionStatement => registerStruct(s)
      case s: UnionDefinitionStatement => registerUnion(s)
      case _ =>
    }
    fixStructFields()
    fixStructAlignments()
    fixStructSizes()
    fixAlignedSizes()
    val pointies = collectPointies(program.declarations)
    pointiesUsed("") = pointies
    program.declarations.foreach { decl =>
      try {
        decl match {
          case f: FunctionDeclarationStatement => registerFunction(f, options)
          case v: VariableDeclarationStatement => registerVariable(v, options, pointies(v.name))
          case a: ArrayDeclarationStatement => registerArray(a, options)
          case _ =>
        }
      } catch {
        case ex: NonFatalCompilationException =>
          log.error(ex.getMessage, ex.position.orElse(decl.position))
      }
    }
    expandAliases()
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
        optimizationHints = Set.empty,
        alignment = None), options, isPointy = true)
    }
    if (CpuFamily.forType(options.platform.cpu) == CpuFamily.M6502) {
      if (!things.contains("__constant8")) {
        things("__constant8") = InitializedArray("__constant8", None, List(LiteralExpression(8, 1)), declaredBank = None, b, b, readOnly = true, Set.empty, NoAlignment)
      }
      if (options.flag(CompilationFlag.SoftwareStack)) {
        if (!things.contains("__sp")) {
          things("__sp") = UninitializedMemoryVariable("__sp", b, VariableAllocationMethod.Auto, None, Set.empty, NoAlignment, isVolatile = false)
          things("__stack") = UninitializedArray("__stack", 256, None, b, b, readOnly = false, Set.empty, DivisibleAlignment(256))
        }
      }
    }

    if (!things.contains("memory_barrier")) {
      things("memory_barrier") = MacroFunction("memory_barrier", v, AssemblyOrMacroParamSignature(Nil), isInAssembly = true, this, CpuFamily.forType(options.platform.cpu) match {
        case CpuFamily.M6502 => List(MosAssemblyStatement(Opcode.CHANGED_MEM, AddrMode.DoesNotExist, LiteralExpression(0, 1), Elidability.Fixed))
        case CpuFamily.I80 => List(Z80AssemblyStatement(ZOpcode.CHANGED_MEM, NoRegisters, None, LiteralExpression(0, 1), Elidability.Fixed))
        case CpuFamily.I86 => List(Z80AssemblyStatement(ZOpcode.CHANGED_MEM, NoRegisters, None, LiteralExpression(0, 1), Elidability.Fixed))
        case CpuFamily.M6809 => List(M6809AssemblyStatement(MOpcode.CHANGED_MEM, NonExistent, LiteralExpression(0, 1), Elidability.Fixed))
        case _ => ???
      })
    }

    if (!things.contains("breakpoint")) {
      val p = get[VariableType]("pointer")
      if (options.flag(CompilationFlag.EnableBreakpoints)) {
        things("breakpoint") = MacroFunction("breakpoint", v, AssemblyOrMacroParamSignature(Nil), isInAssembly = true, this, CpuFamily.forType(options.platform.cpu) match {
          case CpuFamily.M6502 => List(MosAssemblyStatement(Opcode.CHANGED_MEM, AddrMode.DoesNotExist, VariableExpression("..brk"), Elidability.Fixed))
          case CpuFamily.I80 => List(Z80AssemblyStatement(ZOpcode.CHANGED_MEM, NoRegisters, None, VariableExpression("..brk"), Elidability.Fixed))
          case CpuFamily.I86 => List(Z80AssemblyStatement(ZOpcode.CHANGED_MEM, NoRegisters, None, VariableExpression("..brk"), Elidability.Fixed))
          case CpuFamily.M6809 => List(M6809AssemblyStatement(MOpcode.CHANGED_MEM, NonExistent, VariableExpression("..brk"), Elidability.Fixed))
          case _ => ???
        })
      } else {
        things("breakpoint") = MacroFunction("breakpoint", v, AssemblyOrMacroParamSignature(Nil), isInAssembly = true, this, Nil)
      }
    }
  }

  def hintTypo(name: String): Unit = {
    val realThings = this.things.keySet ++ parent.map(_.things.keySet).getOrElse(Set())
    //noinspection ScalaDeprecation
    val matchingThings = realThings.filter(thing => !thing.contains("$") && StringUtils.getJaroWinklerDistance(thing,name) > 0.9) ++ hardcodedHints(name)
    if (matchingThings.nonEmpty) {
      log.info("Did you mean: " + matchingThings.toSeq.sorted.mkString(", "))
    }
  }

  private def hardcodedHints(name: String): Set[String] = {
    name match {
      case "int" => Set("word", "signed16", "int32")
      case "unsigned" => Set("word", "ubyte", "unsigned16", "unsigned32")
      case "char" => Set("byte", "sbyte")
      case "signed" => Set("sbyte", "signed16")
      case "uintptr_t" | "size_t" | "usize" => Set("word")
      case "short" | "intptr_t" | "ptrdiff_t" | "ssize_t" | "isize" => Set("word", "signed16")
      case "uint8_t" | "u8" => Set("byte", "ubyte")
      case "int8_t" | "i8" => Set("byte", "sbyte")
      case "uint16_t" | "u16" => Set("word", "unsigned16")
      case "int16_t" | "i16" => Set("word", "signed16")
      case "uint32_t" | "u32" => Set("int32", "unsigned32")
      case "int32_t" | "i32" => Set("int32", "signed32")
      case "int64_t" | "i64" => Set("int64", "signed64")
      case "boolean" | "_Bool" => Set("bool")
      case "string" => Set("pointer")
      case "puts" | "printf" | "print" => Set("putstrz")
      case "println" => Set("putstrz", "new_line")
      case "strlen" => Set("strzlen", "scrstrzlen")
      case "strcmp" => Set("strzcmp", "scrstrzcmp")
      case "strcpy" => Set("strzcopy", "scrstrzcopy")
      case "getch" | "getchar" => Set("readkey")
      case _ => Set.empty
    }
  }

  private def checkName[T <: Thing : Manifest](objType: String, name: String, pos: Option[Position]): Unit = {
    if (maybeGet[T](name).isEmpty) {
      log.error(s"$objType `$name` is not defined", pos)
      hintTypo(name)
    }
  }

  def nameCheck(nodes: List[_ <:Node]): Unit = nodes.foreach(nameCheck)

  def nameCheck(node: Node): Unit = node match {
    case _:MosAssemblyStatement => ()
    case _:Z80AssemblyStatement => ()
    case _:M6809AssemblyStatement => ()
    case _:DeclarationStatement => ()
    case s:ForStatement =>
      checkName[Variable]("Variable", s.variable, s.position)
      nameCheck(s.start)
      nameCheck(s.end)
      nameCheck(s.body)
      nameCheck(s.extraIncrement)
    case s: ForEachStatement =>
      checkName[Variable]("Variable", s.variable, s.position)
      s.pointerVariable.foreach(pv => checkName[Variable]("Variable", pv, s.position))
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
    case DerefDebuggingExpression(inner, _) =>
      nameCheck(inner)
    case DerefExpression(inner, _, _, _) =>
      nameCheck(inner)
    case IndirectFieldExpression(inner, firstIndices, fields) =>
      nameCheck(inner)
      firstIndices.foreach(nameCheck)
      fields.foreach(f => f._3.foreach(nameCheck))
    case SeparateBytesExpression(h, l) =>
      nameCheck(h)
      nameCheck(l)
    case SumExpression(params, _) =>
      nameCheck(params.map(_._2))
    case FunctionCallExpression("sizeof" | "typeof", List(ve@VariableExpression(e))) =>
      checkName[Thing]("Type, variable or constant", e, ve.position)
    case FunctionCallExpression(name, params) =>
      if (name.exists(_.isLetter) && !Environment.predefinedFunctions(name)) {
        checkName[CallableThing]("Function or type", name, node.position)
      }
      nameCheck(params)
  }

  def getBooleanConstant(literal: String): Option[Boolean] =
    maybeGet[TypedThing](literal).flatMap(_.typ match {
      case ConstantBooleanType(_, x) => Some(x)
      case _ => None
    })

  def isAlias(name: String): Boolean = {
    things.get(name).map(_.isInstanceOf[Alias]).orElse(parent.map(_.isAlias(name))).getOrElse(false)
  }

  def getAliases: Map[String, String] = {
    things.values.flatMap {
      case Alias(a, b, _) => Some(a -> b)
      case _ => None
    }.toMap ++ parent.map(_.getAliases).getOrElse(Map.empty)
  }

  def isVolatile(target: Expression): Boolean = {
    if (eval(target).isDefined) return false
    target match {
      case _: LiteralExpression => false
      case _: GeneratedConstantExpression => false
      case e: VariableExpression => maybeGet[Thing](e.name) match {
        case Some(v: Variable) => v.isVolatile
        case Some(v: MfArray) => true // TODO: all arrays assumed volatile for now
        case Some(_: Constant) => false
        case Some(_: Type) => false
        case _ => true // TODO: ?
      }
      case e: FunctionCallExpression => e.expressions.exists(isVolatile)
      case e: SumExpression => e.expressions.exists(e => isVolatile(e._2))
      case e: IndexedExpression => isVolatile(VariableExpression(e.name)) || isVolatile(e.index)
      case _ => true
    }
  }

  def isGoodEmptyLoopCondition(target: Expression): Boolean = {
    if (eval(target).isDefined) {
      // the user means an infinite loop or an empty loop
      return true
    }
    target match {
      case _: LiteralExpression => false
      case _: GeneratedConstantExpression => false
      case e: VariableExpression => maybeGet[Thing](e.name) match {
        case Some(v: Variable) => v.isVolatile
        case Some(v: MfArray) => true // TODO: all arrays assumed volatile for now
        case Some(_: Constant) => false
        case Some(_: Type) => false
        case _ => true // TODO: ?
      }
      case e: FunctionCallExpression =>
        e.functionName match {
          case "==" | "!=" | ">" | "<" | ">=" | "<=" |
               "*" | "*'" | "/" | "%%" |
               "<<" | ">>" | "<<'" | ">>'" | ">>>>"
               | "&" | "^" | "|" | "&&" | "^^" | "||" | "not" | "hi" | "lo" =>
            e.expressions.exists(isVolatile)
          case _ => true
        }
      case e: SumExpression => e.expressions.exists(e => isGoodEmptyLoopCondition(e._2))
      case e: IndexedExpression => isGoodEmptyLoopCondition(VariableExpression(e.name)) || isGoodEmptyLoopCondition(e.index)
      case _ => true
    }
  }

  def overlapsVariable(variable: String, expr: Expression): Boolean = {
    if (eval(expr).isDefined) return false
    if (expr.containsVariable(variable)) return true
    val varRootName = maybeGet[Thing](variable).getOrElse{return false}.rootName
    if (varRootName == "?") return true
    if (varRootName == "") return false
    overlapsVariableImpl(varRootName, expr)
  }

  private def overlapsVariableImpl(varRootName: String, expr: Expression): Boolean = {
    expr match {
      case _: LiteralExpression => false
      case _: GeneratedConstantExpression => false
      case e: VariableExpression => maybeGet[Thing](e.name) match {
        case Some(t) =>
          val rootName = t.rootName
          rootName == "?" || rootName == varRootName
        case _ => true // TODO: ?
      }
      case e: FunctionCallExpression => e.expressions.exists(x => overlapsVariableImpl(varRootName, x))
      case e: IndexedExpression => overlapsVariableImpl(varRootName, VariableExpression(e.name)) || overlapsVariableImpl(varRootName, e.index)
      case _ => true
    }
  }
}

object Environment {
  // built-in special-cased functions; can be considered keywords by some:
  val predefinedFunctions: Set[String] = Set("not", "hi", "lo", "nonet", "sizeof", "typeof")
  // built-in special-cased functions, not keywords, but assumed to work almost as such:
  val specialFunctions: Set[String] = Set("call")
  // functions that exist only in constants:
  val constOnlyBuiltinFunction: Set[String] = Set("sin", "cos", "tan", "min", "max")
  // keywords:
  val neverIdentifiers: Set[String] = Set(
    "array", "const", "alias", "import", "static", "register", "stack", "volatile", "asm", "extern", "kernal_interrupt", "interrupt", "reentrant", "segment",
    "struct", "union", "enum",
    "for", "if", "do", "while", "else", "return", "default",
    "to", "until", "paralleluntil", "parallelto", "downto",
    "break", "continue",
    "inline", "noinline"
  ) ++ predefinedFunctions
  // predefined identifiers that cannot be overridden and do not name a type:
  val neverValidTypeIdentifiers: Set[String] = Set(
    "true", "false",
  ) ++ neverIdentifiers
  // predefined type identifiers:
  val invalidNewIdentifiers: Set[String] = Set(
    "byte", "sbyte", "word", "pointer", "void", "long", "bool",
    "set_carry", "set_zero", "set_overflow", "set_negative",
    "clear_carry", "clear_zero", "clear_overflow", "clear_negative") ++
    Seq.iterate(8, 16)(_ + 8).map("int" + _) ++
    Seq.iterate(8, 16)(_ + 8).map("unsigned" + _) ++
    Seq.iterate(8, 16)(_ + 8).map("signed" + _) ++ neverValidTypeIdentifiers
  // built-in special-cased field names; can be considered keywords by some:
  val invalidFieldNames: Set[String] = Set("addr", "rawaddr", "pointer", "return")
}
