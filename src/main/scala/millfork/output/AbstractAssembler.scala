package millfork.output

import millfork.assembly._
import millfork.compiler.{AbstractCompiler, CompilationContext}
import millfork.env._
import millfork.error.Logger
import millfork.node.{CallGraph, Expression, FunctionCallExpression, LiteralExpression, NiceFunctionProperty, Position, Program, SumExpression}
import millfork._
import millfork.assembly.z80.ZLine

import scala.collection.mutable
import DecimalUtils._
import millfork.node.NiceFunctionProperty.IsLeaf
import millfork.parser.TextCodec

import scala.collection.mutable.ArrayBuffer

/**
  * @author Karol Stasiak
  */

case class AssemblerOutput(code: Map[String, Array[Byte]], asm: Array[String], labels: List[(String, (Int, Int))], breakpoints: List[(Int, Int)])

abstract class AbstractAssembler[T <: AbstractCode](private val program: Program,
                                           private val rootEnv: Environment,
                                           private val platform: Platform,
                                           private val inliningCalculator: AbstractInliningCalculator[T],
                                           private val compiler: AbstractCompiler[T]) {

  private var env = rootEnv.allThings
  var unoptimizedCodeSize: Int = 0
  var optimizedCodeSize: Int = 0
  var initializedVariablesSize: Int = 0
  protected val log: Logger = rootEnv.log

  val labelMap: mutable.Map[String, (Int, Int)] = mutable.Map()
  val unimportantLabelMap: mutable.Map[String, (Int, Int)] = mutable.Map()
  val mem = new CompiledMemory(platform.bankNumbers.toList, platform.bankFill, platform.isBigEndian, labelMap, log)
  val breakpointSet: mutable.Set[(Int, Int)] = mutable.Set()
  private val bytesToWriteLater = mutable.ListBuffer[(String, Int, Constant, Option[Position])]()
  private val wordsToWriteLater = mutable.ListBuffer[(String, Int, Constant, Option[Position])]()

  def writeByte(bank: String, addr: Int, value: Byte): Unit = {
    mem.banks(bank).occupied(addr) = true
    mem.banks(bank).initialized(addr) = true
    mem.banks(bank).readable(addr) = true
    mem.banks(bank).output(addr) = value.toByte
  }

  protected def writeByte(bank: String, addr: Int, value: Int): Unit = {
    mem.banks(bank).occupied(addr) = true
    mem.banks(bank).initialized(addr) = true
    mem.banks(bank).readable(addr) = true
    if ((value & 0xff) != value) ???
    mem.banks(bank).output(addr) = value.toByte
  }

  def writeByte(bank: String, addr: Int, value: Constant)(implicit position: Option[Position]): Unit = {
    mem.banks(bank).occupied(addr) = true
    mem.banks(bank).initialized(addr) = true
    mem.banks(bank).readable(addr) = true
    value match {
      case NumericConstant(x, _) =>
        if (x > 0xff) log.error("Byte overflow: " + x.toHexString)
        mem.banks(bank).output(addr) = x.toByte
      case _ =>
        bytesToWriteLater += ((bank, addr, value, position))
    }
  }

  def writeWord(bank: String, addr: Int, value: Constant)(implicit position: Option[Position]): Unit = {
    mem.banks(bank).occupied(addr) = true
    mem.banks(bank).occupied(addr + 1) = true
    mem.banks(bank).initialized(addr) = true
    mem.banks(bank).initialized(addr + 1) = true
    mem.banks(bank).readable(addr) = true
    mem.banks(bank).readable(addr + 1) = true
    if (platform.isBigEndian) {
      value match {
        case NumericConstant(x, _) =>
          if (x > 0xffff) log.error("Word overflow")
          mem.banks(bank).output(addr) = (x >> 8).toByte
          mem.banks(bank).output(addr + 1) = x.toByte
        case _ =>
          wordsToWriteLater += ((bank, addr, value, position))
      }
    } else {
      value match {
        case NumericConstant(x, _) =>
          if (x > 0xffff) log.error("Word overflow")
          mem.banks(bank).output(addr) = x.toByte
          mem.banks(bank).output(addr + 1) = (x >> 8).toByte
        case _ =>
          wordsToWriteLater += ((bank, addr, value, position))
      }
    }
  }

  var stackProbeCount = 0

  def deepConstResolve(c: Constant)(implicit position: Option[Position]): Long = {
    def stackProbe(n: Int): Int = {
      stackProbeCount += 1
      if (n == 0) 0 else stackProbe(n - 1) + 1
    }
    c match {
      case NumericConstant(v, _) => v
      case AssertByte(inner) =>
        val value = deepConstResolve(inner)
        if (value.toByte == value) value else {
          log.error("Invalid relative jump: " + c + " calculated offset: " + value, position)
          -2 // spin
        }
      case MemoryAddressConstant(th) =>
        try {
          if (labelMap.contains(th.name)) return labelMap(th.name)._2
          if (labelMap.contains(th.name + "`")) return labelMap(th.name)._2
          if (labelMap.contains(th.name + ".addr")) return labelMap.getOrElse[(Int, Int)](th.name, labelMap(th.name + ".array"))._2
          if (unimportantLabelMap.contains(th.name)) return labelMap(th.name)._2
          val x1 = env.maybeGet[ConstantThing](th.name).map(_.value)
          val x2 = env.maybeGet[ConstantThing](th.name + "`").map(_.value)
          val x3 = env.maybeGet[NormalFunction](th.name).flatMap(_.address)
          val x4 = env.maybeGet[ConstantThing](th.name + ".addr").map(_.value)
          val x5 = env.maybeGet[RelativeVariable](th.name).map(_.address)
          val x6 = env.maybeGet[ConstantThing](th.name.stripSuffix(".array") + ".addr").map(_.value)
          val x = x1.orElse(x2).orElse(x3).orElse(x4).orElse(x5).orElse(x6)
          stackProbe(700)
          x match {
            case Some(cc) =>
              deepConstResolve(cc)
            case None =>
              log.fatal("Failed to resolve constant: " + th.name)
              println(th)
              ???
          }
        } catch {
          case _: StackOverflowError =>
            log.fatal("Stack overflow " + c)
        }
      case badThing@UnexpandedConstant(name, _) =>
        if (labelMap.contains(name)) labelMap(name)._2
        else if (unimportantLabelMap.contains(name)) unimportantLabelMap(name)._2
        else {
          println(badThing)
          ???
        }
      case SubbyteConstant(cc, i) => deepConstResolve(cc).>>>(i * 8).&(0xff)
      case s: StructureConstant =>
        try {
          s.typ.size match {
            case 0 => 0
            case 1 => deepConstResolve(s.subbyte(0))
            case 2 => if (platform.isBigEndian) deepConstResolve(s.subwordReversed(0)) else deepConstResolve(s.subword(0)) // TODO: endianness?
            case _ => ???
          }
        } catch {
          case _: StackOverflowError =>
            log.fatal("Stack overflow " + c)
        }
      case CompoundConstant(operator, lc, rc) =>
        val l = deepConstResolve(lc)
        val r = deepConstResolve(rc)
        operator match {
          case MathOperator.Plus => l + r
          case MathOperator.Plus9 => (l + r) & 0x1ff
          case MathOperator.Minus => l - r
          case MathOperator.Times => l * r
          case MathOperator.Shl => l << r
          case MathOperator.Shl9 => (l << r) & 0x1ff
          case MathOperator.Shr => l >>> r
          case MathOperator.Shr9 => ((l & 0x1ff) >>> r) & 0xff
          case MathOperator.Minimum => l min r
          case MathOperator.Maximum => l max r
          case MathOperator.DecimalPlus => asDecimal(l, r, _ + _)
          case MathOperator.DecimalPlus9 => asDecimal(l, r, _ + _) & 0x1ff
          case MathOperator.DecimalMinus => asDecimal(l, r, _ - _)
          case MathOperator.DecimalTimes => asDecimal(l, r, _ * _)
          case MathOperator.DecimalShl => asDecimal(l, 1 << r, _ * _)
          case MathOperator.DecimalShl9 => asDecimal(l, 1 << r, _ * _) & 0x1ff
          case MathOperator.DecimalShr => asDecimal(l, 1 << r, _ / _)
          case MathOperator.And => l & r
          case MathOperator.Exor => l ^ r
          case MathOperator.Or => l | r
          case MathOperator.Divide => if (r == 0) {
            log.error("Constant division by zero")
            0
          } else l / r
          case MathOperator.Modulo => if (r == 0) {
            log.error("Constant division by zero")
            0
          } else l % r
        }
    }
  }

  def extractBank(c: Constant, options: CompilationOptions): Byte = {
    c.quickSimplify match {
      case NumericConstant(nn, _) => nn.>>(16).toInt.&(0xff).toByte
      case MemoryAddressConstant(th) => th.bank(options).toByte
      case CompoundConstant(MathOperator.Plus, a, b) => (extractBank(a, options) + extractBank(b, options)).toByte
      case CompoundConstant(MathOperator.Minus, a, b) => (extractBank(a, options) - extractBank(b, options)).toByte
      case _ => log.fatal("Failed to extract bank number from constant " + c)
    }
  }

  def bytePseudoopcode: String

  def deduplicate(options: CompilationOptions, compiledFunctions: mutable.Map[String, CompiledFunction[T]]): Unit

  protected def subbyte(c: Constant, index: Int, totalSize: Int): Constant = if (platform.isBigEndian) c.subbyteBe(index, totalSize) else c.subbyte(index)

  def assemble(
                callGraph: CallGraph,
                unfilteredOptimizations: Seq[AssemblyOptimization[T]],
                options: CompilationOptions,
                veryLateOptimizations: (Set[NiceFunctionProperty], CompilationOptions) => Seq[AssemblyOptimization[T]]): AssemblerOutput = {
    mem.programName = options.outputFileName.getOrElse("MILLFORK")
    val platform = options.platform
    val variableAllocators = platform.variableAllocators
    val zpOccupied = mem.banks("default").occupied
    (0 until 0x100).foreach(i => zpOccupied(i) = true)
    platform.freeZpBytes.foreach { i =>
      zpOccupied(i) = false
    }

    val optimizations = unfilteredOptimizations.filter(_.requiredFlags.forall(options.flag))

    val assembly = mutable.ArrayBuffer[String]()

    val inliningResult = inliningCalculator.calculate(
        program,
        platform.bankLayouts,
        options.flags(CompilationFlag.InlineFunctions) || options.flags(CompilationFlag.OptimizeForSonicSpeed),
        if (options.flags(CompilationFlag.OptimizeForSonicSpeed)) 4.0
        else if (options.flags(CompilationFlag.OptimizeForSpeed)) 1.3
        else 1.0,
        if (options.flags(CompilationFlag.OptimizeForSonicSpeed)) 12.0
        else if (options.flags(CompilationFlag.OptimizeForSpeed)) 8.0
        else 1.2)

    val potentiallyInlineable: Map[String, Int] = inliningResult.potentiallyInlineableFunctions
    var functionsThatCanBeCalledFromInlinedFunctions: Set[String] = inliningResult.nonInlineableFunctions

    for(th <- env.things.values) {
      th match {
        case tim: VariableInMemory =>
          tim.toAddress match {
            case NumericConstant(n, _) =>
              val m = mem.banks(tim.bank(options))
              for (i <- 0 until tim.typ.size) {
                m.occupied(i + n.toInt) = true
              }
              labelMap.put(tim.name, m.index -> n.toInt)
            case _ =>
          }
        case arr: MfArray =>
          arr.toAddress match {
            case NumericConstant(n, _) =>
              val m = mem.banks(arr.bank(options))
              for (i <- 0 until arr.sizeInBytes) {
                m.occupied(i + n.toInt) = true
              }
              labelMap.put(arr.name, m.index -> n.toInt)
            case _ =>
          }
        case _ =>
      }
    }

    env.allocateVariables(None, mem, callGraph, variableAllocators, options, labelMap.put, 1, forZpOnly = true)
    env.allocateVariables(None, mem, callGraph, variableAllocators, options, labelMap.put, 2, forZpOnly = true)
    env.allocateVariables(None, mem, callGraph, variableAllocators, options, labelMap.put, 3, forZpOnly = true)

    var inlinedFunctions = Map[String, List[T]]()
    val compiledFunctions = mutable.Map[String, CompiledFunction[T]]()
    val recommendedCompilationOrder = callGraph.recommendedCompilationOrder
    val niceFunctionProperties = mutable.Set[(NiceFunctionProperty, String)]()
    env.things.values.foreach {
      case function: FunctionInMemory =>
        gatherFunctionOptimizationHints(options, niceFunctionProperties, function)
      case _ =>
    }
    println(niceFunctionProperties)
    val aliases = env.getAliases
    recommendedCompilationOrder.foreach { f =>
      if (!env.isAlias(f)) env.maybeGet[NormalFunction](f).foreach { function =>
        val code = compileFunction(function, optimizations, options, inlinedFunctions, labelMap.toMap, niceFunctionProperties.toSet)
        val strippedCodeForInlining = for {
          limit <- potentiallyInlineable.get(f)
          if inliningCalculator.calculateExpectedSizeAfterInlining(options, function.params, code) <= limit
          s <- inliningCalculator.codeForInlining(f, functionsThatCanBeCalledFromInlinedFunctions, code)
        } yield s
        strippedCodeForInlining match {
          case Some(c) =>
            log.debug("Inlining " + f, function.position)
            inlinedFunctions += f -> c
            val tmp = mutable.Set[(NiceFunctionProperty, String)]()
            gatherNiceFunctionProperties(options, tmp, function, c)
            if (tmp.exists(_._1 == IsLeaf)) {
              functionsThatCanBeCalledFromInlinedFunctions += function.name
            }
            compiledFunctions(f) = NonexistentFunction()
          case None =>
            log.trace("Not inlining " + f, function.position)
            functionsThatCanBeCalledFromInlinedFunctions += function.name
            val thisFunctionNiceProperties = niceFunctionProperties.filter(_._2.==(f)).map(_._1).toSet
            val labelMapImm = labelMap.toMap
            val niceFunctionPropertiesImm = niceFunctionProperties.toSet
            val extraOptimizedCode = veryLateOptimizations(thisFunctionNiceProperties, options).foldLeft(code) { (c, opt) =>
              val code = opt.optimize(function, c, OptimizationContext(options, labelMapImm, env.maybeGet[ThingInMemory]("__reg"), niceFunctionPropertiesImm))
              if (code eq c) code else quickSimplify(code)
            }
            compiledFunctions(f) = NormalCompiledFunction(
              function.declaredBank.getOrElse(platform.defaultCodeBank),
              extraOptimizedCode,
              function.address.isDefined,
              function.optimizationHints,
              function.alignment)
            optimizedCodeSize += code.map(_.sizeInBytes).sum
            if (options.flag(CompilationFlag.InterproceduralOptimization)) {
              gatherNiceFunctionProperties(options, niceFunctionProperties, function, code)
            }
        }
        function.environment.removedThings.foreach(env.removeVariable)
      }
    }
    deduplicate(options, compiledFunctions)
    if (log.traceEnabled) {
      niceFunctionProperties.toList.groupBy(_._2).mapValues(_.map(_._1).sortBy(_.toString)).toList.sortBy(_._1).foreach{ case (fname, properties) =>
          log.trace(fname.padTo(30, ' ') + properties.mkString(" "))
      }
    }

    rootEnv.things.foreach{case (name, thing) =>
        if (!env.things.contains(name)) {
          env.things(name) = thing
        }
    }

    val objectsThatMayBeUnused = Set("__constant8", "identity$",
      "__mul_u8u8u8", "__mul_u16u8u16", "__mul_u16u16u16",
      "__divmod_u8u8u8u8", "__mod_u8u8u8u8", "__div_u8u8u8u8",
      "__divmod_u16u8u16u8", "__mod_u16u8u16u8", "__div_u16u8u16u8",
      "__divmod_u16u16u16u16", "__mod_u16u16u16u16", "__div_u16u16u16u16") ++
      compiledFunctions.keySet.filter(_.endsWith(".trampoline"))
    val unusedRuntimeObjects0 = objectsThatMayBeUnused.filterNot(name =>{
      compiledFunctions.exists{
        case (fname, compiled) => fname != name && (compiled match {
          case f:NormalCompiledFunction[_] => f.code.exists(_.refersTo(name))
          case _ => false
        })
      }
    })
    val unusedRuntimeObjects = unusedRuntimeObjects0 ++ objectsThatMayBeUnused.filterNot(name =>{
      compiledFunctions.exists{
        case (fname, compiled) => fname != name && !unusedRuntimeObjects0(fname) && (compiled match {
          case f:NormalCompiledFunction[_] => f.code.exists(_.refersTo(name))
          case _ => false
        })
      }
    })

    env.allPreallocatables.filterNot(o => unusedRuntimeObjects(o.name)).foreach {
      case thing@InitializedArray(name, Some(NumericConstant(address, _)), items, _, _, elementType, readOnly, _, _) =>
        val bank = thing.bank(options)
        if (!readOnly && options.platform.ramInitialValuesBank.isDefined) {
            log.error(s"Preinitialized writable array $name cannot be put at a fixed address")
        }
        val bank0 = mem.banks(bank)
        var index = address.toInt
        assembly.append("* = $" + index.toHexString)
        assembly.append(name + ":")
        for (item <- items) {
          env.eval(item) match {
            case Some(c) =>
              for(i <- 0 until elementType.alignedSize) {
                writeByte(bank, index, subbyte(c, i, elementType.size))(None)
                bank0.occupied(index) = true
                bank0.initialized(index) = true
                bank0.writeable(index) = true
                bank0.readable(index) = true
                index += 1
              }
            case None =>
              env.debugConstness(item)
              log.error(s"Non-constant contents of array `$name`: " + item, item.position)
          }
        }
        printArrayToAssemblyOutput(assembly, name, elementType, items)
        initializedVariablesSize += thing.sizeInBytes
      case thing@InitializedArray(name, Some(_), items, _, _, _, _, _, _) => ???
      case f: NormalFunction if f.address.isDefined =>
        val bank = f.bank(options)
        val bank0 = mem.banks(bank)
        val index = f.address.get.asInstanceOf[NumericConstant].value.toInt
        compiledFunctions(f.name) match {
          case NormalCompiledFunction(_, functionCode, _, _, _) =>
            labelMap(f.name) = bank0.index -> index
            val end = outputFunction(bank, functionCode, index, assembly, options)
            for (i <- index until end) {
              bank0.occupied(index) = true
              bank0.initialized(index) = true
              bank0.readable(index) = true
            }
          case NonexistentFunction() => throw new IllegalStateException()
          case RedirectedFunction(_, _, _) => throw new IllegalStateException()
        }
      case _ =>
    }

    val codeAllocators = platform.codeAllocators.mapValues(new VariableAllocator(Nil, _)).view.force
    var justAfterCode = platform.codeAllocators.mapValues(a => a.startAt).view.force


    def getLayoutStage(name: String, segment: String): Int = {
      var result = platform.bankLayouts(segment).indexOf(name)
      if (result < 0) {
        result = platform.bankLayouts(segment).indexOf("*")
      }
      //      log.trace(s"Emit stage for ${name} is $result")
      result
    }

    @inline
    def getLayoutStageThing(th: ThingInMemory): Int = getLayoutStage(th.name, th.bank(options))

    @inline
    def getLayoutStageNcf(name: String, th: NormalCompiledFunction[_]): Int = getLayoutStage(name, th.segment)

    val layoutStageCount = platform.bankLayouts.values.map(_.length).max
    val defaultStage = platform.bankLayouts("default").indexOf("*")
    if (defaultStage < 0) {
      log.fatal("The layout for the default segment lacks *")
    }
    var rwDataStart = Int.MaxValue
    var rwDataEnd = 0

    val sortedCompilerFunctions = compiledFunctions.toList.sortBy { case (_, cf) => cf.orderKey }
    for (layoutStage <- 0 until layoutStageCount) {
      sortedCompilerFunctions.filterNot(o => unusedRuntimeObjects(o._1)).foreach {
        case (_, NormalCompiledFunction(_, _, true, _, _)) =>
        // already done before
        case (name, th@NormalCompiledFunction(bank, functionCode, false, optimizationFlags, alignment)) if layoutStage == getLayoutStageNcf(name, th) =>
          val size = functionCode.map(_.sizeInBytes).sum
          val bank0 = mem.banks(bank)
          val index = codeAllocators(bank).allocateBytes(bank0, options, size, initialized = true, writeable = false, location = AllocationLocation.High, alignment = alignment)
          labelMap(name) = bank0.index -> index
          justAfterCode += bank -> outputFunction(bank, functionCode, index, assembly, options)
        case _ =>
      }
      sortedCompilerFunctions.foreach {
        case (name, RedirectedFunction(segment, target, offset)) if (layoutStage == getLayoutStage(target, segment)) =>
          val tuple = labelMap(target)
          labelMap(name) = tuple._1 -> (tuple._2 + offset)
        case _ =>
      }
      if (layoutStage == 0) {
        // force early allocation of text literals:
        env.allPreallocatables.filterNot(o => unusedRuntimeObjects(o.name)).foreach {
          case thing@InitializedArray(_, _, items, _, _, _, _, _, _) =>
            items.foreach(env.eval(_))
          case InitializedMemoryVariable(_, _, _, value, _, _, _, _) =>
            env.eval(value)
          case _ =>
        }
      }

      if (layoutStage == defaultStage && options.flag(CompilationFlag.LUnixRelocatableCode)) {
        env.allThings.things.foreach {
          case (_, m@UninitializedMemoryVariable(name, typ, _, _, _, _, _)) if name.endsWith(".addr") || env.maybeGet[Thing](name + ".array").isDefined =>
            val isUsed = compiledFunctions.values.exists {
              case NormalCompiledFunction(_, functionCode, _, _,  _) => functionCode.exists(_.parameter.isRelatedTo(m))
              case _ => false
            }
            //          println(m.name -> isUsed)
            if (isUsed) {
              val bank = m.bank(options)
              if (bank != "default") ???
              val bank0 = mem.banks(bank)
              var index = codeAllocators(bank).allocateBytes(bank0, options, typ.size + 1, initialized = true, writeable = false, location = AllocationLocation.High, alignment = m.alignment)
              labelMap(name) = bank0.index -> (index + 1)
              val altName = m.name.stripPrefix(env.prefix) + "`"
              val thing = if (name.endsWith(".addr")) env.get[ThingInMemory](name.stripSuffix(".addr")) else env.get[ThingInMemory](name + ".array")
              env.things += altName -> ConstantThing(altName, NumericConstant(index, 2), env.get[Type]("pointer"))
              assembly.append("* = $" + index.toHexString)
              assembly.append("    " + bytePseudoopcode + " $2c")
              assembly.append(name + ":")
              val c = thing.toAddress
              writeByte(bank, index, 0x2c.toByte) // BIT abs
              index += 1
              if (platform.isBigEndian) {
                throw new IllegalStateException("LUnix cannot run on big-endian architectures")
              }
              for (i <- 0 until typ.size) {
                writeByte(bank, index, subbyte(c, i, typ.size))(None)
                assembly.append("    " + bytePseudoopcode + " " + subbyte(c, i, typ.size).quickSimplify)
                index += 1
              }
              initializedVariablesSize += typ.size
              justAfterCode += bank -> index
            }
          case _ => ()
        }
        val index = codeAllocators("default").allocateBytes(mem.banks("default"), options, 1, initialized = true, writeable = false, location = AllocationLocation.High, alignment = NoAlignment)
        writeByte("default", index, 2.toByte) // BIT abs
        assembly.append("* = $" + index.toHexString)
        assembly.append("    " + bytePseudoopcode + " 2 ;; end of LUnix relocatable segment")
        justAfterCode += "default" -> (index + 1)
      }
      if (layoutStage == 0) {
        env.getAllFixedAddressObjects.foreach {
          case (bank, addr, size) =>
            val bank0 = mem.banks(bank)
            for (i <- 0 until size) bank0.occupied(addr + i) = true
            variableAllocators(bank).notifyAboutHole(bank0, addr, size)
        }
      }
      for (readOnlyPass <- Seq(true, false)) {
        if (layoutStage == defaultStage && !readOnlyPass) {
          if (options.platform.ramInitialValuesBank.isDefined) {
            codeAllocators("default").notifyAboutEndOfCode(codeAllocators("default").heapStart)
          }
        }
        env.allPreallocatables.filterNot(o => unusedRuntimeObjects(o.name)).foreach {
          case thing@InitializedArray(name, None, items, _, _, elementType, readOnly, _, alignment) if readOnly == readOnlyPass && layoutStage == getLayoutStageThing(thing) =>
            val bank = thing.bank(options)
            if (options.platform.ramInitialValuesBank.isDefined && !readOnly && bank != "default") {
              log.error(s"Preinitialized writable array `$name` should be defined in the `default` bank")
            }
            val bank0 = mem.banks(bank)
            var index = codeAllocators(bank).allocateBytes(bank0, options, thing.sizeInBytes, initialized = true, writeable = true, location = AllocationLocation.High, alignment = alignment)
            labelMap(name) = bank0.index -> index
            if (!readOnlyPass) {
              rwDataStart = rwDataStart.min(index)
              rwDataEnd = rwDataEnd.max(index + thing.sizeInBytes)
            }
            assembly.append("* = $" + index.toHexString)
            assembly.append(name + ":")
            for (item <- items) {
              val w = env
              env.eval(item) match {
                case Some(c) =>
                  for (i <- 0 until elementType.size) {
                    writeByte(bank, index, subbyte(c, i, elementType.size))(None)
                    index += 1
                  }
                case None =>
                  env.debugConstness(item)
                  log.error(s"Non-constant contents of array `$name`:" + item, item.position)
              }
            }
            printArrayToAssemblyOutput(assembly, name, elementType, items)
            initializedVariablesSize += items.length
            justAfterCode += bank -> index
          case m@InitializedMemoryVariable(name, None, typ, value, _, _, alignment, _) if !readOnlyPass && layoutStage == getLayoutStageThing(m) =>
            val bank = m.bank(options)
            if (options.platform.ramInitialValuesBank.isDefined && bank != "default") {
              log.error(s"Preinitialized variable `$name` should be defined in the `default` bank")
            }
            val bank0 = mem.banks(bank)
            var index = codeAllocators(bank).allocateBytes(bank0, options, typ.alignedSize, initialized = true, writeable = true, location = AllocationLocation.High, alignment = alignment)
            labelMap(name) = bank0.index -> index
            if (!readOnlyPass) {
              rwDataStart = rwDataStart.min(index)
              rwDataEnd = rwDataEnd.max(index + typ.alignedSize)
            }
            val altName = m.name.stripPrefix(env.prefix) + "`"
            env.things += altName -> ConstantThing(altName, NumericConstant(index, 2), env.get[Type]("pointer"))
            assembly.append("* = $" + index.toHexString)
            assembly.append(name + ":")
            env.eval(value) match {
              case Some(c) =>
                for (i <- 0 until typ.size) {
                  writeByte(bank, index, subbyte(c, i, typ.size))(None)
                  assembly.append("    " + bytePseudoopcode + " " + subbyte(c, i, typ.size).quickSimplify)
                  index += 1
                }
              case None =>
                env.debugConstness(value)
                log.error(s"Non-constant initial value for variable `$name`")
                index += typ.size
            }
            initializedVariablesSize += typ.size
            justAfterCode += bank -> index
          case _ =>
        }
      }
    }
    if (rwDataEnd == 0 && rwDataStart == Int.MaxValue) {
      rwDataStart = 0
    }
    platform.ramInitialValuesBank match {
      case None =>
      case Some(ivBank) =>
        val db = mem.banks("default")
        val ib = mem.banks(ivBank)
        val size = rwDataEnd - rwDataStart
        if (size < 0) log.fatal("Negative writable memory size. It's a compiler bug.")
        val ivAddr = codeAllocators(ivBank).allocateBytes(ib, options, size, initialized = true, writeable = false, AllocationLocation.High, NoAlignment)
        unimportantLabelMap += "__rwdata_init_start" -> (ib.index -> ivAddr)
        unimportantLabelMap += "__rwdata_init_end" -> (ib.index -> (ivAddr + size))
        unimportantLabelMap += "__rwdata_size" -> (ib.index -> size)
        for (i <- 0 until size) {
          ib.output(ivAddr + i) = db.output(rwDataStart + i)
        }
        val debugArray = Array.fill[Option[Constant]](size)(None)
        bytesToWriteLater ++= bytesToWriteLater.flatMap{
          case ("default", addr, value, position) if addr >= rwDataStart && addr < rwDataEnd =>
            debugArray(addr - rwDataStart) = Some(value)
            Some(ivBank, addr + ivAddr - rwDataStart, value, position)
          case _ => None
        }
        wordsToWriteLater ++= wordsToWriteLater.flatMap {
          case ("default", addr, value, position) if addr >= rwDataStart && addr < rwDataEnd =>
            debugArray(addr - rwDataStart) = Some(value.loByte)
            debugArray(addr - rwDataStart + 1) = Some(value.hiByte)
            Some(ivBank, addr + ivAddr - rwDataStart, value, position)
          case _ => None
        }
        assembly.append("* = $" + ivAddr.toHexString)
        assembly.append("__rwdata_init_start:")
        for (addrs <- 0 until size grouped 16) {
          assembly.append("    " + bytePseudoopcode + " " + addrs.map(i =>
            debugArray(i).getOrElse(NumericConstant(ib.output(i + ivAddr) & 0xff, 1))
          ).mkString(", "))
        }
    }
    if (options.platform.ramInitialValuesBank.isDefined) {
      variableAllocators("default").notifyAboutEndOfData(rwDataEnd)
    }
    variableAllocators.foreach { case (b, a) => a.notifyAboutEndOfCode(justAfterCode(b)) }
    env.allocateVariables(None, mem, callGraph, variableAllocators, options, labelMap.put, 2, forZpOnly = false)
    env.allocateVariables(None, mem, callGraph, variableAllocators, options, labelMap.put, 3, forZpOnly = false)

    val defaultBank = mem.banks("default").index
    if (platform.freeZpBytes.nonEmpty) {
      val zpUsageOffset = platform.freeZpBytes.min
      val zeropageOccupation = zpOccupied.slice(zpUsageOffset, platform.freeZpBytes.max + 1)
      unimportantLabelMap += "__zeropage_usage" -> (defaultBank, zeropageOccupation.lastIndexOf(true) - zeropageOccupation.indexOf(true) + 1)
      unimportantLabelMap += "__zeropage_first" -> (defaultBank, zpUsageOffset + (zeropageOccupation.indexOf(true) max 0))
      unimportantLabelMap += "__zeropage_last" -> (defaultBank, zpUsageOffset + (zeropageOccupation.lastIndexOf(true) max 0))
      unimportantLabelMap += "__zeropage_end" -> (defaultBank, zpUsageOffset + zeropageOccupation.lastIndexOf(true) + 1)
    } else {
      unimportantLabelMap += "__zeropage_usage" -> (defaultBank -> 0)
      unimportantLabelMap += "__zeropage_first" -> (defaultBank -> 3)
      unimportantLabelMap += "__zeropage_last" -> (defaultBank -> 2)
      unimportantLabelMap += "__zeropage_end" -> (defaultBank -> 3)
    }
    unimportantLabelMap += "__rwdata_start" -> (defaultBank -> rwDataStart)
    unimportantLabelMap += "__rwdata_end" -> (defaultBank -> rwDataEnd)
    unimportantLabelMap += "__heap_start" -> (defaultBank -> variableAllocators("default").heapStart)
    for (segment <- platform.bankNumbers.keys) {
      val allocator = options.platform.variableAllocators(segment)
      unimportantLabelMap += s"segment.$segment.start" -> (defaultBank -> allocator.startAt)
      unimportantLabelMap += s"segment.$segment.end" -> (defaultBank -> (allocator.endBefore - 1))
      unimportantLabelMap += s"segment.$segment.heapstart" -> (defaultBank -> allocator.heapStart)
      unimportantLabelMap += s"segment.$segment.length" -> (defaultBank -> (allocator.endBefore - allocator.startAt))
      unimportantLabelMap += s"segment.$segment.bank" -> (defaultBank -> platform.bankNumbers(segment))
    }

    env = rootEnv.allThings

    for ((bank, addr, b, position) <- bytesToWriteLater) {
      val value = deepConstResolve(b)(position)
      mem.banks(bank).output(addr) = value.toByte
    }
    for ((bank, addr, b, position) <- wordsToWriteLater) {
      val value = deepConstResolve(b)(position)
      if (platform.isBigEndian) {
        mem.banks(bank).output(addr) = value.>>>(8).toByte
        mem.banks(bank).output(addr + 1) = value.toByte
      } else {
        mem.banks(bank).output(addr) = value.toByte
        mem.banks(bank).output(addr + 1) = value.>>>(8).toByte
      }
    }

    for (bank <- mem.banks.keys) {
      val start = mem.banks(bank).initialized.indexOf(true)
      val end = mem.banks(bank).initialized.lastIndexOf(true)
      val length = end - start + 1
      mem.banks(bank).start = start
      mem.banks(bank).end = end
    }
    val allLabelList = labelMap.toList ++ unimportantLabelMap.toList
    allLabelList.sorted.foreach { case (l, (_, v)) =>
      assembly += f"$l%-30s = $$$v%04X"
    }
    allLabelList.sortBy { case (a, (_, v)) => v -> a }.foreach { case (l, (_, v)) =>
      assembly += f"    ; $$$v%04X = $l%s"
    }

    // TODO:
    val code = (platform.outputStyle match {
      case OutputStyle.Single | OutputStyle.LUnix => List("default")
      case OutputStyle.PerBank => platform.bankNumbers.keys.toList
    }).map{b =>
      val outputPackager = platform.outputPackagers.getOrElse(b, platform.defaultOutputPackager)
      b -> outputPackager.packageOutput(mem, b)
    }.toMap
    AssemblerOutput(code, assembly.toArray, labelMap.toList, breakpointSet.toList.sorted)
  }

  private def printArrayToAssemblyOutput(assembly: ArrayBuffer[String], name: String, elementType: Type, items: Seq[Expression]): Unit = {
    if (name.startsWith("textliteral$")) {
      var suffix = ""
      var chars = items.lastOption match {
        case Some(LiteralExpression(0, _)) =>
          suffix = "z"
          items.init
        case _ => items
      }
      chars.headOption match {
        case Some(LiteralExpression(n, _)) if n + 1 == chars.size  =>
          // length-prefixed
          suffix = "p" + suffix
          chars = chars.tail
        case _ =>
      }
      val text = chars.map {
        case LiteralExpression(i, _) if i >= 0 && i <= 255 => platform.defaultCodec.decode(i.toInt)
        case _ => TextCodec.NotAChar
      }.mkString("")
      if (!text.contains(TextCodec.NotAChar) && !text.exists(c => c.isControl)) assembly.append("    ; \"" + text + "\"" + suffix)
    }
    items.flatMap(expr => env.eval(expr) match {
      case Some(c) => List.tabulate(elementType.size)(i => subbyte(c, i, elementType.size).quickSimplify.toString)
      case None => List.fill(elementType.size)("<? unknown constant ?>")
    }).grouped(16).foreach { group =>
      assembly.append("    " + bytePseudoopcode + " " + group.mkString(", "))
    }
  }

  def injectLabels(labelMap: Map[String, (Int, Int)], code: List[T]): List[T]

  private def compileFunction(f: NormalFunction,
                              optimizations: Seq[AssemblyOptimization[T]],
                              options: CompilationOptions,
                              inlinedFunctions: Map[String, List[T]],
                              labelMap: Map[String, (Int, Int)],
                              niceFunctionProperties: Set[(NiceFunctionProperty, String)]): List[T] = {
    log.debug("Compiling: " + f.name, f.position)
    val unoptimized: List[T] =
      injectLabels(labelMap, inliningCalculator.inline(
        compiler.compile(CompilationContext(env = f.environment, function = f, extraStackOffset = 0, options = options, niceFunctionProperties = niceFunctionProperties)),
        inlinedFunctions,
        options.jobContext))
    unoptimizedCodeSize += unoptimized.map(_.sizeInBytes).sum
    // unoptimized.foreach(l => log.trace(l.toString))
    val code = optimizations.foldLeft(quickSimplify(unoptimized)) { (c, opt) =>
      val code = opt.optimize(f, c, OptimizationContext(options, labelMap, env.maybeGet[ThingInMemory]("__reg"), niceFunctionProperties))
      if (code eq c) code else quickSimplify(code)
    }
    performFinalOptimizationPass(f, optimizations.nonEmpty, options, code)
  }

  def quickSimplify(code: List[T]): List[T]

  def gatherNiceFunctionProperties(options: CompilationOptions, niceFunctionProperties: mutable.Set[(NiceFunctionProperty, String)], function: NormalFunction, code: List[T]): Unit

  def gatherFunctionOptimizationHints(options: CompilationOptions, niceFunctionProperties: mutable.Set[(NiceFunctionProperty, String)], function: FunctionInMemory): Unit

  def performFinalOptimizationPass(f: NormalFunction, actuallyOptimize: Boolean, options: CompilationOptions, code: List[T]): List[T]

  private val FinalWhitespace = "\\s+$".r

  private def outputFunction(bank: String, code: List[T], startFrom: Int, assOut: mutable.ArrayBuffer[String], options: CompilationOptions): Int = {
    val printLineNumbers = options.flag(CompilationFlag.LineNumbersInAssembly)
    val sourceInAssembly = options.flag(CompilationFlag.SourceInAssembly)
    var index = startFrom
    assOut.append(" ")
    assOut.append("* = $" + startFrom.toHexString)
    var lastSource = Option.empty[SourceLine]
    for (instr <- code) {
      if (instr.isPrintable) {
        if(lastSource != instr.source) {
          lastSource = instr.source
          if (printLineNumbers) {
            lastSource match {
              case Some(sl@SourceLine(moduleName, lineNo)) if lineNo > 0 =>
                if (sourceInAssembly) {
                  assOut.append("; ")
                }
                assOut.append(s";line:$lineNo:$moduleName")
                if (sourceInAssembly) {
                  log.getLine(sl).foreach(l => assOut.append(";   " + l))
                }
              case _ =>
                if (sourceInAssembly) {
                  assOut.append("; ")
                }
                assOut.append(s";line")
            }
          }
        }
        val line = if (options.platform.cpuFamily == CpuFamily.I86) {
          instr match {
            case zline: ZLine =>
              zline.toIntel8086String
            case _ =>
              instr.toString
          }
        } else if (options.flag(CompilationFlag.UseIntelSyntaxForOutput)) {
          instr match {
            case zline: ZLine =>
              zline.toIntelString
            case _ =>
              instr.toString
          }
        } else {
          instr.toString
        }
        if (line.contains("; @")) {
          assOut.append(FinalWhitespace.replaceAllIn(line.substring(0, line.lastIndexOf("; @")), ""))
        } else {
          assOut.append(line)
        }
      }
      index = emitInstruction(bank, options, index, instr)
    }
    if (printLineNumbers && lastSource.isDefined){
      if (sourceInAssembly) assOut.append("; ")
      assOut.append(s";line")
    }
    index
  }

  def emitInstruction(bank: String, options: CompilationOptions, index: Int, instr: T): Int
}