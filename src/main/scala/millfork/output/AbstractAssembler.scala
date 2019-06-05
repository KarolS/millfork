package millfork.output

import millfork.assembly._
import millfork.compiler.{AbstractCompiler, CompilationContext}
import millfork.env._
import millfork.error.Logger
import millfork.node.{CallGraph, NiceFunctionProperty, Program}
import millfork._
import millfork.assembly.z80.ZLine

import scala.collection.mutable
import DecimalUtils._
import millfork.node.NiceFunctionProperty.IsLeaf

/**
  * @author Karol Stasiak
  */

case class AssemblerOutput(code: Map[String, Array[Byte]], asm: Array[String], labels: List[(String, Int)])

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

  val mem = new CompiledMemory(platform.bankNumbers.keys.toList)
  val labelMap: mutable.Map[String, Int] = mutable.Map()
  private val bytesToWriteLater = mutable.ListBuffer[(String, Int, Constant)]()
  private val wordsToWriteLater = mutable.ListBuffer[(String, Int, Constant)]()

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

  def writeByte(bank: String, addr: Int, value: Constant): Unit = {
    mem.banks(bank).occupied(addr) = true
    mem.banks(bank).initialized(addr) = true
    mem.banks(bank).readable(addr) = true
    value match {
      case NumericConstant(x, _) =>
        if (x > 0xff) log.error("Byte overflow")
        mem.banks(bank).output(addr) = x.toByte
      case _ =>
        bytesToWriteLater += ((bank, addr, value))
    }
  }

  def writeWord(bank: String, addr: Int, value: Constant): Unit = {
    mem.banks(bank).occupied(addr) = true
    mem.banks(bank).occupied(addr + 1) = true
    mem.banks(bank).initialized(addr) = true
    mem.banks(bank).initialized(addr + 1) = true
    mem.banks(bank).readable(addr) = true
    mem.banks(bank).readable(addr + 1) = true
    value match {
      case NumericConstant(x, _) =>
        if (x > 0xffff) log.error("Word overflow")
        mem.banks(bank).output(addr) = x.toByte
        mem.banks(bank).output(addr + 1) = (x >> 8).toByte
      case _ =>
        wordsToWriteLater += ((bank, addr, value))
    }
  }

  var stackProbeCount = 0

  def deepConstResolve(c: Constant): Long = {
    def stackProbe(n: Int): Int = {
      stackProbeCount += 1
      if (n == 0) 0 else stackProbe(n - 1) + 1
    }
    c match {
      case NumericConstant(v, _) => v
      case AssertByte(inner) =>
        val value = deepConstResolve(inner)
        if (value.toByte == value) value else {
          log.error("Invalid relative jump: " + c)
          -2 // spin
        }
      case MemoryAddressConstant(th) =>
        try {
          if (labelMap.contains(th.name)) return labelMap(th.name)
          if (labelMap.contains(th.name + "`")) return labelMap(th.name)
          if (labelMap.contains(th.name + ".addr")) return labelMap.getOrElse[Int](th.name, labelMap(th.name + ".array"))
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
      case UnexpandedConstant(name, _) =>
        if (labelMap.contains(name)) labelMap(name)
        else ???
      case SubbyteConstant(cc, i) => deepConstResolve(cc).>>>(i * 8).&(0xff)
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
          case MathOperator.Divide => l / r
          case MathOperator.Modulo => l % r
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

  def assemble(callGraph: CallGraph, unfilteredOptimizations: Seq[AssemblyOptimization[T]], options: CompilationOptions): AssemblerOutput = {
    mem.programName = options.outputFileName.getOrElse("MILLFORK")
    val platform = options.platform
    val variableAllocators = platform.variableAllocators
    val zpOccupied = mem.banks("default").occupied
    (0 until 0x100).foreach(i => zpOccupied(i) = true)
    platform.freeZpPointers.foreach { i =>
      zpOccupied(i) = false
      zpOccupied(i + 1) = false
    }

    val optimizations = unfilteredOptimizations.filter(_.requiredFlags.forall(options.flag))

    val assembly = mutable.ArrayBuffer[String]()

    val inliningResult = inliningCalculator.calculate(
        program,
        options.flags(CompilationFlag.InlineFunctions) || options.flags(CompilationFlag.OptimizeForSonicSpeed),
        if (options.flags(CompilationFlag.OptimizeForSonicSpeed)) 4.0
        else if (options.flags(CompilationFlag.OptimizeForSpeed)) 1.3
        else 1.0,
        if (options.flags(CompilationFlag.OptimizeForSonicSpeed)) 12.0
        else if (options.flags(CompilationFlag.OptimizeForSpeed)) 8.0
        else 1.2)

    val potentiallyInlineable: Map[String, Int] = inliningResult.potentiallyInlineableFunctions
    var functionsThatCanBeCalledFromInlinedFunctions: Set[String] = inliningResult.nonInlineableFunctions

    env.allocateVariables(None, mem, callGraph, variableAllocators, options, labelMap.put, 1, forZpOnly = true)
    env.allocateVariables(None, mem, callGraph, variableAllocators, options, labelMap.put, 2, forZpOnly = true)
    env.allocateVariables(None, mem, callGraph, variableAllocators, options, labelMap.put, 3, forZpOnly = true)

    var inlinedFunctions = Map[String, List[T]]()
    val compiledFunctions = mutable.Map[String, CompiledFunction[T]]()
    val recommendedCompilationOrder = callGraph.recommendedCompilationOrder
    val niceFunctionProperties = mutable.Set[(NiceFunctionProperty, String)]()
    val aliases = env.getAliases
    recommendedCompilationOrder.foreach { f =>
      if (!env.isAlias(f)) env.maybeGet[NormalFunction](f).foreach { function =>
        val code = compileFunction(function, optimizations, options, inlinedFunctions, labelMap.toMap, niceFunctionProperties.toSet)
        val strippedCodeForInlining = for {
          limit <- potentiallyInlineable.get(f)
          if code.map(_.sizeInBytes).sum <= limit
          s <- inliningCalculator.codeForInlining(f, functionsThatCanBeCalledFromInlinedFunctions, code)
        } yield s
        strippedCodeForInlining match {
          case Some(c) =>
            log.debug("Inlining " + f, function.position)
            inlinedFunctions += f -> c
            val tmp = mutable.Set[(NiceFunctionProperty, String)]()
            gatherNiceFunctionProperties(tmp, f, c)
            if (tmp.exists(_._1 == IsLeaf)) {
              functionsThatCanBeCalledFromInlinedFunctions += function.name
            }
            compiledFunctions(f) = NonexistentFunction()
          case None =>
            log.trace("Not inlining " + f, function.position)
            functionsThatCanBeCalledFromInlinedFunctions += function.name
            compiledFunctions(f) = NormalCompiledFunction(function.declaredBank.getOrElse(platform.defaultCodeBank), code, function.address.isDefined, function.alignment)
            optimizedCodeSize += code.map(_.sizeInBytes).sum
            if (options.flag(CompilationFlag.InterproceduralOptimization)) {
              gatherNiceFunctionProperties(niceFunctionProperties, f, code)
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

    val unusedRuntimeObjects = Set("__mul_u8u8u8", "__constant8", "identity$", "__mul_u16u8u16").filterNot(name =>{
      compiledFunctions.exists{
        case (fname, compiled) => fname != name && (compiled match {
          case f:NormalCompiledFunction[_] => f.code.exists(_.refersTo(name))
          case _ => false
        })
      }
    })

    env.allPreallocatables.filterNot(o => unusedRuntimeObjects(o.name)).foreach {
      case thing@InitializedArray(name, Some(NumericConstant(address, _)), items, _, _, _, _, _) =>
        val bank = thing.bank(options)
        val bank0 = mem.banks(bank)
        var index = address.toInt
        assembly.append("* = $" + index.toHexString)
        assembly.append(name)
        for (item <- items) {
          env.eval(item) match {
            case Some(c) => writeByte(bank, index, c)
            case None => log.error(s"Non-constant contents of array `$name`", item.position)
          }
          bank0.occupied(index) = true
          bank0.initialized(index) = true
          bank0.writeable(index) = true
          bank0.readable(index) = true
          index += 1
        }
        items.grouped(16).foreach { group =>
          assembly.append("    " + bytePseudoopcode + " " + group.map(expr => env.eval(expr) match {
            case Some(c) => c.quickSimplify.toString
            case None => "<? unknown constant ?>"
          }).mkString(", "))
        }
        initializedVariablesSize += items.length
      case thing@InitializedArray(name, Some(_), items, _, _, _, _, _) => ???
      case f: NormalFunction if f.address.isDefined =>
        val bank = f.bank(options)
        val bank0 = mem.banks(bank)
        val index = f.address.get.asInstanceOf[NumericConstant].value.toInt
        compiledFunctions(f.name) match {
          case NormalCompiledFunction(_, code, _, _) =>
            labelMap(f.name) = index
            val end = outputFunction(bank, code, index, assembly, options)
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

    val codeAllocators = platform.codeAllocators.mapValues(new VariableAllocator(Nil, _))
    var justAfterCode = platform.codeAllocators.mapValues(a => a.startAt)

    val sortedCompilerFunctions = compiledFunctions.toList.sortBy { case (name, cf) => if (name == "main") 0 -> "" else cf.orderKey }
    sortedCompilerFunctions.filterNot(o => unusedRuntimeObjects(o._1)).foreach {
      case (_, NormalCompiledFunction(_, _, true, _)) =>
        // already done before
      case (name, NormalCompiledFunction(bank, code, false, alignment)) =>
        val size = code.map(_.sizeInBytes).sum
        val index = codeAllocators(bank).allocateBytes(mem.banks(bank), options, size, initialized = true, writeable = false, location = AllocationLocation.High, alignment = alignment)
        labelMap(name) = index
        justAfterCode += bank -> outputFunction(bank, code, index, assembly, options)
      case _ =>
    }
    sortedCompilerFunctions.foreach {
      case (name, RedirectedFunction(_, target, offset)) =>
        labelMap(name) = labelMap(target) + offset
      case _ =>
    }

    if (options.flag(CompilationFlag.LUnixRelocatableCode)) {
      env.allThings.things.foreach {
        case (_, m@UninitializedMemoryVariable(name, typ, _, _, _, _)) if name.endsWith(".addr") || env.maybeGet[Thing](name + ".array").isDefined =>
          val isUsed = compiledFunctions.values.exists{
            case NormalCompiledFunction(_, code, _, _)  => code.exists(_.parameter.isRelatedTo(m))
            case _ => false
          }
//          println(m.name -> isUsed)
          if (isUsed) {
            val bank = m.bank(options)
            if (bank != "default") ???
            val bank0 = mem.banks(bank)
            var index = codeAllocators(bank).allocateBytes(bank0, options, typ.size + 1, initialized = true, writeable = false, location = AllocationLocation.High, alignment = m.alignment)
            labelMap(name) = index + 1
            val altName = m.name.stripPrefix(env.prefix) + "`"
            val thing = if (name.endsWith(".addr")) env.get[ThingInMemory](name.stripSuffix(".addr")) else env.get[ThingInMemory](name + ".array")
            env.things += altName -> ConstantThing(altName, NumericConstant(index, 2), env.get[Type]("pointer"))
            assembly.append("* = $" + index.toHexString)
            assembly.append("    " + bytePseudoopcode + " $2c")
            assembly.append(name)
            val c = thing.toAddress
            writeByte(bank, index, 0x2c.toByte) // BIT abs
            index += 1
            for (i <- 0 until typ.size) {
              writeByte(bank, index, c.subbyte(i))
              assembly.append("    " + bytePseudoopcode + " " + c.subbyte(i).quickSimplify)
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
    env.allPreallocatables.filterNot(o => unusedRuntimeObjects(o.name)).foreach {
      case thing@InitializedArray(name, None, items, _, _, _, _, alignment) =>
        val bank = thing.bank(options)
        val bank0 = mem.banks(bank)
        var index = codeAllocators(bank).allocateBytes(bank0, options, items.size, initialized = true, writeable = true, location = AllocationLocation.High, alignment = alignment)
        labelMap(name) = index
        assembly.append("* = $" + index.toHexString)
        assembly.append(name)
        for (item <- items) {
          env.eval(item) match {
            case Some(c) => writeByte(bank, index, c)
            case None => log.error(s"Non-constant contents of array `$name`", item.position)
          }
          index += 1
        }
        items.grouped(16).foreach { group =>
          assembly.append("    " + bytePseudoopcode + " " + group.map(expr => env.eval(expr) match {
            case Some(c) => c.quickSimplify.toString
            case None => "<? unknown constant ?>"
          }).mkString(", "))
        }
        initializedVariablesSize += items.length
        justAfterCode += bank -> index
      case m@InitializedMemoryVariable(name, None, typ, value, _, alignment, _)  =>
        val bank = m.bank(options)
        val bank0 = mem.banks(bank)
        var index = codeAllocators(bank).allocateBytes(bank0, options, typ.size, initialized = true, writeable = true, location = AllocationLocation.High, alignment = alignment)
        labelMap(name) = index
        val altName = m.name.stripPrefix(env.prefix) + "`"
        env.things += altName -> ConstantThing(altName, NumericConstant(index, 2), env.get[Type]("pointer"))
        assembly.append("* = $" + index.toHexString)
        assembly.append(name)
        env.eval(value) match {
          case Some(c) =>
            for (i <- 0 until typ.size) {
              writeByte(bank, index, c.subbyte(i))
              assembly.append("    " + bytePseudoopcode + " " + c.subbyte(i).quickSimplify)
              index += 1
            }
          case None =>
            log.error(s"Non-constant initial value for variable `$name`")
            index += typ.size
        }
        initializedVariablesSize += typ.size
        justAfterCode += bank -> index
      case _ =>
    }
    env.getAllFixedAddressObjects.foreach {
      case (bank, addr, size) =>
        val bank0 = mem.banks(bank)
        for(i <- 0 until size) bank0.occupied(addr + i) = true
    }
    variableAllocators.foreach { case (b, a) => a.notifyAboutEndOfCode(justAfterCode(b)) }
    env.allocateVariables(None, mem, callGraph, variableAllocators, options, labelMap.put, 2, forZpOnly = false)
    env.allocateVariables(None, mem, callGraph, variableAllocators, options, labelMap.put, 3, forZpOnly = false)

    if (platform.freeZpPointers.nonEmpty) {
      val zpUsageOffset = platform.freeZpPointers.min
      val zeropageOccupation = zpOccupied.slice(zpUsageOffset, platform.freeZpPointers.max + 2)
      labelMap += "__zeropage_usage" -> (zeropageOccupation.lastIndexOf(true) - zeropageOccupation.indexOf(true) + 1)
      labelMap += "__zeropage_first" -> (zpUsageOffset + (zeropageOccupation.indexOf(true) max 0))
      labelMap += "__zeropage_last" -> (zpUsageOffset + (zeropageOccupation.lastIndexOf(true) max 0))
      labelMap += "__zeropage_end" -> (zpUsageOffset + zeropageOccupation.lastIndexOf(true) + 1)
    } else {
      labelMap += "__zeropage_usage" -> 0
      labelMap += "__zeropage_first" -> 3
      labelMap += "__zeropage_last" -> 2
      labelMap += "__zeropage_end" -> 3
    }
    labelMap += "__heap_start" -> variableAllocators("default").heapStart

    env = rootEnv.allThings

    for ((bank, addr, b) <- bytesToWriteLater) {
      val value = deepConstResolve(b)
      mem.banks(bank).output(addr) = value.toByte
    }
    for ((bank, addr, b) <- wordsToWriteLater) {
      val value = deepConstResolve(b)
      mem.banks(bank).output(addr) = value.toByte
      mem.banks(bank).output(addr + 1) = value.>>>(8).toByte
    }

    for (bank <- mem.banks.keys) {
      val start = mem.banks(bank).initialized.indexOf(true)
      val end = mem.banks(bank).initialized.lastIndexOf(true)
      val length = end - start + 1
      mem.banks(bank).start = start
      mem.banks(bank).end = end
    }

    labelMap.toList.sorted.foreach { case (l, v) =>
      assembly += f"$l%-30s = $$$v%04X"
    }
    labelMap.toList.sortBy { case (a, b) => b -> a }.foreach { case (l, v) =>
      assembly += f"    ; $$$v%04X = $l%s"
    }

    // TODO:
    val code = (platform.outputStyle match {
      case OutputStyle.Single | OutputStyle.LUnix => List("default")
      case OutputStyle.PerBank => platform.bankNumbers.keys.toList
    }).map(b => b -> platform.outputPackager.packageOutput(mem, b)).toMap
    AssemblerOutput(code, assembly.toArray, labelMap.toList)
  }

  def injectLabels(labelMap: Map[String, Int], code: List[T]): List[T]

  private def compileFunction(f: NormalFunction,
                              optimizations: Seq[AssemblyOptimization[T]],
                              options: CompilationOptions,
                              inlinedFunctions: Map[String, List[T]],
                              labelMap: Map[String, Int],
                              niceFunctionProperties: Set[(NiceFunctionProperty, String)]): List[T] = {
    log.debug("Compiling: " + f.name, f.position)
    val unoptimized: List[T] =
      injectLabels(labelMap, inliningCalculator.inline(
        compiler.compile(CompilationContext(env = f.environment, function = f, extraStackOffset = 0, options = options, niceFunctionProperties = niceFunctionProperties)),
        inlinedFunctions,
        options.jobContext))
    unoptimizedCodeSize += unoptimized.map(_.sizeInBytes).sum
    val code = optimizations.foldLeft(quickSimplify(unoptimized)) { (c, opt) =>
      val code = opt.optimize(f, c, OptimizationContext(options, labelMap, env.maybeGet[ThingInMemory]("__reg"), niceFunctionProperties))
      if (code eq c) code else quickSimplify(code)
    }
    performFinalOptimizationPass(f, optimizations.nonEmpty, options, code)
  }

  def quickSimplify(code: List[T]): List[T]

  def gatherNiceFunctionProperties(niceFunctionProperties: mutable.Set[(NiceFunctionProperty, String)], functionName: String, code: List[T]): Unit

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
              case Some(sl@SourceLine(moduleName, line)) if line > 0 =>
                if (sourceInAssembly) {
                  assOut.append("; ")
                }
                assOut.append(s";line:$line:$moduleName")
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