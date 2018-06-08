package millfork.output

import millfork.assembly.opt.{AssemblyOptimization, HudsonOptimizations, JumpShortening}
import millfork.assembly.{AddrMode, AssemblyLine, Opcode}
import millfork.compiler.{CompilationContext, MfCompiler}
import millfork.env._
import millfork.error.ErrorReporting
import millfork.node.{CallGraph, Program}
import millfork._

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */

case class AssemblerOutput(code: Map[String, Array[Byte]], asm: Array[String], labels: List[(String, Int)])

class Assembler(private val program: Program, private val rootEnv: Environment, private val platform: Platform) {

  private var env = rootEnv.allThings
  var unoptimizedCodeSize: Int = 0
  var optimizedCodeSize: Int = 0
  var initializedVariablesSize: Int = 0

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

  def writeByte(bank: String, addr: Int, value: Constant): Unit = {
    mem.banks(bank).occupied(addr) = true
    mem.banks(bank).initialized(addr) = true
    mem.banks(bank).readable(addr) = true
    value match {
      case NumericConstant(x, _) =>
        if (x > 0xff) ErrorReporting.error("Byte overflow")
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
        if (x > 0xffff) ErrorReporting.error("Word overflow")
        mem.banks(bank).output(addr) = x.toByte
        mem.banks(bank).output(addr + 1) = (x >> 8).toByte
      case _ =>
        wordsToWriteLater += ((bank, addr, value))
    }
  }

  def deepConstResolve(c: Constant): Long = {
    c match {
      case NumericConstant(v, _) => v
      case AssertByte(inner) =>
        val value = deepConstResolve(inner)
        if (value.toByte == value) value else {
          ErrorReporting.error("Invalid relative jump: " + c)
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
          x match {
            case Some(cc) =>
              deepConstResolve(cc)
            case None =>
              println(th)
              ???
          }
        } catch {
          case e: StackOverflowError =>
            ErrorReporting.fatal("Stack overflow " + c)
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
        }
    }
  }

  def extractBank(c: Constant, options: CompilationOptions): Byte = {
    c.quickSimplify match {
      case NumericConstant(nn, _) => nn.>>(16).toInt.&(0xff).toByte
      case MemoryAddressConstant(th) => th.bank(options).toByte
      case CompoundConstant(MathOperator.Plus, a, b) => (extractBank(a, options) + extractBank(b, options)).toByte
      case CompoundConstant(MathOperator.Minus, a, b) => (extractBank(a, options) - extractBank(b, options)).toByte
      case _ => ErrorReporting.fatal("Failed to extract bank number from constant " + c)
    }
  }

  private def parseNormalToDecimalValue(a: Long): Long = {
    if (a < 0) -parseNormalToDecimalValue(-a)
    var x = a
    var result = 0L
    var multiplier = 1L
    while (x > 0) {
      result += multiplier * (a % 16L)
      x /= 16L
      multiplier *= 10L
    }
    result
  }

  private def storeDecimalValueInNormalRespresentation(a: Long): Long = {
    if (a < 0) -storeDecimalValueInNormalRespresentation(-a)
    var x = a
    var result = 0L
    var multiplier = 1L
    while (x > 0) {
      result += multiplier * (a % 10L)
      x /= 10L
      multiplier *= 16L
    }
    result
  }

  private def asDecimal(a: Long, b: Long, f: (Long, Long) => Long): Long =
    storeDecimalValueInNormalRespresentation(f(parseNormalToDecimalValue(a), parseNormalToDecimalValue(b)))

  def assemble(callGraph: CallGraph, optimizations: Seq[AssemblyOptimization], options: CompilationOptions): AssemblerOutput = {
    val platform = options.platform

    val assembly = mutable.ArrayBuffer[String]()

    val inliningResult = InliningCalculator.calculate(
        program,
        options.flags(CompilationFlag.InlineFunctions) || options.flags(CompilationFlag.OptimizeForSonicSpeed),
        if (options.flags(CompilationFlag.OptimizeForSonicSpeed)) 4.0
        else if (options.flags(CompilationFlag.OptimizeForSpeed)) 1.3
        else 1.0,
        if (options.flags(CompilationFlag.OptimizeForSonicSpeed)) 12.0
        else if (options.flags(CompilationFlag.OptimizeForSpeed)) 8.0
        else 1.2)

    val potentiallyInlineable: Map[String, Int] = inliningResult.potentiallyInlineableFunctions
    var nonInlineableFunctions: Set[String] = inliningResult.nonInlineableFunctions

    var inlinedFunctions = Map[String, List[AssemblyLine]]()
    val compiledFunctions = mutable.Map[String, List[AssemblyLine]]()
    val recommendedCompilationOrder = callGraph.recommendedCompilationOrder
    recommendedCompilationOrder.foreach { f =>
      env.maybeGet[NormalFunction](f).foreach { function =>
        val code = compileFunction(function, optimizations, options, inlinedFunctions)
        val strippedCodeForInlining = for {
          limit <- potentiallyInlineable.get(f)
          if code.map(_.sizeInBytes).sum <= limit
          s <- InliningCalculator.codeForInlining(f, nonInlineableFunctions, code)
        } yield s
        strippedCodeForInlining match {
          case Some(c) =>
            ErrorReporting.debug("Inlining " + f, function.position)
            inlinedFunctions += f -> c
            compiledFunctions(f) = Nil
          case None =>
            nonInlineableFunctions += function.name
            compiledFunctions(f) = code
            optimizedCodeSize += code.map(_.sizeInBytes).sum
        }
        function.environment.removedThings.foreach(env.removeVariable)
      }
    }

    rootEnv.things.foreach{case (name, thing) =>
        if (!env.things.contains(name)) {
          env.things(name) = thing
        }
    }

    env.allPreallocatables.foreach {
      case thing@InitializedArray(name, Some(NumericConstant(address, _)), items, _) =>
        val bank = thing.bank(options)
        val bank0 = mem.banks(bank)
        var index = address.toInt
        assembly.append("* = $" + index.toHexString)
        assembly.append(name)
        for (item <- items) {
          env.eval(item) match {
            case Some(c) => writeByte(bank, index, c)
            case None => ErrorReporting.error(s"Non-constant contents of array `$name`")
          }
          bank0.occupied(index) = true
          bank0.initialized(index) = true
          bank0.writeable(index) = true
          bank0.readable(index) = true
          index += 1
        }
        items.grouped(16).foreach { group =>
          assembly.append("    !byte " + group.map(expr => env.eval(expr) match {
            case Some(c) => c.quickSimplify.toString
            case None => "<? unknown constant ?>"
          }).mkString(", "))
        }
        initializedVariablesSize += items.length
      case thing@InitializedArray(name, Some(_), items, _) => ???
      case f: NormalFunction if f.address.isDefined =>
        val bank = f.bank(options)
        val bank0 = mem.banks(bank)
        val index = f.address.get.asInstanceOf[NumericConstant].value.toInt
        val code = compiledFunctions(f.name)
        if (code.nonEmpty) {
          labelMap(f.name) = index
          val end = outputFunction(bank, code, index, assembly, options)
          for(i <- index until end) {
            bank0.occupied(index) = true
            bank0.initialized(index) = true
            bank0.readable(index) = true
          }
        }
      case _ =>
    }

    val codeAllocators = platform.codeAllocators.mapValues(new VariableAllocator(Nil, _))
    var justAfterCode = platform.codeAllocators.mapValues(a => a.startAt)
    env.allPreallocatables.foreach {
      case f: NormalFunction if f.address.isEmpty && f.name == "main" =>
        val bank = f.bank(options)

        val code = compiledFunctions(f.name)
        if (code.nonEmpty) {
          val size = code.map(_.sizeInBytes).sum
          val index = codeAllocators(bank).allocateBytes(mem.banks(bank), options, size, initialized = true, writeable = false)
          labelMap(f.name) = index
          justAfterCode += bank -> outputFunction(bank, code, index, assembly, options)
        }
      case _ =>
    }
    env.allPreallocatables.foreach {
      case f: NormalFunction if f.address.isEmpty && f.name != "main" =>
        val bank = f.bank(options)
        val bank0 = mem.banks(bank)
        val code = compiledFunctions(f.name)
        if (code.nonEmpty) {
          val size = code.map(_.sizeInBytes).sum
          val index = codeAllocators(bank).allocateBytes(bank0, options, size, initialized = true, writeable = false)
          labelMap(f.name) = index
          justAfterCode += bank -> outputFunction(bank, code, index, assembly, options)
        }
      case _ =>
    }
    if (options.flag(CompilationFlag.LUnixRelocatableCode)) {
      env.allThings.things.foreach {
        case (_, m@UninitializedMemoryVariable(name, typ, _, _)) if name.endsWith(".addr") || env.maybeGet[Thing](name + ".array").isDefined =>
          val isUsed = compiledFunctions.values.exists(_.exists(_.parameter.isRelatedTo(m)))
//          println(m.name -> isUsed)
          if (isUsed) {
            val bank = m.bank(options)
            if (bank != "default") ???
            val bank0 = mem.banks(bank)
            var index = codeAllocators(bank).allocateBytes(bank0, options, typ.size + 1, initialized = true, writeable = false)
            labelMap(name) = index + 1
            val altName = m.name.stripPrefix(env.prefix) + "`"
            val thing = if (name.endsWith(".addr")) env.get[ThingInMemory](name.stripSuffix(".addr")) else env.get[ThingInMemory](name + ".array")
            env.things += altName -> ConstantThing(altName, NumericConstant(index, 2), env.get[Type]("pointer"))
            assembly.append("* = $" + index.toHexString)
            assembly.append("    !byte $2c")
            assembly.append(name)
            val c = thing.toAddress
            writeByte(bank, index, 0x2c.toByte) // BIT abs
            index += 1
            for (i <- 0 until typ.size) {
              writeByte(bank, index, c.subbyte(i))
              assembly.append("    !byte " + c.subbyte(i).quickSimplify)
              index += 1
            }
            initializedVariablesSize += typ.size
            justAfterCode += bank -> index
          }
        case _ => ()
      }
      val index = codeAllocators("default").allocateBytes(mem.banks("default"), options, 1, initialized = true, writeable = false)
      writeByte("default", index, 2.toByte) // BIT abs
      assembly.append("* = $" + index.toHexString)
      assembly.append("    !byte 2 ;; end of LUnix relocatable segment")
      justAfterCode += "default" -> (index + 1)
    }
    env.allPreallocatables.foreach {
      case thing@InitializedArray(name, None, items, _) =>
        val bank = thing.bank(options)
        val bank0 = mem.banks(bank)
        var index = codeAllocators(bank).allocateBytes(bank0, options, items.size, initialized = true, writeable = true)
        labelMap(name) = index
        assembly.append("* = $" + index.toHexString)
        assembly.append(name)
        for (item <- items) {
          env.eval(item) match {
            case Some(c) => writeByte(bank, index, c)
            case None => ErrorReporting.error(s"Non-constant contents of array `$name`")
          }
          index += 1
        }
        items.grouped(16).foreach { group =>
          assembly.append("    !byte " + group.map(expr => env.eval(expr) match {
            case Some(c) => c.quickSimplify.toString
            case None => "<? unknown constant ?>"
          }).mkString(", "))
        }
        initializedVariablesSize += items.length
        justAfterCode += bank -> index
      case m@InitializedMemoryVariable(name, None, typ, value, _)  =>
        val bank = m.bank(options)
        val bank0 = mem.banks(bank)
        var index = codeAllocators(bank).allocateBytes(bank0, options, typ.size, initialized = true, writeable = true)
        labelMap(name) = index
        val altName = m.name.stripPrefix(env.prefix) + "`"
        env.things += altName -> ConstantThing(altName, NumericConstant(index, 2), env.get[Type]("pointer"))
        assembly.append("* = $" + index.toHexString)
        assembly.append(name)
        env.eval(value) match {
          case Some(c) =>
            for (i <- 0 until typ.size) {
              writeByte(bank, index, c.subbyte(i))
              assembly.append("    !byte " + c.subbyte(i).quickSimplify)
              index += 1
            }
          case None =>
            ErrorReporting.error(s"Non-constant initial value for variable `$name`")
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
    val variableAllocators = platform.variableAllocators
    variableAllocators.foreach { case (b, a) => a.notifyAboutEndOfCode(justAfterCode(b)) }
    env.allocateVariables(None, mem, callGraph, variableAllocators, options, labelMap.put)

    val zeropageOccupation = mem.banks("default").occupied.slice(variableAllocators("default").pointers.head, variableAllocators("default").pointers.last + 2)
    labelMap += "__zeropage_usage" -> (zeropageOccupation.lastIndexOf(true) - zeropageOccupation.indexOf(true) + 1)
    labelMap += "__zeropage_first" -> (zeropageOccupation.indexOf(true) max 0)
    labelMap += "__zeropage_last" -> (zeropageOccupation.lastIndexOf(true) max 0)
    labelMap += "__zeropage_end" -> (zeropageOccupation.lastIndexOf(true) + 1)

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

  private def compileFunction(f: NormalFunction, optimizations: Seq[AssemblyOptimization], options: CompilationOptions, inlinedFunctions: Map[String, List[AssemblyLine]]): List[AssemblyLine] = {
    ErrorReporting.debug("Compiling: " + f.name, f.position)
    val unoptimized =
      MfCompiler.compile(CompilationContext(env = f.environment, function = f, extraStackOffset = 0, options = options)).flatMap {
        case AssemblyLine(Opcode.JSR, AddrMode.Absolute | AddrMode.LongAbsolute, p, true) if inlinedFunctions.contains(p.toString) =>
          val labelPrefix = MfCompiler.nextLabel("ai")
          inlinedFunctions(p.toString).map{
            case line@AssemblyLine(_, _, MemoryAddressConstant(Label(label)), _) =>
              val newLabel = MemoryAddressConstant(Label(labelPrefix + label))
              line.copy(parameter = newLabel)
            case l => l
          }
        case AssemblyLine(Opcode.JMP, AddrMode.Absolute, p, true) if inlinedFunctions.contains(p.toString) =>
          val labelPrefix = MfCompiler.nextLabel("ai")
          inlinedFunctions(p.toString).map{
            case line@AssemblyLine(_, _, MemoryAddressConstant(Label(label)), _) =>
              val newLabel = MemoryAddressConstant(Label(labelPrefix + label))
              line.copy(parameter = newLabel)
            case l => l
          } :+ AssemblyLine.implied(Opcode.RTS)
        case x => List(x)
      }
    unoptimizedCodeSize += unoptimized.map(_.sizeInBytes).sum
    val code = optimizations.foldLeft(unoptimized) { (c, opt) =>
      opt.optimize(f, c, options)
    }
    if (optimizations.nonEmpty) {
      val finalCode = if (options.flag(CompilationFlag.EmitHudsonOpcodes)) HudsonOptimizations.removeLoadZero(code) else code
      JumpShortening(f, JumpShortening(f, finalCode, options), options)
    }
    else code
  }

  private def outputFunction(bank: String, code: List[AssemblyLine], startFrom: Int, assOut: mutable.ArrayBuffer[String], options: CompilationOptions): Int = {
    var index = startFrom
    assOut.append("* = $" + startFrom.toHexString)
    import millfork.assembly.AddrMode._
    import millfork.assembly.Opcode._
    for (instr <- code) {
      if (instr.isPrintable) {
        assOut.append(instr.toString)
      }
      instr match {
        case AssemblyLine(BYTE, RawByte, c, _) =>
          writeByte(bank, index, c)
          index += 1
        case AssemblyLine(BYTE, _, _, _) => ???
        case AssemblyLine(_, RawByte, _, _) => ???
        case AssemblyLine(LABEL, _, MemoryAddressConstant(Label(labelName)), _) =>
          labelMap(labelName) = index
        case AssemblyLine(_, DoesNotExist, _, _) =>
          ()
        case AssemblyLine(op, Implied, _, _) =>
          writeByte(bank, index, Assembler.opcodeFor(op, Implied, options))
          index += 1
        case AssemblyLine(op, Relative, param, _) =>
          writeByte(bank, index, Assembler.opcodeFor(op, Relative, options))
          writeByte(bank, index + 1, AssertByte(param - (index + 2)))
          index += 2
        case AssemblyLine(op, am@(Immediate | ZeroPage | ZeroPageX | ZeroPageY | IndexedY | IndexedX | IndexedZ | LongIndexedY | LongIndexedZ | Stack), param, _) =>
          writeByte(bank, index, Assembler.opcodeFor(op, am, options))
          writeByte(bank, index + 1, param)
          index += 2
        case AssemblyLine(op, am@(WordImmediate | Absolute | AbsoluteY | AbsoluteX | Indirect | AbsoluteIndexedX), param, _) =>
          writeByte(bank, index, Assembler.opcodeFor(op, am, options))
          writeWord(bank, index + 1, param)
          index += 3
        case AssemblyLine(op, am@(LongAbsolute | LongAbsoluteX | LongIndirect), param, _) =>
          writeByte(bank, index, Assembler.opcodeFor(op, am, options))
          writeWord(bank, index + 1, param)
          writeByte(bank, index + 3, extractBank(param, options))
          index += 4
      }
    }
    index
  }
}

object Assembler {
  val opcodes = mutable.Map[(Opcode.Value, AddrMode.Value), Byte]()
  val illegalOpcodes = mutable.Map[(Opcode.Value, AddrMode.Value), Byte]()
  val cmosOpcodes = mutable.Map[(Opcode.Value, AddrMode.Value), Byte]()
  val cmosNopOpcodes = mutable.Map[(Opcode.Value, AddrMode.Value), Byte]()
  val ce02Opcodes = mutable.Map[(Opcode.Value, AddrMode.Value), Byte]()
  val hudsonOpcodes = mutable.Map[(Opcode.Value, AddrMode.Value), Byte]()
  val emulation65816Opcodes = mutable.Map[(Opcode.Value, AddrMode.Value), Byte]()
  val native65816Opcodes = mutable.Map[(Opcode.Value, AddrMode.Value), Byte]()

  def opcodeFor(opcode: Opcode.Value, addrMode: AddrMode.Value, options: CompilationOptions): Byte = {
    val key = opcode -> addrMode
    opcodes.get(key).foreach(return _)
    if (options.flag(CompilationFlag.EmitIllegals)) illegalOpcodes.get(key).foreach(return _)
    if (options.flag(CompilationFlag.EmitCmosOpcodes)) cmosOpcodes.get(key).foreach(return _)
    if (options.flag(CompilationFlag.EmitCmosNopOpcodes)) cmosNopOpcodes.get(key).foreach(return _)
    if (options.flag(CompilationFlag.Emit65CE02Opcodes)) ce02Opcodes.get(key).foreach(return _)
    if (options.flag(CompilationFlag.EmitHudsonOpcodes))  hudsonOpcodes.get(key).foreach(return _)
    if (options.flag(CompilationFlag.EmitEmulation65816Opcodes)) emulation65816Opcodes.get(key).foreach(return _)
    if (options.flag(CompilationFlag.EmitNative65816Opcodes)) native65816Opcodes.get(key).foreach(return _)
    ErrorReporting.fatal("Cannot assemble an unknown opcode " + key)
  }

  private def op(op: Opcode.Value, am: AddrMode.Value, x: Int): Unit = {
    if (x < 0 || x > 0xff) ErrorReporting.fatal("Invalid code for" + (op -> am))
    opcodes(op -> am) = x.toByte
    if (am == AddrMode.Relative) opcodes(op -> AddrMode.Immediate) = x.toByte
  }

  private def cm(op: Opcode.Value, am: AddrMode.Value, x: Int): Unit = {
    if (x < 0 || x > 0xff) ErrorReporting.fatal("Invalid code for" + (op -> am))
    cmosOpcodes(op -> am) = x.toByte
  }

  private def cn(op: Opcode.Value, am: AddrMode.Value, x: Int): Unit = {
    if (x < 0 || x > 0xff) ErrorReporting.fatal("Invalid code for" + (op -> am))
    cmosNopOpcodes(op -> am) = x.toByte
  }

  private def il(op: Opcode.Value, am: AddrMode.Value, x: Int): Unit = {
    if (x < 0 || x > 0xff) ErrorReporting.fatal("Invalid code for" + (op -> am))
    illegalOpcodes(op -> am) = x.toByte
  }

  private def hu(op: Opcode.Value, am: AddrMode.Value, x: Int): Unit = {
    if (x < 0 || x > 0xff) ErrorReporting.fatal("Invalid code for" + (op -> am))
    hudsonOpcodes(op -> am) = x.toByte
  }

  private def ce(op: Opcode.Value, am: AddrMode.Value, x: Int): Unit = {
    if (x < 0 || x > 0xff) ErrorReporting.fatal("Invalid code for" + (op -> am))
    ce02Opcodes(op -> am) = x.toByte
  }

  private def em(op: Opcode.Value, am: AddrMode.Value, x: Int): Unit = {
    if (x < 0 || x > 0xff) ErrorReporting.fatal("Invalid code for" + (op -> am))
    emulation65816Opcodes(op -> am) = x.toByte
  }

  private def na(op: Opcode.Value, am: AddrMode.Value, x: Int): Unit = {
    if (x < 0 || x > 0xff) ErrorReporting.fatal("Invalid code for" + (op -> am))
    native65816Opcodes(op -> am) = x.toByte
  }

  def getStandardLegalOpcodes: Set[Int] = opcodes.values.map(_ & 0xff).toSet

  import AddrMode._
  import Opcode._

  op(ADC, Immediate, 0x69)
  op(ADC, ZeroPage, 0x65)
  op(ADC, ZeroPageX, 0x75)
  op(ADC, Absolute, 0x6D)
  op(ADC, AbsoluteX, 0x7D)
  op(ADC, AbsoluteY, 0x79)
  op(ADC, IndexedX, 0x61)
  op(ADC, IndexedY, 0x71)

  op(AND, Immediate, 0x29)
  op(AND, ZeroPage, 0x25)
  op(AND, ZeroPageX, 0x35)
  op(AND, Absolute, 0x2D)
  op(AND, AbsoluteX, 0x3D)
  op(AND, AbsoluteY, 0x39)
  op(AND, IndexedX, 0x21)
  op(AND, IndexedY, 0x31)

  op(ASL, Implied, 0x0A)
  op(ASL, ZeroPage, 0x06)
  op(ASL, ZeroPageX, 0x16)
  op(ASL, Absolute, 0x0E)
  op(ASL, AbsoluteX, 0x1E)

  op(BIT, ZeroPage, 0x24)
  op(BIT, Absolute, 0x2C)

  op(BPL, Relative, 0x10)
  op(BMI, Relative, 0x30)
  op(BVC, Relative, 0x50)
  op(BVS, Relative, 0x70)
  op(BCC, Relative, 0x90)
  op(BCS, Relative, 0xB0)
  op(BNE, Relative, 0xD0)
  op(BEQ, Relative, 0xF0)

  op(BRK, Implied, 0)

  op(CMP, Immediate, 0xC9)
  op(CMP, ZeroPage, 0xC5)
  op(CMP, ZeroPageX, 0xD5)
  op(CMP, Absolute, 0xCD)
  op(CMP, AbsoluteX, 0xDD)
  op(CMP, AbsoluteY, 0xD9)
  op(CMP, IndexedX, 0xC1)
  op(CMP, IndexedY, 0xD1)

  op(CPX, Immediate, 0xE0)
  op(CPX, ZeroPage, 0xE4)
  op(CPX, Absolute, 0xEC)

  op(CPY, Immediate, 0xC0)
  op(CPY, ZeroPage, 0xC4)
  op(CPY, Absolute, 0xCC)

  op(DEC, ZeroPage, 0xC6)
  op(DEC, ZeroPageX, 0xD6)
  op(DEC, Absolute, 0xCE)
  op(DEC, AbsoluteX, 0xDE)

  op(EOR, Immediate, 0x49)
  op(EOR, ZeroPage, 0x45)
  op(EOR, ZeroPageX, 0x55)
  op(EOR, Absolute, 0x4D)
  op(EOR, AbsoluteX, 0x5D)
  op(EOR, AbsoluteY, 0x59)
  op(EOR, IndexedX, 0x41)
  op(EOR, IndexedY, 0x51)

  op(INC, ZeroPage, 0xE6)
  op(INC, ZeroPageX, 0xF6)
  op(INC, Absolute, 0xEE)
  op(INC, AbsoluteX, 0xFE)

  op(CLC, Implied, 0x18)
  op(SEC, Implied, 0x38)
  op(CLI, Implied, 0x58)
  op(SEI, Implied, 0x78)
  op(CLV, Implied, 0xB8)
  op(CLD, Implied, 0xD8)
  op(SED, Implied, 0xF8)

  op(JMP, Absolute, 0x4C)
  op(JMP, Indirect, 0x6C)

  op(JSR, Absolute, 0x20)

  op(LDA, Immediate, 0xA9)
  op(LDA, ZeroPage, 0xA5)
  op(LDA, ZeroPageX, 0xB5)
  op(LDA, Absolute, 0xAD)
  op(LDA, AbsoluteX, 0xBD)
  op(LDA, AbsoluteY, 0xB9)
  op(LDA, IndexedX, 0xA1)
  op(LDA, IndexedY, 0xB1)

  op(LDX, Immediate, 0xA2)
  op(LDX, ZeroPage, 0xA6)
  op(LDX, ZeroPageY, 0xB6)
  op(LDX, Absolute, 0xAE)
  op(LDX, AbsoluteY, 0xBE)

  op(LDY, Immediate, 0xA0)
  op(LDY, ZeroPage, 0xA4)
  op(LDY, ZeroPageX, 0xB4)
  op(LDY, Absolute, 0xAC)
  op(LDY, AbsoluteX, 0xBC)

  op(LSR, Implied, 0x4A)
  op(LSR, ZeroPage, 0x46)
  op(LSR, ZeroPageX, 0x56)
  op(LSR, Absolute, 0x4E)
  op(LSR, AbsoluteX, 0x5E)

  op(NOP, Implied, 0xEA)

  op(ORA, Immediate, 0x09)
  op(ORA, ZeroPage, 0x05)
  op(ORA, ZeroPageX, 0x15)
  op(ORA, Absolute, 0x0D)
  op(ORA, AbsoluteX, 0x1D)
  op(ORA, AbsoluteY, 0x19)
  op(ORA, IndexedX, 0x01)
  op(ORA, IndexedY, 0x11)

  op(TAX, Implied, 0xAA)
  op(TXA, Implied, 0x8A)
  op(DEX, Implied, 0xCA)
  op(INX, Implied, 0xE8)
  op(TAY, Implied, 0xA8)
  op(TYA, Implied, 0x98)
  op(DEY, Implied, 0x88)
  op(INY, Implied, 0xC8)

  op(ROL, Implied, 0x2A)
  op(ROL, ZeroPage, 0x26)
  op(ROL, ZeroPageX, 0x36)
  op(ROL, Absolute, 0x2E)
  op(ROL, AbsoluteX, 0x3E)

  op(ROR, Implied, 0x6A)
  op(ROR, ZeroPage, 0x66)
  op(ROR, ZeroPageX, 0x76)
  op(ROR, Absolute, 0x6E)
  op(ROR, AbsoluteX, 0x7E)

  op(RTI, Implied, 0x40)
  op(RTS, Implied, 0x60)

  op(SBC, Immediate, 0xE9)
  op(SBC, ZeroPage, 0xE5)
  op(SBC, ZeroPageX, 0xF5)
  op(SBC, Absolute, 0xED)
  op(SBC, AbsoluteX, 0xFD)
  op(SBC, AbsoluteY, 0xF9)
  op(SBC, IndexedX, 0xE1)
  op(SBC, IndexedY, 0xF1)

  op(STA, ZeroPage, 0x85)
  op(STA, ZeroPageX, 0x95)
  op(STA, Absolute, 0x8D)
  op(STA, AbsoluteX, 0x9D)
  op(STA, AbsoluteY, 0x99)
  op(STA, IndexedX, 0x81)
  op(STA, IndexedY, 0x91)

  op(TXS, Implied, 0x9A)
  op(TSX, Implied, 0xBA)
  op(PHA, Implied, 0x48)
  op(PLA, Implied, 0x68)
  op(PHP, Implied, 0x08)
  op(PLP, Implied, 0x28)

  op(STX, ZeroPage, 0x86)
  op(STX, ZeroPageY, 0x96)
  op(STX, Absolute, 0x8E)

  op(STY, ZeroPage, 0x84)
  op(STY, ZeroPageX, 0x94)
  op(STY, Absolute, 0x8C)

  il(LAX, ZeroPage, 0xA7)
  il(LAX, ZeroPageY, 0xB7)
  il(LAX, Absolute, 0xAF)
  il(LAX, AbsoluteY, 0xBF)
  il(LAX, IndexedX, 0xA3)
  il(LAX, IndexedY, 0xB3)

  il(SAX, ZeroPage, 0x87)
  il(SAX, ZeroPageY, 0x97)
  il(SAX, Absolute, 0x8F)
  il(TAS, AbsoluteY, 0x9B)
  il(AHX, AbsoluteY, 0x9F)
  il(SAX, IndexedX, 0x83)
  il(AHX, IndexedY, 0x93)
  il(SHY, AbsoluteX, 0x9C)

  il(ANC, Immediate, 0x0B)
  il(ALR, Immediate, 0x4B)
  il(ARR, Immediate, 0x6B)
  il(XAA, Immediate, 0x8B)
  il(LXA, Immediate, 0xAB)
  il(SBX, Immediate, 0xCB)

  il(SLO, ZeroPage, 0x07)
  il(SLO, ZeroPageX, 0x17)
  il(SLO, IndexedX, 0x03)
  il(SLO, IndexedY, 0x13)
  il(SLO, Absolute, 0x0F)
  il(SLO, AbsoluteX, 0x1F)
  il(SLO, AbsoluteY, 0x1B)

  il(RLA, ZeroPage, 0x27)
  il(RLA, ZeroPageX, 0x37)
  il(RLA, IndexedX, 0x23)
  il(RLA, IndexedY, 0x33)
  il(RLA, Absolute, 0x2F)
  il(RLA, AbsoluteX, 0x3F)
  il(RLA, AbsoluteY, 0x3B)

  il(SRE, ZeroPage, 0x47)
  il(SRE, ZeroPageX, 0x57)
  il(SRE, IndexedX, 0x43)
  il(SRE, IndexedY, 0x53)
  il(SRE, Absolute, 0x4F)
  il(SRE, AbsoluteX, 0x5F)
  il(SRE, AbsoluteY, 0x5B)

  il(RRA, ZeroPage, 0x67)
  il(RRA, ZeroPageX, 0x77)
  il(RRA, IndexedX, 0x63)
  il(RRA, IndexedY, 0x73)
  il(RRA, Absolute, 0x6F)
  il(RRA, AbsoluteX, 0x7F)
  il(RRA, AbsoluteY, 0x7B)

  il(DCP, ZeroPage, 0xC7)
  il(DCP, ZeroPageX, 0xD7)
  il(DCP, IndexedX, 0xC3)
  il(DCP, IndexedY, 0xD3)
  il(DCP, Absolute, 0xCF)
  il(DCP, AbsoluteX, 0xDF)
  il(DCP, AbsoluteY, 0xDB)

  il(ISC, ZeroPage, 0xE7)
  il(ISC, ZeroPageX, 0xF7)
  il(ISC, IndexedX, 0xE3)
  il(ISC, IndexedY, 0xF3)
  il(ISC, Absolute, 0xEF)
  il(ISC, AbsoluteX, 0xFF)
  il(ISC, AbsoluteY, 0xFB)

  il(NOP, Immediate, 0x80)
  il(NOP, ZeroPage, 0x44)
  il(NOP, ZeroPageX, 0x54)
  il(NOP, Absolute, 0x5C)
  il(NOP, AbsoluteX, 0x1C)

  cn(NOP, Immediate, 0x02)
  cn(NOP, ZeroPage, 0x44)
  cn(NOP, ZeroPageX, 0x54)
  cn(NOP, Absolute, 0x5C)

  cm(STZ, ZeroPage, 0x64)
  cm(STZ, ZeroPageX, 0x74)
  cm(STZ, Absolute, 0x9C)
  cm(STZ, AbsoluteX, 0x9E)

  cm(PHX, Implied, 0xDA)
  cm(PHY, Implied, 0x5A)
  cm(PLX, Implied, 0xFA)
  cm(PLY, Implied, 0x7A)

  cm(ORA, IndexedZ, 0x12)
  cm(AND, IndexedZ, 0x32)
  cm(EOR, IndexedZ, 0x52)
  cm(ADC, IndexedZ, 0x72)
  cm(STA, IndexedZ, 0x92)
  cm(LDA, IndexedZ, 0xB2)
  cm(CMP, IndexedZ, 0xD2)
  cm(SBC, IndexedZ, 0xF2)

  cm(TSB, ZeroPage, 0x04)
  cm(TSB, Absolute, 0x0C)
  cm(TRB, ZeroPage, 0x14)
  cm(TRB, Absolute, 0x1C)

  cm(BRA, Relative, 0x80)
  cm(BIT, ZeroPageX, 0x34)
  cm(BIT, AbsoluteX, 0x3C)
  cm(INC, Implied, 0x1A)
  cm(DEC, Implied, 0x3A)
  cm(JMP, AbsoluteIndexedX, 0x7C)
  cm(WAI, Implied, 0xCB)
  cm(STP, Implied, 0xDB)

  ce(CPZ, Immediate, 0xC2)
  ce(CPZ, ZeroPage, 0xD4)
  ce(CPZ, Absolute, 0xDC)
  ce(DEZ, Implied, 0x3B)
  ce(INZ, Implied,0x1B )
  ce(DEC_W, ZeroPage, 0xC3)
  ce(INC_W, ZeroPage, 0xE3)
  ce(ASL_W, Absolute, 0xCB)
  // TODO: or is it ROL_W?
  ce(ROR_W, Absolute, 0xEB)
  ce(ASR, Implied, 0x43)
  ce(ASR, ZeroPage, 0x44)
  ce(ASR, ZeroPageX, 0x54)
  ce(LDZ, Immediate, 0xA3)
  ce(LDZ, Absolute, 0xAB)
  ce(LDZ, AbsoluteX, 0xBB)
  ce(TAB, Implied, 0x5B)
  ce(TBA, Implied, 0x7B)
  ce(TAZ, Implied, 0x4B)
  ce(TZA, Implied, 0x6B)
  ce(TSY, Implied, 0x0B)
  ce(TYS, Implied, 0x2B)
  ce(PHW, WordImmediate, 0xF4)
  ce(PHW, Absolute, 0xFC)
  ce(PHZ, Implied, 0xDB)
  ce(PLZ, Implied, 0xFB)
//  ce(CLE, Implied, )
//  ce(SEE, Implied, )
//  ce(BSR, , )

  hu(CLY, Implied, 0xC2)
  hu(CLX, Implied, 0x82)
  hu(CLA, Implied, 0x62)
  hu(CSH, Implied, 0xD4)
  hu(CSL, Implied, 0x54)
  hu(HuSAX, Implied, 0x22)
  hu(SAY, Implied, 0x42)
  hu(SXY, Implied, 0x02)
  hu(TAM, Immediate, 0x53)
  hu(TMA, Immediate, 0x43)

  em(ORA, Stack, 0x03)
  em(ORA, IndexedSY, 0x13)
  na(ORA, LongIndexedZ, 0x07)
  na(ORA, LongIndexedY, 0x17)
  na(ORA, LongAbsolute, 0x0F)
  na(ORA, LongAbsoluteX, 0x1F)
  em(AND, Stack, 0x23)
  em(AND, IndexedSY, 0x33)
  na(AND, LongIndexedZ, 0x27)
  na(AND, LongIndexedY, 0x37)
  na(AND, LongAbsolute, 0x2F)
  na(AND, LongAbsoluteX, 0x3F)
  em(EOR, Stack, 0x43)
  em(EOR, IndexedSY, 0x53)
  na(EOR, LongIndexedZ, 0x47)
  na(EOR, LongIndexedY, 0x57)
  na(EOR, LongAbsolute, 0x4F)
  na(EOR, LongAbsoluteX, 0x5F)
  em(ADC, Stack, 0x63)
  em(ADC, IndexedSY, 0x73)
  na(ADC, LongIndexedZ, 0x67)
  na(ADC, LongIndexedY, 0x77)
  na(ADC, LongAbsolute, 0x6F)
  na(ADC, LongAbsoluteX, 0x7F)
  em(STA, Stack, 0x83)
  em(STA, IndexedSY, 0x93)
  na(STA, LongIndexedZ, 0x87)
  na(STA, LongIndexedY, 0x97)
  na(STA, LongAbsolute, 0x8F)
  na(STA, LongAbsoluteX, 0x9F)
  em(LDA, Stack, 0xA3)
  em(LDA, IndexedSY, 0xB3)
  na(LDA, LongIndexedZ, 0xA7)
  na(LDA, LongIndexedY, 0xB7)
  na(LDA, LongAbsolute, 0xAF)
  na(LDA, LongAbsoluteX, 0xBF)
  em(CMP, Stack, 0xA3)
  em(CMP, IndexedSY, 0xB3)
  na(CMP, LongIndexedZ, 0xA7)
  na(CMP, LongIndexedY, 0xB7)
  na(CMP, LongAbsolute, 0xAF)
  na(CMP, LongAbsoluteX, 0xBF)

  em(COP, Immediate, 0x02)
  em(XBA, Implied, 0xEB)
  em(TXY, Implied, 0x9B)
  em(TYX, Implied, 0xBB)


  na(RTL, Implied, 0x6B)
  na(JMP, LongAbsolute, 0x5C)
  na(JMP, LongIndirect, 0x7C)
  na(BRL, LongRelative, 0x82)

  em(PHD, Implied, 0x0B)
  em(PLD, Implied, 0x2B)
  em(PHB, Implied, 0x8B)
  em(PLB, Implied, 0xAB)
  em(PHK, Implied, 0x4B)

  na(REP, Immediate, 0xC2)
  na(SEP, Immediate, 0xE2)

  na(XCE, Implied, 0xFB)
  na(TCD, Implied, 0x5B)
  na(TDC, Implied, 0x7B)
  na(TSC, Implied, 0x3B)
  na(TCS, Implied, 0x1B)

  for {
    ((narrow, am), code) <- emulation65816Opcodes ++ opcodes ++ cmosOpcodes ++ native65816Opcodes
    wide <- Opcode.widen(narrow)
  } na(wide, if (am == Immediate) WordImmediate else am, code & 0xff)

}
