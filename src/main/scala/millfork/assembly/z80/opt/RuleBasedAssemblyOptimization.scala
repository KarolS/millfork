package millfork.assembly.z80.opt

import millfork.CompilationOptions
import millfork.assembly._
import millfork.assembly.opt.{AnyStatus, SingleStatus}
import millfork.assembly.z80._
import millfork.env._
import millfork.error.ErrorReporting
import millfork.node.ZRegister

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */

object FlowInfoRequirement extends Enumeration {

  val NoRequirement, JustLabels, BothFlows, ForwardFlow, BackwardFlow = Value

  def assertForward(x: FlowInfoRequirement.Value): Unit = x match {
    case BothFlows | ForwardFlow => ()
    case NoRequirement | JustLabels | BackwardFlow => ErrorReporting.fatal("Forward flow info required")
  }

  def assertBackward(x: FlowInfoRequirement.Value): Unit = x match {
    case BothFlows | BackwardFlow => ()
    case NoRequirement | JustLabels | ForwardFlow => ErrorReporting.fatal("Backward flow info required")
  }
}

trait AssemblyRuleSet{
  def flatten: Seq[AssemblyRule]
}

class RuleBasedAssemblyOptimization(val name: String, val needsFlowInfo: FlowInfoRequirement.Value, val rules: AssemblyRuleSet*) extends AssemblyOptimization[ZLine]{

  private val actualRules = rules.flatMap(_.flatten)
  actualRules.foreach(_.pattern.validate(needsFlowInfo))

  override def optimize(f: NormalFunction, code: List[ZLine], optimizationContext: OptimizationContext): List[ZLine] = {
    val effectiveCode = code.map(a => a.copy(parameter = a.parameter.quickSimplify))
    val taggedCode = FlowAnalyzer.analyze(f, effectiveCode, optimizationContext.options, needsFlowInfo)
    optimizeImpl(f, taggedCode, optimizationContext)
  }

  def optimizeImpl(f: NormalFunction, code: List[(FlowInfo, ZLine)], optimizationContext: OptimizationContext): List[ZLine] = {
    code match {
      case Nil => Nil
      case head :: tail =>
        for ((rule, index) <- actualRules.zipWithIndex) {
          val ctx = new AssemblyMatchingContext(optimizationContext.options)
          rule.pattern.matchTo(ctx, code) match {
            case Some(rest: List[(FlowInfo, ZLine)]) =>
              val matchedChunkToOptimize: List[ZLine] = code.take(code.length - rest.length).map(_._2)
              val optimizedChunk: List[ZLine] = rule.result(matchedChunkToOptimize, ctx)
              ErrorReporting.debug(s"Applied $name ($index)")
              if (needsFlowInfo != FlowInfoRequirement.NoRequirement) {
                val before = code.head._1.statusBefore
                val after = code(matchedChunkToOptimize.length - 1)._1.importanceAfter
                ErrorReporting.trace(s"Before: $before")
                ErrorReporting.trace(s"After:  $after")
              }
              matchedChunkToOptimize.filter(_.isPrintable).foreach(l => ErrorReporting.trace(l.toString))
              ErrorReporting.trace("     ↓")
              optimizedChunk.filter(_.isPrintable).foreach(l => ErrorReporting.trace(l.toString))
              if (needsFlowInfo != FlowInfoRequirement.NoRequirement) {
                return optimizedChunk ++ optimizeImpl(f, rest, optimizationContext)
              } else {
                return optimize(f, optimizedChunk ++ rest.map(_._2), optimizationContext)
              }
            case None => ()
          }
        }
        head._2 :: optimizeImpl(f, tail, optimizationContext)
    }
  }
}

class AssemblyMatchingContext(val compilationOptions: CompilationOptions) {
  private val map = mutable.Map[Int, Any]()

  override def toString: String = map.mkString(", ")

  def addObject(i: Int, o: Any): Boolean = {
    if (map.contains(i)) {
      map(i) == o
    } else {
      map(i) = o
      true
    }
  }

  def dontMatch(i: Int, o: Any): Boolean = {
    if (map.contains(i)) {
      map(i) != o
    } else {
      false
    }
  }

  private def getImpl[T: Manifest](i: Int): AnyRef = {
    if (!map.contains(i)) return null
    val t = map(i)
    val clazz = implicitly[Manifest[T]].runtimeClass match {
      case java.lang.Integer.TYPE => classOf[java.lang.Integer]
      case java.lang.Boolean.TYPE => classOf[java.lang.Boolean]
      // TODO
      case x => x
    }
    if (clazz.isInstance(t)) {
      t.asInstanceOf[AnyRef]
    } else {
      if (i eq null) {
        ErrorReporting.fatal(s"Value at index $i is null")
      } else {
        throw new IllegalStateException(s"Value at index $i is a ${t.getClass.getSimpleName}, not a ${clazz.getSimpleName}")
      }
    }
  }

  def get[T: Manifest](i: Int): T = {
    val v = getImpl[T](i)
    if (v eq null) {
      ErrorReporting.fatal(s"Value at index $i is null")
    }
    v.asInstanceOf[T]
  }

  def getOrDefault[T: Manifest](i: Int, defau: T): T = {
    val v = getImpl[T](i)
    if (v eq null) {
      defau
    } else {
      v.asInstanceOf[T]
    }
  }

  def isExternallyLinearBlock(i: Int): Boolean = {
    val labels = mutable.Set[String]()
    val jumps = mutable.Set[String]()
    import millfork.assembly.z80.ZOpcode._
    get[List[ZLine]](i).foreach {
      // JSR and BSR are allowed
      case ZLine(RET | RST | RETI | RETN, _, _, _) =>
        return false
      case ZLine(JP | JR, OneRegister(_), _, _) =>
        return false
      case ZLine(JP | JR | DJNZ, _, MemoryAddressConstant(Label(l)), _) =>
        jumps += l
      case ZLine(LABEL, _, MemoryAddressConstant(Label(l)), _) =>
        labels += l
      case ZLine(JP | JR | DJNZ, _, _, _) =>
        return false
      case _ => ()
    }
    // if a jump leads inside the block, then it's internal
    // if a jump leads outside the block, then it's external
    jumps --= labels
    jumps.isEmpty
  }

}

object HelperCheckers {
  import ZOpcode._
  import ZRegister._
  private def isBad(l: ZLine): Boolean = {
    l.opcode match {
      case LD | LD_16 | ADD_16 | ADC_16 | SBC_16 => l.registers match {
        case TwoRegisters(MEM_HL | MEM_IX_D | MEM_IY_D | MEM_BC | MEM_DE, _) => true
        case TwoRegisters(_, MEM_HL | MEM_IX_D | MEM_IY_D | MEM_BC | MEM_DE) => true
        case TwoRegisters(_, _) => false
      }
      case ADD | SUB | SBC | ADC | XOR | CP | OR | AND => l.registers match {
        case OneRegister(MEM_HL | MEM_IX_D | MEM_IY_D | MEM_BC | MEM_DE) => true
        case OneRegister(_) => false
      }
      case POP | PUSH => false
      case _ => true // TODO
    }
  }

  def distinctThings(a: String, b: String): Boolean = {
    a.takeWhile(_ != '.') != b.takeWhile(_ != '.')
  }

  def memoryAccessDoesntOverlap(l1: ZLine, l2: ZLine): Boolean = {
    if (!l1.readsMemory && !l1.changesMemory) return true
    if (!l2.readsMemory && !l2.changesMemory) return true
    if (isBad(l1) || isBad(l2)) return false

    (l1.parameter.quickSimplify, l2.parameter.quickSimplify) match {
      case (NumericConstant(n1, _), NumericConstant(n2, _)) => n1 != n2
      case (MemoryAddressConstant(_: ThingInMemory), NumericConstant(_, _)) => true // TODO: ???
      case (NumericConstant(_, _), MemoryAddressConstant(_: ThingInMemory)) => true // TODO: ???
      case (CompoundConstant(MathOperator.Plus | MathOperator.Minus, MemoryAddressConstant(a: ThingInMemory), NumericConstant(_, _)), NumericConstant(_, _)) => true // TODO: ???
      case (NumericConstant(_, _), CompoundConstant(MathOperator.Plus | MathOperator.Minus, MemoryAddressConstant(a: ThingInMemory), NumericConstant(_, _))) => true // TODO: ???
      case (MemoryAddressConstant(a: ThingInMemory), MemoryAddressConstant(b: ThingInMemory)) => distinctThings(a.name, b.name) // TODO: ???
      case (CompoundConstant(op@(MathOperator.Plus | MathOperator.Minus), MemoryAddressConstant(a: ThingInMemory), NumericConstant(offset, _)),
      MemoryAddressConstant(b: ThingInMemory)) =>
        if (a.name == b.name) {
          offset.abs > 1
        } else {
          distinctThings(a.name, b.name) // TODO: ???
        }
      case (MemoryAddressConstant(a: ThingInMemory),
      CompoundConstant(op@(MathOperator.Plus | MathOperator.Minus), MemoryAddressConstant(b: ThingInMemory), NumericConstant(offset, _))) =>
        if (a.name == b.name) {
          offset.abs > 1
        } else {
          distinctThings(a.name, b.name) // TODO: ???
        }
      case _ => false
    }
  }
}
case class AssemblyRule(pattern: AssemblyPattern, result: (List[ZLine], AssemblyMatchingContext) => List[ZLine]) extends AssemblyRuleSet {
  override def flatten: Seq[AssemblyRule] = List(this)
}

case class MultipleAssemblyRules(list: Seq[AssemblyRuleSet]) extends AssemblyRuleSet {
  override def flatten: Seq[AssemblyRule] = list.flatMap(_.flatten)
}

trait AssemblyPattern {

  def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = ()

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, ZLine)]): Option[List[(FlowInfo, ZLine)]]

  def ~(x: AssemblyPattern) = Concatenation(this, x)

  def ~(x: AssemblyLinePattern) = Concatenation(this, x)

  def ~~>(result: (List[ZLine], AssemblyMatchingContext) => List[ZLine]) = AssemblyRule(this, result)

  def ~~>(result: List[ZLine] => List[ZLine]) = AssemblyRule(this, (code, _) => result(code))

  def capture(i: Int) = Capture(i, this)

  def captureLength(i: Int) = CaptureLength(i, this)

}

case class Capture(i: Int, pattern: AssemblyPattern) extends AssemblyPattern {
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, ZLine)]): Option[List[(FlowInfo, ZLine)]] =
    for {
      rest <- pattern.matchTo(ctx, code)
    } yield {
      ctx.addObject(i, code.take(code.length - rest.length).map(_._2))
      rest
    }

  override def toString: String = s"(?<$i>$pattern)"
}

case class CaptureLine(i: Int, pattern: AssemblyLinePattern) extends AssemblyLinePattern {

  override def toString: String = s"(?<$i>$pattern)"

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean = {
    pattern.matchLineTo(ctx, flowInfo, line) && ctx.addObject(i, line)
  }
}

case class CaptureLength(i: Int, pattern: AssemblyPattern) extends AssemblyPattern {
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, ZLine)]): Option[List[(FlowInfo, ZLine)]] =
    for {
      rest <- pattern.matchTo(ctx, code)
    } yield {
      ctx.addObject(i, code.length - rest.length)
      rest
    }

  override def toString: String = s"(?<$i>$pattern)"
}


case class Where(predicate: (AssemblyMatchingContext => Boolean)) extends AssemblyPattern {
  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, ZLine)]): Option[List[(FlowInfo, ZLine)]] = {
    if (predicate(ctx)) Some(code) else None
  }

  override def toString: String = "Where(...)"
}

case class Concatenation(l: AssemblyPattern, r: AssemblyPattern) extends AssemblyPattern {

  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    l.validate(needsFlowInfo)
    r.validate(needsFlowInfo)
  }

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, ZLine)]): Option[List[(FlowInfo, ZLine)]] = {
    for {
      middle <- l.matchTo(ctx, code)
      end <- r.matchTo(ctx, middle)
    } yield end
  }

  override def toString: String = (l, r) match {
    case (_: Both, _: Both) => s"($l) · ($r)"
    case (_, _: Both) => s"$l · ($r)"
    case (_: Both, _) => s"($l) · $r"
    case _ => s"$l · $r"
  }
}

case class Many(rule: AssemblyLinePattern) extends AssemblyPattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    rule.validate(needsFlowInfo)
  }

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, ZLine)]): Option[List[(FlowInfo, ZLine)]] = {
    var c = code
    while (true) {
      c match {
        case Nil =>
          return Some(Nil)
        case x :: xs =>
          if (rule.matchLineTo(ctx, x._1, x._2)) {
            c = xs
          } else {
            return Some(c)
          }
      }
    }
    None
  }

  override def toString: String = s"[$rule]*"
}

case class ManyWhereAtLeastOne(rule: AssemblyLinePattern, atLeastOneIsThis: AssemblyLinePattern) extends AssemblyPattern {

  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    rule.validate(needsFlowInfo)
  }

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, ZLine)]): Option[List[(FlowInfo, ZLine)]] = {
    var c = code
    var oneFound = false
    while (true) {
      c match {
        case Nil =>
          return Some(Nil)
        case x :: xs =>
          if (atLeastOneIsThis.matchLineTo(ctx, x._1, x._2)) {
            oneFound = true
          }
          if (rule.matchLineTo(ctx, x._1, x._2)) {
            c = xs
          } else {
            if (oneFound) {
              return Some(c)
            } else {
              return None
            }
          }
      }
    }
    None
  }

  override def toString: String = s"[∃$atLeastOneIsThis:$rule]*"
}

case class Opt(rule: AssemblyLinePattern) extends AssemblyPattern {

  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    rule.validate(needsFlowInfo)
  }

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, ZLine)]): Option[List[(FlowInfo, ZLine)]] = {
    code match {
      case Nil =>
        Some(Nil)
      case x :: xs =>
        if (rule.matchLineTo(ctx, x._1, x._2)) {
          Some(xs)
        } else {
          Some(code)
        }
    }
  }

  override def toString: String = s"[$rule]?"
}

trait AssemblyLinePattern extends AssemblyPattern {
  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, ZLine)]): Option[List[(FlowInfo, ZLine)]] = code match {
    case Nil => None
    case x :: xs => if (matchLineTo(ctx, x._1, x._2)) Some(xs) else None
  }

  def captureLine(i: Int): AssemblyLinePattern = CaptureLine(i, this)

  def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean

  def unary_! : AssemblyLinePattern = Not(this)

  def ? : AssemblyPattern = Opt(this)

  def * : AssemblyPattern = Many(this)

  def + : AssemblyPattern = this ~ Many(this)

  def |(x: AssemblyLinePattern): AssemblyLinePattern = EitherPattern(this, x)

  def &(x: AssemblyLinePattern): AssemblyLinePattern = Both(this, x)
}

trait TrivialAssemblyLinePattern extends AssemblyLinePattern with (ZLine => Boolean) {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean = this (line)
}

case class Match(predicate: (AssemblyMatchingContext => Boolean)) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean = predicate(ctx)

  override def toString: String = "Match(...)"
}

//noinspection LanguageFeature
object AssemblyLinePattern {
  implicit def __implicitOpcodeIn(ops: Set[ZOpcode.Value]): AssemblyLinePattern = HasOpcodeIn(ops)
}

case class MatchRegister(register: ZRegister.Value, i: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    flowInfo.statusBefore.getRegister(register) match {
      case SingleStatus(value) => ctx.addObject(i, value)
      case _ => false
    }
}

case class MatchValueAtIxOffset(offset: Int, i: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    flowInfo.statusBefore.memIx.getOrElse(offset, AnyStatus) match {
      case SingleStatus(value) => ctx.addObject(i, value)
      case _ => false
    }
}

case class MatchValueAtMatchedIxOffset(oi: Int, vi: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean = {
    val offset = ctx.get[Int](oi)
    flowInfo.statusBefore.memIx.getOrElse(offset, AnyStatus) match {
      case SingleStatus(value) => ctx.addObject(vi, value)
      case _ => false
    }
  }
}

case class MatchImmediate(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    line.registers match {
      case TwoRegisters(_, ZRegister.IMM_16 | ZRegister.IMM_8) | OneRegister(ZRegister.IMM_8 | ZRegister.IMM_16) => ctx.addObject(i, line.parameter.quickSimplify)
      case _ => false
    }
}

case class RegisterAndOffset(register: ZRegister.Value, offset: Int) {
  def toOneRegister: ZRegisters = register match {
    case ZRegister.MEM_IX_D | ZRegister.MEM_IY_D => OneRegisterOffset(register, offset)
    case _ =>
      if (offset != 0) ???
      OneRegister(register)
  }
}

case class MatchSourceRegisterAndOffset(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    line.registers match {
      case TwoRegisters(_, s) => ctx.addObject(i, RegisterAndOffset(s, 0))
      case TwoRegistersOffset(_, s, o) => ctx.addObject(i, RegisterAndOffset(s, o))
      case _ => false
    }
}

case class MatchTargetRegisterAndOffset(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    line.registers match {
      case TwoRegisters(t, _) => ctx.addObject(i, RegisterAndOffset(t, 0))
      case TwoRegistersOffset(t, _, o) => ctx.addObject(i, RegisterAndOffset(t, o))
      case _ => false
    }
}

case class MatchSoleRegisterAndOffset(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    line.registers match {
      case OneRegister(t) => ctx.addObject(i, RegisterAndOffset(t, 0))
      case OneRegisterOffset(t, o) => ctx.addObject(i, RegisterAndOffset(t, o))
      case _ => false
    }
}

case class DoesntChangeMatchedRegisterAndOffset(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean = {
    val ro = ctx.get[RegisterAndOffset](i)
    import ZRegister._
    ro.register match {
      case AF | SP => false // ?
      case MEM_ABS_8 | MEM_ABS_16 => !line.changesMemory
      case MEM_HL => !line.changesMemory && !line.changesRegister(ZRegister.HL)
      case MEM_BC => !line.changesMemory && !line.changesRegister(ZRegister.BC)
      case MEM_DE => !line.changesMemory && !line.changesRegister(ZRegister.DE)
      case _ => !line.changesRegisterAndOffset(ro.register, ro.offset)
    }
  }
}


case class DoesntConcernMatchedRegisterAndOffset(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean = {
    val ro = ctx.get[RegisterAndOffset](i)
    import ZRegister._
    ro.register match {
      case AF | SP => false // ?
      case MEM_ABS_8 | MEM_ABS_16 => !line.changesMemory && !line.readsMemory
      case MEM_HL => !line.changesMemory && !line.readsMemory && !line.changesRegister(ZRegister.HL) && !line.readsRegister(ZRegister.HL)
      case MEM_BC => !line.changesMemory && !line.readsMemory && !line.changesRegister(ZRegister.BC) && !line.readsRegister(ZRegister.BC)
      case MEM_DE => !line.changesMemory && !line.readsMemory && !line.changesRegister(ZRegister.DE) && !line.readsRegister(ZRegister.DE)
      case _ => !line.changesRegisterAndOffset(ro.register, ro.offset) && !line.readsRegisterAndOffset(ro.register, ro.offset)
    }
  }
}

case class MatchParameter(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    line.registers match {
      case TwoRegisters(_, ZRegister.IMM_16 | ZRegister.IMM_8 | ZRegister.MEM_ABS_8 | ZRegister.MEM_ABS_16) |
           TwoRegisters(ZRegister.IMM_16 | ZRegister.IMM_8 | ZRegister.MEM_ABS_8 | ZRegister.MEM_ABS_16, _) |
           OneRegister(ZRegister.IMM_8 | ZRegister.IMM_16 | ZRegister.MEM_ABS_8 | ZRegister.MEM_ABS_16) => ctx.addObject(i, line.parameter.quickSimplify)
      case _ => false
    }
}

case class MatchParameterOrNothing(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    ctx.addObject(i, line.parameter.quickSimplify)
}

case class MatchJumpTarget(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    line.registers match {
      case NoRegisters | IfFlagClear(_) | IfFlagSet(_) => ctx.addObject(i, line.parameter.quickSimplify)
      case _ => false
    }
}

case class MatchConstantInHL(i: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    flowInfo.statusBefore.hl match {
      case SingleStatus(value) => ctx.addObject(i, value)
      case _ => false
    }
}

case class MatchOpcode(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    ctx.addObject(i, line.opcode)
}

case class HasRegister(register: ZRegister.Value, value: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    flowInfo.statusBefore.getRegister(register).contains(value)
}

case class DoesntMatterWhatItDoesWith(registers: ZRegister.Value*) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertBackward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    registers.forall(r => flowInfo.importanceAfter.getRegister(r) != Important)

  override def toString: String = registers.mkString("[¯\\_(ツ)_/¯:", ",", "]")
}

case object DoesntMatterWhatItDoesWithFlags extends AssemblyLinePattern {

  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertBackward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    ZFlag.values.forall(r => flowInfo.importanceAfter.getFlag(r) != Important)

  override def toString: String = "[¯\\_(ツ)_/¯:F]"
}

case object DoesntMatterWhatItDoesWithFlagsExceptCarry extends AssemblyLinePattern {

  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertBackward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    ZFlag.values.forall(r => r == ZFlag.C || flowInfo.importanceAfter.getFlag(r) != Important)

  override def toString: String = "[¯\\_(ツ)_/¯:F\\C]"
}

case class HasSet(flag: ZFlag.Value) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    flowInfo.statusBefore.getFlag(flag).exists(_ == true)
}

case class HasClear(flag: ZFlag.Value) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    flowInfo.statusBefore.getFlag(flag).exists(_ == true)
}

case object Anything extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean = true
}

case class Not(inner: AssemblyLinePattern) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = inner.validate(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    !inner.matchLineTo(ctx, flowInfo, line)

  override def toString: String = "¬" + inner
}

case class Both(l: AssemblyLinePattern, r: AssemblyLinePattern) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    l.validate(needsFlowInfo)
    r.validate(needsFlowInfo)
  }

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    l.matchLineTo(ctx, flowInfo, line) && r.matchLineTo(ctx, flowInfo, line)

  override def toString: String = l + " ∧ " + r
}

case class EitherPattern(l: AssemblyLinePattern, r: AssemblyLinePattern) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    l.validate(needsFlowInfo)
    r.validate(needsFlowInfo)
  }

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    l.matchLineTo(ctx, flowInfo, line) || r.matchLineTo(ctx, flowInfo, line)

  override def toString: String = s"($l ∨ $r)"
}

case object Elidable extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    line.elidable
}

case object DebugMatching extends AssemblyPattern {
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, ZLine)]): Option[List[(FlowInfo, ZLine)]] = {
    println(ctx)
    code.foreach(println)
    Some(code)
  }
}

case object Linear extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    !ZOpcodeClasses.NonLinear(line.opcode)
}

case object LinearOrLabel extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean =
    line.opcode == ZOpcode.LABEL || !ZOpcodeClasses.NonLinear(line.opcode)
}

case class Reads(register: ZRegister.Value) extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean = line.readsRegister(register)
}

case class Changes(register: ZRegister.Value) extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean = line.changesRegister(register)
}

case class Concerns(register: ZRegister.Value) extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean = line.readsRegister(register) || line.changesRegister(register)
}

case object ReadsMemory extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean = line.readsMemory
}

case object ChangesMemory extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean = line.changesMemory
}

case class DoesntChangeMemoryAt(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean = {
    HelperCheckers.memoryAccessDoesntOverlap(ctx.get[ZLine](i), line)
  }
}

case object ConcernsMemory extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean = line.readsMemory || line.changesMemory
}

case class HasOpcode(op: ZOpcode.Value) extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean =
    line.opcode == op

  override def toString: String = op.toString
}

case class Is8BitLoad(target:ZRegister.Value, source: ZRegister.Value) extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean =
    line.opcode == ZOpcode.LD && line.registers == TwoRegisters(target, source)

  override def toString: String = s"LD $target,$source"
}

case class Is16BitLoad(target:ZRegister.Value, source: ZRegister.Value) extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean =
    line.opcode == ZOpcode.LD_16 && line.registers == TwoRegisters(target, source)

  override def toString: String = s"LD $target,$source"
}

case class IsRegular8BitLoadFrom(source: ZRegister.Value) extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean =
    line.opcode == ZOpcode.LD && (line.registers match {
      case TwoRegistersOffset(ZRegister.I | ZRegister.MEM_ABS_8 | ZRegister.R | ZRegister.MEM_DE | ZRegister.MEM_BC, _, _) => false
      case TwoRegisters(ZRegister.I | ZRegister.MEM_ABS_8 | ZRegister.R | ZRegister.MEM_DE | ZRegister.MEM_BC, _) => false
      case TwoRegistersOffset(t, s, _) => s == source
      case TwoRegisters(t, s) => s == source
    })

  override def toString: String = "LD _," + source
}

object NoOffset extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean =
    line.registers match {
      case _:TwoRegistersOffset => false
      case _:OneRegisterOffset => false
      case _ => true
    }
}

case class Is8BitLoadTo(target: ZRegister.Value) extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean =
    line.opcode == ZOpcode.LD && (line.registers match {
      case TwoRegistersOffset(t, s, _) => target == t
      case TwoRegisters(t, s) => target == t && (s match {
        case ZRegister.I | ZRegister.MEM_ABS_8 | ZRegister.R => false
        case _ => true
      })
    })

  override def toString: String = s"LD $target,_"
}

case class MatchSourceIxOffsetOf8BitLoad(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean = {
    line.opcode == ZOpcode.LD && (line.registers match {
      case TwoRegistersOffset(_, ZRegister.MEM_IX_D, offset) => ctx.addObject(i, offset)
      case _ => false
    })
  }

  override def toString: String = "LD (IX,!),_"
}

case class MatchIxOffset(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean = {
    line.registers match {
      case OneRegisterOffset(ZRegister.MEM_IX_D, offset) => ctx.addObject(i, offset)
      case _ => false
    }
  }

  override def toString: String = "LD (IX,!),_"
}

case class MatchTargetIxOffsetOf8BitLoad(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean = {
    line.opcode == ZOpcode.LD && (line.registers match {
      case TwoRegistersOffset(ZRegister.MEM_IX_D, _, offset) => ctx.addObject(i, offset)
      case _ => false
    })
  }

  override def toString: String = "LD (IX,!),_"
}

case class MatchUnimportantIxOffset(i: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertBackward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean = {
    val offset = ctx.get[Int](i)
    flowInfo.importanceAfter.memIx.getOrElse(offset, Unimportant) == Unimportant
  }
}

case class Is16BitLoadTo(target: ZRegister.Value) extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean =
    line.opcode == ZOpcode.LD_16 && (line.registers match {
      case TwoRegistersOffset(t, s, _) => target == t
      case TwoRegisters(t, s) => target == t
    })

  override def toString: String = s"LD_16 $target,_"
}

case class HasRegisterParam(register: ZRegister.Value) extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean =
    line.registers match {
      case OneRegister(r) => r == register
      case _ => false
    }
}

case class HasRegisters(registers: ZRegisters) extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean = line.registers == registers
}

case class RefersTo(identifier: String, offset: Int = 999) extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean = {
    line.parameter match {
      case MemoryAddressConstant(th) =>
        (offset == 999 || offset == 0) && th.name == identifier
      case CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(nn, _)) =>
        (offset == 999 || offset == nn) && th.name == identifier
      case CompoundConstant(MathOperator.Plus, NumericConstant(nn, _), MemoryAddressConstant(th)) =>
        (offset == 999 || offset == nn) && th.name == identifier
      case _ => false
    }
  }

  override def toString: String = s"<$identifier+$offset>"
}

case class CallsAnyOf(identifiers: Set[String]) extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean = {
    line.parameter match {
      case MemoryAddressConstant(th) => identifiers(th.name)
      case _ => false
    }
  }

  override def toString: String = identifiers.mkString("(CALL {", ",", "})")
}

case class CallsAnyExcept(identifiers: Set[String]) extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean = {
    line.parameter match {
      case MemoryAddressConstant(th) => !identifiers(th.name)
      case _ => false
    }
  }

  override def toString: String = identifiers.mkString("(JSR ¬{", ",", "})")
}

case class HasOpcodeIn(ops: Set[ZOpcode.Value]) extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean =
    ops(line.opcode)

  override def toString: String = ops.mkString("{", ",", "}")
}

case class Has8BitImmediate(i: Int) extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean = (line.registers match {
    case TwoRegisters(_, ZRegister.IMM_8) => true
    case OneRegister(ZRegister.IMM_8) => true
    case _ => false
  }) && (line.parameter.quickSimplify match {
    case NumericConstant(j, _) => (i & 0xff) == (j & 0xff)
    case _ => false
  })

  override def toString: String = "#" + i
}


case class Match8BitImmediate(i: Int) extends AssemblyLinePattern {

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean = line.registers match {
    case TwoRegisters(_, ZRegister.IMM_8) => ctx.addObject(i, line.parameter)
    case OneRegister(ZRegister.IMM_8) => ctx.addObject(i, line.parameter)
    case _ => false
  }
}

case class HasImmediateWhere(predicate: Int => Boolean) extends TrivialAssemblyLinePattern {
  override def apply(line: ZLine): Boolean =
    (line.registers match {
      case TwoRegisters(_, ZRegister.IMM_8) => true
      case OneRegister(ZRegister.IMM_8) => true
      case _ => false
    }) && (line.parameter.quickSimplify match {
      case NumericConstant(j, _) => predicate(j.toInt & 0xff)
      case _ => false
    })
}

case class Before(pattern: AssemblyPattern) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    pattern.validate(needsFlowInfo)
  }

  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, ZLine)]): Option[List[(FlowInfo, ZLine)]] = code match {
    case Nil => None
    case x :: xs => pattern.matchTo(ctx, xs) match {
      case Some(m) => Some(xs)
      case None => None
    }
  }

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean = ???
}

case class HasCallerCount(count: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: ZLine): Boolean =
    line match {
      case ZLine(ZOpcode.LABEL, _, MemoryAddressConstant(Label(l)), _) => flowInfo.labelUseCount(l) == count
      case _ => false
    }
}

case class MatchElidableCopyOf(i: Int, firstLinePattern: AssemblyLinePattern, lastLinePattern: AssemblyLinePattern) extends AssemblyPattern {
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, ZLine)]): Option[List[(FlowInfo, ZLine)]] = {
    val pattern = ctx.get[List[ZLine]](i)
    if (code.length < pattern.length) return None
    val (before, after) = code.splitAt(pattern.length)
    val lastIndex = code.length - 1
    for (((a, (f, b)), ix) <- pattern.zip(before).zipWithIndex) {
      if (!b.elidable) return None
      if (a.opcode != b.opcode) return None
      if (a.registers != b.registers) return None
      if (a.parameter.quickSimplify != b.parameter.quickSimplify) return None
      if (ix == 0 && !firstLinePattern.matchLineTo(ctx, f, b)) return None
      if (ix == lastIndex && !lastLinePattern.matchLineTo(ctx, f, b)) return None
    }
    Some(after)
  }
}