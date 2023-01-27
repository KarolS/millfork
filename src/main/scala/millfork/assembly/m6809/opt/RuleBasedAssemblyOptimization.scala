package millfork.assembly.m6809.opt

import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly._
import millfork.assembly.m6809.{Absolute, DAccumulatorIndexed, Immediate, Indexed, Inherent, InherentA, InherentB, LongRelative, MAddrMode, MLine, MLine0, MOpcode, MState, NonExistent, PostIncremented, PreDecremented, Relative, TwoRegisters}
import millfork.assembly.opt.SingleStatus
import millfork.compiler.LabelGenerator
import millfork.env._
import millfork.error.{FatalErrorReporting, Logger}
import millfork.node.{M6809NiceFunctionProperty, M6809Register, NiceFunctionProperty}

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */

object FlowInfoRequirement extends Enumeration {

  val NoRequirement, JustLabels, BothFlows, ForwardFlow, BackwardFlow = Value

  def assertForward(x: FlowInfoRequirement.Value): Unit = x match {
    case BothFlows | ForwardFlow => ()
    case NoRequirement | JustLabels | BackwardFlow => FatalErrorReporting.reportFlyingPig("Forward flow info required")
  }

  def assertBackward(x: FlowInfoRequirement.Value): Unit = x match {
    case BothFlows | BackwardFlow => ()
    case NoRequirement | JustLabels | ForwardFlow => FatalErrorReporting.reportFlyingPig("Backward flow info required")
  }

  def assertLabels(x: FlowInfoRequirement.Value): Unit = x match {
    case NoRequirement => FatalErrorReporting.reportFlyingPig("Label info required")
    case _ => ()
  }
}

trait AssemblyRuleSet{
  def flatten: Seq[AssemblyRule]

  def minimumRequiredLines: Int
}

class RuleBasedAssemblyOptimization(val name: String, val needsFlowInfo: FlowInfoRequirement.Value, val rules: AssemblyRuleSet*) extends AssemblyOptimization[MLine] {

  private val actualRules = rules.flatMap(_.flatten)
  actualRules.foreach(_.pattern.validate(needsFlowInfo))
  private val actualRulesWithIndex = actualRules.zipWithIndex

  override val minimumRequiredLines: Int = rules.map(_.minimumRequiredLines).min

  override def toString: String = name

  override def optimize(f: NormalFunction, code: List[MLine], optimizationContext: OptimizationContext): List[MLine] = {
    val taggedCode = FlowAnalyzer.analyze(f, code, optimizationContext, needsFlowInfo)
    optimizeImpl(f, code, taggedCode, optimizationContext)
  }

  final def optimizeImpl(f: NormalFunction, code: List[MLine], taggedCode: List[(FlowInfo, MLine)], optimizationContext: OptimizationContext): List[MLine] = {
    val log = optimizationContext.log
    taggedCode match {
      case Nil => code
      case head :: tail =>
        val codeLength = code.length
        for {
          (rule, index) <- actualRulesWithIndex
          if codeLength >= rule.minimumRequiredLines
        } {
          val ctx = new AssemblyMatchingContext(
            optimizationContext.options,
            optimizationContext.labelMap,
            optimizationContext.niceFunctionProperties,
            head._1.labelUseCount(_)
          )
          rule.pattern.matchTo(ctx, taggedCode) match {
            case Some(rest: List[(FlowInfo, MLine)]) =>
              val optimizedChunkLengthBefore = taggedCode.length - rest.length
              val (matchedChunkToOptimize, restOfCode) = code.splitAt(optimizedChunkLengthBefore)
              val optimizedChunk: List[MLine] = rule.result(matchedChunkToOptimize, ctx)
              val optimizedChunkWithSource =
                if (!ctx.compilationOptions.flag(CompilationFlag.LineNumbersInAssembly)) optimizedChunk
                else if (optimizedChunk.isEmpty) optimizedChunk
                else if (matchedChunkToOptimize.size == 1)  optimizedChunk.map(_.pos(matchedChunkToOptimize.head.source))
                else if (optimizedChunk.size == 1) optimizedChunk.map(_.pos(SourceLine.merge(matchedChunkToOptimize.map(_.source))))
                else if (matchedChunkToOptimize.flatMap(_.source).toSet.size == 1) optimizedChunk.map(_.pos(SourceLine.merge(matchedChunkToOptimize.map(_.source))))
                else optimizedChunk
              if (log.debugEnabled) {
                log.debug(s"Applied $name ($index)")
              }
              if (log.traceEnabled) {
                if (needsFlowInfo != FlowInfoRequirement.NoRequirement) {
                  val before = head._1.statusBefore
                  val after = taggedCode(matchedChunkToOptimize.length - 1)._1.importanceAfter
                  log.trace(s"Before: $before")
                  log.trace(s"After:  $after")
                }
                matchedChunkToOptimize.filter(_.isPrintable).foreach(l => log.trace(l.toString))
                log.trace("     ↓")
                optimizedChunkWithSource.filter(_.isPrintable).foreach(l => log.trace(l.toString))
              }
              if (needsFlowInfo != FlowInfoRequirement.NoRequirement) {
                return optimizedChunkWithSource ++ optimizeImpl(f, restOfCode, rest, optimizationContext)
              } else {
                return optimize(f, optimizedChunkWithSource ++ restOfCode, optimizationContext)
              }
            case None => ()
          }
        }
        val optimizedTail = optimizeImpl(f, code.tail, tail, optimizationContext)
        if (optimizedTail eq code.tail) {
          code
        } else {
          code.head :: optimizedTail
        }
    }
  }
}

class AssemblyMatchingContext(val compilationOptions: CompilationOptions,
                              val labelMap: Map[String, (String, Int)],
                              val niceFunctionProperties: Set[(NiceFunctionProperty, String)],
                              val labelUseCount: String => Int) {
  @inline
  def log: Logger = compilationOptions.log
  @inline
  def nextLabel: LabelGenerator = compilationOptions.nextLabel

  def functionChangesA(name: String): Boolean = !niceFunctionProperties(M6809NiceFunctionProperty.DoesntChangeA -> name)

  def functionChangesX(name: String): Boolean = !niceFunctionProperties(M6809NiceFunctionProperty.DoesntChangeX -> name)

  def functionChangesY(name: String): Boolean = !niceFunctionProperties(M6809NiceFunctionProperty.DoesntChangeY -> name)

  def functionChangesU(name: String): Boolean = !niceFunctionProperties(M6809NiceFunctionProperty.DoesntChangeU -> name)

  def functionChangesB(name: String): Boolean = !niceFunctionProperties(M6809NiceFunctionProperty.DoesntChangeB -> name)

  def functionChangesCF(name: String): Boolean = !niceFunctionProperties(M6809NiceFunctionProperty.DoesntChangeCF -> name)
  
  def functionChangesMemory(name: String): Boolean = !niceFunctionProperties(NiceFunctionProperty.DoesntWriteMemory -> name)

  def functionReadsMemory(name: String): Boolean = !niceFunctionProperties(NiceFunctionProperty.DoesntReadMemory -> name)

  private val map = new mutable.HashMap[Int, Any]()

  override def toString: String = if (map.isEmpty) "<empty context>" else map.mkString(", ")

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
      if (t.asInstanceOf[AnyRef] eq null) {
        log.fatal(s"Value at index $i is null")
      } else {
        log.fatal(s"Value at index $i is a ${t.getClass.getSimpleName}, not a ${clazz.getSimpleName}")
      }
    }
  }

  def get[T: Manifest](i: Int): T = {
    val v = getImpl[T](i)
    if (v eq null) {
      log.fatal(s"Value at index $i is null")
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
    get[List[MLine]](i).foreach {
      // JSR and BSR are allowed
      case MLine0(MOpcode.RTS | MOpcode.RTI | MOpcode.SWI | MOpcode.SWI2 | MOpcode.SWI3, _, _) =>
        return false
      case MLine0(MOpcode.JMP, Absolute(false), MemoryAddressConstant(Label(l))) =>
        jumps += l
      case MLine0(MOpcode.JMP, a, _) =>
        return false
      case MLine0(MOpcode.LABEL, _, MemoryAddressConstant(Label(l))) =>
        labels += l
      case MLine0(b, _, MemoryAddressConstant(Label(l))) if MOpcode.Branching(b)=>
        jumps += l
      case MLine0(b, _, _) if MOpcode.Branching(b) =>
        return false
      case _ => ()
    }
    // if a jump leads inside the block, then it's internal
    // if a jump leads outside the block, then it's external
    jumps == labels && labels.forall(l => labelUseCount(l) <= 1)
  }

}

case class AssemblyRule(pattern: AssemblyPattern, result: (List[MLine], AssemblyMatchingContext) => List[MLine]) extends AssemblyRuleSet {
  override def flatten: Seq[AssemblyRule] = List(this)

  override def minimumRequiredLines: Int = pattern.minimumRequiredLines
}

case class MultipleAssemblyRules(list: Seq[AssemblyRuleSet]) extends AssemblyRuleSet {
  override def flatten: Seq[AssemblyRule] = list.flatMap(_.flatten)

  override val minimumRequiredLines: Int = flatten.map(_.minimumRequiredLines).sum
}

trait AssemblyPattern {

  def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = ()

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, MLine)]): Option[List[(FlowInfo, MLine)]]

  def ~(x: AssemblyPattern) = Concatenation(this, x)

  def ~(x: MLinePattern) = Concatenation(this, x)

  def ~~>(result: (List[MLine], AssemblyMatchingContext) => List[MLine]) = AssemblyRule(this, result)

  def ~~>(result: List[MLine] => List[MLine]) = AssemblyRule(this, (code, _) => result(code))

  def capture(i: Int) = Capture(i, this)

  def captureLength(i: Int) = CaptureLength(i, this)

  def minimumRequiredLines: Int

}
object HelperCheckers {
  private def badAddrModes(am: MAddrMode): Boolean = am match {
    case Inherent | Immediate | InherentA | InherentB | Relative | LongRelative => false
    case Absolute(b) => b
    case _ => true
  }
  private val goodAddrModes: Set[MAddrMode] = Set(Inherent, Immediate, InherentA, InherentB, Relative, LongRelative)

  def memoryAccessDoesntOverlap(l1: MLine, l2: MLine, assumeSameIndices: Boolean = false): Boolean = {
    val a1 = l1.addrMode
    val a2 = l2.addrMode
    if (goodAddrModes(a1) || goodAddrModes(a2)) return true
    if (badAddrModes(a1) || badAddrModes(a2)) return false
    if (l1.opcode == MOpcode.CHANGED_MEM || l2.opcode == MOpcode.CHANGED_MEM) return false
//    if ((a1 == IndexedSY) != (a2 == IndexedSY)) return true // bold assertion, but usually true
    val p1 = l1.parameter
    val p2 = l2.parameter
    val w1 = MOpcode.AccessesWordInMemory(l1.opcode)
    val w2 = MOpcode.AccessesWordInMemory(l2.opcode)

    def distinctThings(a: String, b: String): Boolean = {
      if (a == "__reg") return b != "__reg"
      if (b == "__reg") return a != "__reg"
      if (a == "__sp") return b != "__sp"
      if (b == "__sp") return a != "__sp"
      a.takeWhile(_ != '.') != b.takeWhile(_ != '.')
    }

    def handleKnownDistance(distance: Short): Boolean = {
      if (w1 || w2) distance >= 2 || distance <= -2
      else distance != 0
    }

    (p1.quickSimplify, p2.quickSimplify) match {
      case (NumericConstant(n1, _), NumericConstant(n2, _)) =>
        handleKnownDistance((n2 - n1).toShort)
      case (a, CompoundConstant(MathOperator.Plus, b, NumericConstant(distance, _))) if a.quickSimplify == b.quickSimplify =>
        handleKnownDistance(distance.toShort)
      case (CompoundConstant(MathOperator.Plus, a, NumericConstant(distance, _)), b) if a.quickSimplify == b.quickSimplify =>
        handleKnownDistance((-distance).toShort)
      case (a, CompoundConstant(MathOperator.Minus, b, NumericConstant(distance, _))) if a.quickSimplify == b.quickSimplify =>
        handleKnownDistance((-distance).toShort)
      case (CompoundConstant(MathOperator.Minus, a, NumericConstant(distance, _)), b) if a.quickSimplify == b.quickSimplify =>
        handleKnownDistance(distance.toShort)
      case (MemoryAddressConstant(_: ThingInMemory), NumericConstant(_, _)) =>
        true // TODO: ???
      case (NumericConstant(_, _), MemoryAddressConstant(_: ThingInMemory)) =>
        true // TODO: ???
      case (CompoundConstant(MathOperator.Plus | MathOperator.Minus, MemoryAddressConstant(a: ThingInMemory), NumericConstant(_, _)), NumericConstant(_, _)) =>
        true // TODO: ???
      case (NumericConstant(_, _), CompoundConstant(MathOperator.Plus | MathOperator.Minus, MemoryAddressConstant(a: ThingInMemory), NumericConstant(_, _))) =>
        true // TODO: ???
      case (MemoryAddressConstant(a: ThingInMemory), MemoryAddressConstant(b: ThingInMemory)) =>
        distinctThings(a.name, b.name) // TODO: ???
      case (CompoundConstant(op@(MathOperator.Plus | MathOperator.Minus), MemoryAddressConstant(a: ThingInMemory), NumericConstant(offset, _)),
      MemoryAddressConstant(b: ThingInMemory)) =>
        if (a.name == b.name) {
          if (op == MathOperator.Plus) {
            handleKnownDistance((-offset).toShort)
          } else {
            handleKnownDistance(offset.toShort)
          }
        } else {
          distinctThings(a.name, b.name) // TODO: ???
        }
      case (MemoryAddressConstant(a: ThingInMemory),
      CompoundConstant(op@(MathOperator.Plus | MathOperator.Minus), MemoryAddressConstant(b: ThingInMemory), NumericConstant(offset, _))) =>
        if (a.name == b.name) {
          if (op == MathOperator.Minus) {
            handleKnownDistance((-offset).toShort)
          } else {
            handleKnownDistance(offset.toShort)
          }
        } else {
          distinctThings(a.name, b.name) // TODO: ???
        }
      case (CompoundConstant(op1@(MathOperator.Plus | MathOperator.Minus), MemoryAddressConstant(a: ThingInMemory), NumericConstant(o1, _)),
      CompoundConstant(op2@(MathOperator.Plus | MathOperator.Minus), MemoryAddressConstant(b: ThingInMemory), NumericConstant(o2, _))) =>
        if (a.name == b.name) {
          val offset1 = if (op1==MathOperator.Plus) o1 else -o1
          val offset2 = if (op2==MathOperator.Plus) o2 else -o2
          handleKnownDistance((offset2 - offset1).toShort)
        } else {
          distinctThings(a.name, b.name) // TODO: ???
        }
      case _ =>
        false
    }
  }
}

case class Capture(i: Int, pattern: AssemblyPattern) extends AssemblyPattern {
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, MLine)]): Option[List[(FlowInfo, MLine)]] =
    for {
      rest <- pattern.matchTo(ctx, code)
    } yield {
      ctx.addObject(i, code.take(code.length - rest.length).map(_._2))
      rest
    }

  override def toString: String = s"(?<$i>$pattern)"

  override def minimumRequiredLines: Int = 0
}

case class CaptureLength(i: Int, pattern: AssemblyPattern) extends AssemblyPattern {
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, MLine)]): Option[List[(FlowInfo, MLine)]] =
    for {
      rest <- pattern.matchTo(ctx, code)
    } yield {
      ctx.addObject(i, code.length - rest.length)
      rest
    }

  override def toString: String = s"(?<$i>$pattern)"

  override def minimumRequiredLines: Int = 0
}


case class Where(predicate: AssemblyMatchingContext => Boolean) extends AssemblyPattern {
  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, MLine)]): Option[List[(FlowInfo, MLine)]] = {
    if (predicate(ctx)) Some(code) else None
  }

  override def toString: String = "Where(...)"

  override def minimumRequiredLines: Int = 0
}

case class Concatenation(l: AssemblyPattern, r: AssemblyPattern) extends AssemblyPattern {

  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    l.validate(needsFlowInfo)
    r.validate(needsFlowInfo)
  }

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, MLine)]): Option[List[(FlowInfo, MLine)]] = {
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

  override val minimumRequiredLines: Int = l.minimumRequiredLines + r.minimumRequiredLines
}

case class Many(rule: MLinePattern) extends AssemblyPattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    rule.validate(needsFlowInfo)
  }

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, MLine)]): Option[List[(FlowInfo, MLine)]] = {
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

  override def minimumRequiredLines: Int = 0
}

case class ManyWhereAtLeastOne(rule: MLinePattern, atLeastOneIsThis: MLinePattern) extends AssemblyPattern {

  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    rule.validate(needsFlowInfo)
  }

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, MLine)]): Option[List[(FlowInfo, MLine)]] = {
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

  override def minimumRequiredLines: Int = 1
}

case class Opt(rule: MLinePattern) extends AssemblyPattern {

  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    rule.validate(needsFlowInfo)
  }

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, MLine)]): Option[List[(FlowInfo, MLine)]] = {
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

  override def minimumRequiredLines: Int = 0
}

trait MLinePattern extends AssemblyPattern {
  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, MLine)]): Option[List[(FlowInfo, MLine)]] = code match {
    case Nil => None
    case x :: xs => if (matchLineTo(ctx, x._1, x._2)) Some(xs) else None
  }

  def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean

  def unary_! : MLinePattern = Not(this)

  def ? : AssemblyPattern = Opt(this)

  def * : AssemblyPattern = Many(this)

  def + : AssemblyPattern = this ~ Many(this)

  def |(x: MLinePattern): MLinePattern =
    if (this.hitRate >= x.hitRate) EitherPattern(this, x)
    else EitherPattern(x, this)

  def &(x: MLinePattern): MLinePattern =
    if (this.hitRate <= x.hitRate) Both(this, x)
    else Both(x, this)

  def hitRate: Double

  override def minimumRequiredLines: Int = 1
}

//noinspection ScalaUnnecessaryParentheses
trait TrivialMLinePattern extends MLinePattern with (MLine => Boolean) {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean = this (line)
}

case class Match(predicate: AssemblyMatchingContext => Boolean) extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean = predicate(ctx)

  override def toString: String = "Match(...)"

  override def hitRate: Double = 0.5 // ?
}

case class WhereNoMemoryAccessOverlapBetweenTwoLineLists(ix1: Int, ix2: Int) extends AssemblyPattern {
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, MLine)]): Option[List[(FlowInfo, MLine)]] = {
    val s1s = ctx.get[List[MLine]](ix1)
    val s2s = ctx.get[List[MLine]](ix2)
    if (s1s.forall(s1 => s2s.forall(s2 => HelperCheckers.memoryAccessDoesntOverlap(s1, s2)))) Some(code) else None
  }

  override def minimumRequiredLines: Int = 0
}

case class MatchA(i: Int) extends MLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    flowInfo.statusBefore.a match {
      case SingleStatus(value) => ctx.addObject(i, value)
      case _ => false
    }

  override def hitRate: Double = 0.42
}

case class MatchStoredRegister(i: Int) extends MLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean = {
    import MOpcode._
    line.opcode match {
      case STA =>
        flowInfo.statusBefore.a match {
          case SingleStatus(value) => ctx.addObject(i, value)
          case _ => false
        }
      case STX =>
        flowInfo.statusBefore.x match {
          case SingleStatus(value) => ctx.addObject(i, value)
          case _ => false
        }
      case STY =>
        flowInfo.statusBefore.y match {
          case SingleStatus(value) => ctx.addObject(i, value)
          case _ => false
        }
      case _ => false
    }
  }

  override def hitRate: Double = 0.42
}

case class MatchX(i: Int) extends MLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    flowInfo.statusBefore.x match {
      case SingleStatus(value) => ctx.addObject(i, value)
      case _ => false
    }

  override def hitRate: Double = 0.077
}

case class MatchY(i: Int) extends MLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    flowInfo.statusBefore.y match {
      case SingleStatus(value) => ctx.addObject(i, value)
      case _ => false
    }

  override def hitRate: Double = 0.074
}

case class HasA(value: Int) extends MLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    flowInfo.statusBefore.a.contains(value)

  override def hitRate: Double = 0.08
}

case class HasB(value: Int) extends MLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    flowInfo.statusBefore.b.contains(value)

  override def hitRate: Double = 0.08
}

case class HasD(value: Int) extends MLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    flowInfo.statusBefore.d.contains(value)

  override def hitRate: Double = 0.08
}

case class HasX(value: Int) extends MLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    flowInfo.statusBefore.x.contains(value)

  override def hitRate: Double = 0.018
}

case class HasY(value: Int) extends MLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    flowInfo.statusBefore.y.contains(value)

  override def hitRate: Double = 0.011
}

case class DoesntMatterWhatItDoesWith(states: MState.Value*) extends MLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertBackward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    states.forall(state => flowInfo.importanceAfter.isUnimportant(state))

  override def toString: String = states.mkString("[¯\\_(ツ)_/¯:", ",", "]")

  override def hitRate: Double = 0.688
}

case class HasSet(state: MState.Value) extends MLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    flowInfo.hasSet(state)

  override def hitRate: Double = 0.026
}

//case class HasSourceOfNZ(state: MState.Value) extends MLinePattern {
//  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
//    FlowInfoRequirement.assertForward(needsFlowInfo)
//
//  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
//    flowInfo.statusBefore.src.exists(s => s.matches(state))
//
//  override def hitRate: Double = 0.2
//}

case class HasClear(state: MState.Value) extends MLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    flowInfo.hasClear(state)

  override def hitRate: Double = 0.48
}

case object HasClearBitB0 extends MLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    flowInfo.statusBefore.b0.contains(false)

  override def hitRate: Double = 0.30
}

case object Anything extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean = true

  override def hitRate: Double = 1
}

case class Not(inner: MLinePattern) extends MLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = inner.validate(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    !inner.matchLineTo(ctx, flowInfo, line)

  override def toString: String = "¬" + inner

  override def hitRate: Double = 1 - inner.hitRate
}

case class Both(l: MLinePattern, r: MLinePattern) extends MLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    l.validate(needsFlowInfo)
    r.validate(needsFlowInfo)
  }

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    l.matchLineTo(ctx, flowInfo, line) && r.matchLineTo(ctx, flowInfo, line)

  override def toString: String = l + " ∧ " + r

  override def &(x: MLinePattern): MLinePattern =
    if (x.hitRate < l.hitRate) Both(x, this)
    else Both(l, r & x)

  override def hitRate: Double = l.hitRate min r.hitRate
}

case class EitherPattern(l: MLinePattern, r: MLinePattern) extends MLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    l.validate(needsFlowInfo)
    r.validate(needsFlowInfo)
  }

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    l.matchLineTo(ctx, flowInfo, line) || r.matchLineTo(ctx, flowInfo, line)

  override def toString: String = s"($l ∨ $r)"

  override def |(x: MLinePattern): MLinePattern =
    if (x.hitRate > l.hitRate) EitherPattern(x, this)
    else EitherPattern(l, r | x)

  override def hitRate: Double = l.hitRate max r.hitRate
}

case object Elidable extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    line.elidable

  override def hitRate: Double = 0.937
}

case object NotFixed extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    line.notFixed

  override def hitRate: Double = 0.926
}

case object DebugMatching extends AssemblyPattern {
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, MLine)]): Option[List[(FlowInfo, MLine)]] = {
    println(ctx)
    code.foreach(println)
    Some(code)
  }

  override def minimumRequiredLines: Int = 0
}

case object Linear extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    MOpcode.AllLinear(line.opcode)

  override def hitRate: Double = 0.89
}

case object LinearOrBranch extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean =
    MOpcode.AllLinear(line.opcode) || MOpcode.Branching(line.opcode)

  override def hitRate: Double = 0.887
}

case object LinearOrLabel extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean =
    line.opcode == MOpcode.LABEL ||  MOpcode.AllLinear(line.opcode)

  override def hitRate: Double = 0.899
}

case object ReadsA extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean = line.readsRegister(M6809Register.A)

  override def hitRate: Double = 0.58
}

case object ReadsB extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean = line.readsRegister(M6809Register.B)

  override def hitRate: Double = 0.5 // ?
}

case object ReadsMemory extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean = line.readsMemory()

  override def hitRate: Double = 0.33
}

case object ReadsX extends TrivialMLinePattern {

  override def apply(line: MLine): Boolean = line.readsRegister(M6809Register.X)

  override def hitRate: Double = 0.025
}

case object ReadsY extends TrivialMLinePattern {

  override def apply(line: MLine): Boolean = line.readsRegister(M6809Register.Y)

  override def hitRate: Double = 0.0249
}

case object ConcernsC extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean =
    MOpcode.ReadsC(line.opcode) || MOpcode.ChangesC(line.opcode)

  override def hitRate: Double = 0.378
}

case object ConcernsA extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean =
    line.readsRegister(M6809Register.A) || line.changesRegister(M6809Register.A)

  override def hitRate: Double = 0.5
}

case object ConcernsAH extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean =
    line.readsRegister(M6809Register.B) || line.changesRegister(M6809Register.B)

  override def hitRate: Double = 0.75 // ?
}

case object ConcernsX extends TrivialMLinePattern {

  override def apply(line: MLine): Boolean =
    line.readsRegister(M6809Register.X) || line.changesRegister(M6809Register.X)

  override def hitRate: Double = 0.072
}

case object ConcernsS extends TrivialMLinePattern {

  override def apply(line: MLine): Boolean =
    line.readsRegister(M6809Register.S) || line.changesRegister(M6809Register.S)

  override def hitRate: Double = 0.15
}

case object ConcernsY extends TrivialMLinePattern {

  override def apply(line: MLine): Boolean =
    line.readsRegister(M6809Register.Y) || line.changesRegister(M6809Register.Y)

  override def hitRate: Double = 0.077
}

case object ChangesA extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean = {
    import MOpcode._
    line match {
      case MLine0(JSR, Absolute(false), MemoryAddressConstant(th)) => ctx.functionChangesA(th.name)
      case _ => line.changesRegister(M6809Register.A)
    }
  }

  override def hitRate: Double = 0.145
}

case object ChangesB extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean = {
    import MOpcode._
    line match {
      case MLine0(JSR, Absolute(false), MemoryAddressConstant(th)) => ctx.functionChangesB(th.name)
      case _ => line.changesRegister(M6809Register.B)
    }
  }

  override def hitRate: Double = 0.345
}

case object ChangesX extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean = {
    import MOpcode._
    line match {
      case MLine0(JSR, Absolute(false), MemoryAddressConstant(th)) => ctx.functionChangesX(th.name)
      case _ => line.changesRegister(M6809Register.X)
    }
  }

  override def hitRate: Double = 0.072
}

case object ChangesY extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean = {
    import MOpcode._
    line match {
      case MLine0(JSR, Absolute(false), MemoryAddressConstant(th)) => ctx.functionChangesY(th.name)
      case _ => line.changesRegister(M6809Register.Y)
    }
  }

  override def hitRate: Double = 0.094
}


case object ReadsC extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean = {
    import MOpcode._
    line match {
//      case MLine0(JSR | BSR, Absolute | LongAbsolute, MemoryAddressConstant(th)) => ctx.functionReadsC(th.name)
      case _ => MOpcode.ReadsC(line.opcode)
    }
  }

  override def hitRate: Double = 0.19
}

case object ReadsN extends HasOpcodeIn(MOpcode.ReadsN)
case object ReadsZ extends HasOpcodeIn(MOpcode.ReadsZ)
case object ReadsV extends HasOpcodeIn(MOpcode.ReadsV)
case object ReadsH extends HasOpcodeIn(MOpcode.ReadsH)

//case object ChangesNAndZ extends HasOpcodeIn(MOpcode.ChangesNAndZ)

case object ChangesC extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean = {
    import MOpcode._
    line match {
//      case MLine0(JSR | BSR, Absolute | LongAbsolute, MemoryAddressConstant(th)) => ctx.functionChangesC(th.name)
      case MLine0(EXG, TwoRegisters(M6809Register.CC, _), _) => true
      case MLine0(EXG, TwoRegisters(_, M6809Register.CC), _) => true
      case MLine0(TFR, TwoRegisters(_, M6809Register.CC), _) => true
      case _ => MOpcode.ChangesC(line.opcode)
    }
  }

  override def hitRate: Double = 0.0317
}

case object NoopDiscards extends HasOpcodeIn(MOpcode.NoopDiscard)

case object NoopDiscardsOrLabel extends HasOpcodeIn(MOpcode.NoopDiscard + MOpcode.LABEL)

case object ChangesMemory extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean = {
    import MOpcode._
    line match {
      case MLine0(JSR, Absolute(false), MemoryAddressConstant(th)) => ctx.functionChangesMemory(th.name)
      case _ => line.changesMemory()
    }
  }

  override def hitRate: Double = 0.66
}

case class DoesntChangeMemoryAt(addrMode1: Int, param1: Int, opcode: MOpcode.Value = MOpcode.NOP) extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean = {
    import MOpcode._
    line match {
      case MLine0(JSR, Absolute(false), MemoryAddressConstant(th)) => !ctx.functionChangesMemory(th.name)
      case _ =>
        val p1 = ctx.get[Constant](param1)
        val a1 = ctx.get[MAddrMode](addrMode1)
        val changesSomeMemory = line.changesMemory()
        // TODO: NOP
        // this will break if the actual instruction was 16-bit
        !changesSomeMemory || HelperCheckers.memoryAccessDoesntOverlap(MLine(opcode, a1, p1), line)
    }
  }

  override def hitRate: Double = 0.966
}

case class DoesntChangeMemoryAtAssumingNonchangingIndices(addrMode1: Int, param1: Int, opcode: MOpcode.Value = MOpcode.NOP) extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean = {
    import MOpcode._
    line match {
      case MLine0(JSR, Absolute(false), MemoryAddressConstant(th)) => !ctx.functionChangesMemory(th.name)
      case _ =>
        val p1 = ctx.get[Constant](param1)
        val a1 = ctx.get[MAddrMode](addrMode1)
        val changesSomeMemory = line.changesMemory()
        // TODO: NOP
        // this will break if the actual instruction was 16-bit
        !changesSomeMemory || HelperCheckers.memoryAccessDoesntOverlap(MLine(opcode, a1, p1), line, assumeSameIndices = true)
    }
  }

  override def hitRate: Double = 0.973
}

case class DoesntChangeIndexingInAddrMode(i: Int) extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean = {
    import M6809Register._
    ctx.get[MAddrMode](i) match {
      case Indexed(S, false) => !ConcernsS.matchLineTo(ctx, flowInfo, line)
      case Indexed(X, false) => !ChangesX.matchLineTo(ctx, flowInfo, line)
      case Indexed(Y, false) => !ChangesY.matchLineTo(ctx, flowInfo, line)
      case Absolute(true) => !ChangesMemory.matchLineTo(ctx, flowInfo, line)
      case Absolute(false) | Inherent | InherentA | InherentB | Immediate => true
      case _ => false // let's ignore rarer addressing modes
    }
  }

  override def toString: String = s"¬(?<$i>AddrMode)"

  override def hitRate: Double = 0.99
}


case object ConcernsMemory extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    ReadsMemory.matchLineTo(ctx, flowInfo, line) || ChangesMemory.matchLineTo(ctx, flowInfo, line)

  override def hitRate: Double = 0.662
}

case class DoesNotConcernMemoryAt(addrMode1: Int, param1: Int) extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean = {
    {
      import MOpcode._
      line match {
        case MLine0(JSR, Absolute(false), MemoryAddressConstant(th)) => !ctx.functionReadsMemory(th.name) && !ctx.functionChangesMemory(th.name)
        case _ =>
          val p1 = ctx.get[Constant](param1)
          val a1 = ctx.get[MAddrMode](addrMode1)
          // TODO: NOP
          // this will break if the actual instruction was 16-bit
          HelperCheckers.memoryAccessDoesntOverlap(MLine(MOpcode.NOP, a1, p1), line)
      }
    }
  }

  override def hitRate: Double = 0.968
}

case class HasOpcode(op: MOpcode.Value) extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean =
    line.opcode == op

  override def toString: String = op.toString

  override def hitRate: Double = 0.071
}

class HasOpcodeIn(val ops: Set[MOpcode.Value]) extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean =
    ops(line.opcode)

  override def toString: String = ops.mkString("{", ",", "}")

  def |(that: HasOpcodeIn): HasOpcodeIn = new HasOpcodeIn(ops ++ that.ops)

  def --(that: HasOpcodeIn): HasOpcodeIn = new HasOpcodeIn(ops -- that.ops)

  override def hitRate: Double = 0.1312
}

object HasOpcodeIn {
  def apply(ops: MOpcode.Value*): HasOpcodeIn = new HasOpcodeIn(ops.toSet)
  def apply(ops: Set[MOpcode.Value]): HasOpcodeIn = new HasOpcodeIn(ops)
}

case class HasAddrMode(am: MAddrMode) extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean =
    line.addrMode == am

  override def toString: String = am.toString

  override def hitRate: Double = 0.295
}

case class IsTfr(s: M6809Register.Value, t: M6809Register.Value) extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean =
    line.opcode == MOpcode.TFR && line.addrMode == TwoRegisters(s, t)

  override def toString: String = s"(TFR $s,$t)"

  override def hitRate: Double = 0.006
}

case class IsTfrFrom(s: M6809Register.Value) extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean =
    line.opcode == MOpcode.TFR && (line.addrMode match {
      case TwoRegisters(s1, _) => s == s1
      case _ => false
    })

  override def toString: String = s"(TFR $s,_)"

  override def hitRate: Double = 0.006
}

case class IsTfrTo(t: M6809Register.Value) extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean =
    line.opcode == MOpcode.TFR && (line.addrMode match {
      case TwoRegisters(_, t1) => t == t1
      case _ => false
    })

  override def toString: String = s"(TFR _,$t)"

  override def hitRate: Double = 0.006
}

case class HasAddrModeIn(ams: Set[MAddrMode]) extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean =
    ams(line.addrMode)

  override def toString: String = ams.mkString("{", ",", "}")

  def |(that: HasAddrModeIn): HasAddrModeIn = HasAddrModeIn(ams ++ that.ams)

  def --(that: HasAddrModeIn): HasAddrModeIn = HasAddrModeIn(ams -- that.ams)

  override def hitRate: Double = 0.574
}
object HasAddrModeIn {
  def apply(ams: MAddrMode*): HasAddrModeIn = HasAddrModeIn(ams.toSet)
}

case class HasImmediate(i: Int) extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean =
    line.addrMode == Immediate && (line.parameter.quickSimplify match {
      case NumericConstant(j, _) => (i & 0xffff) == (j & 0xffff)
      case _ => false
    })

  override def toString: String = "#" + i

  override def hitRate: Double = 0.039
}

case class HasByteImmediate(i: Int) extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean =
    line.addrMode == Immediate && (line.parameter.quickSimplify match {
      case NumericConstant(j, _) => (i & 0xff) == (j & 0xff)
      case _ => false
    })

  override def toString: String = "#" + i

  override def hitRate: Double = 0.039
}

case class HasImmediateWhere(predicate: Int => Boolean) extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean =
    line.addrMode == Immediate && (line.parameter.quickSimplify match {
      case NumericConstant(j, _) => predicate(j.toInt & 0xffff)
      case _ => false
    })

  override def hitRate: Double = 0.556
}

case class HasParameterWhere(predicate: Constant => Boolean) extends TrivialMLinePattern {
  override def apply(line: MLine): Boolean = predicate(line.parameter)

  override def hitRate: Double = 0.332
}

case class MatchObject(i: Int, f: Function[MLine, Any]) extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    ctx.addObject(i, f(line))

  override def toString: String = s"(?<$i>...)"

  override def hitRate: Double = 0.8 // ?
}

case class MatchParameter(i: Int) extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    ctx.addObject(i, line.parameter.quickSimplify)

  override def toString: String = s"(?<$i>Param)"

  override def hitRate: Double = 0.947
}

case class DontMatchParameter(i: Int) extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    ctx.dontMatch(i, line.parameter.quickSimplify)

  override def toString: String = s"¬(?<$i>Param)"

  override def hitRate: Double = 0.7 // ?
}

case class MatchAddrMode(i: Int) extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    ctx.addObject(i, line.addrMode)

  override def toString: String = s"¬(?<$i>AddrMode)"

  override def hitRate: Double = 0.964
}

case class MatchOpcode(i: Int) extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    ctx.addObject(i, line.opcode)

  override def toString: String = s"¬(?<$i>Op)"

  override def hitRate: Double = 0.953
}

case class MatchImmediate(i: Int) extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    if (line.addrMode == Immediate) {
      ctx.addObject(i, line.parameter.quickSimplify)
    } else false

  override def toString: String = s"(?<$i>#)"

  override def hitRate: Double = 0.664
}

case class MatchNumericImmediate(i: Int) extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    if (line.addrMode == Immediate) {
      line.parameter.quickSimplify match {
        case NumericConstant(value, _) => ctx.addObject(i, value.toInt & 0xffff)
        case _ => false
      }
    } else false

  override def toString: String = s"(?<$i>#)"

  override def hitRate: Double = 0.65
}


case class Before(pattern: AssemblyPattern) extends MLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    pattern.validate(needsFlowInfo)
  }

  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, MLine)]): Option[List[(FlowInfo, MLine)]] = code match {
    case Nil => None
    case x :: xs => pattern.matchTo(ctx, xs) match {
      case Some(m) => Some(xs)
      case None => None
    }
  }

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean = ???

  override def hitRate: Double = 0.5 // ?
}

case class HasCallerCount(count: Int) extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    line match {
      case MLine0(MOpcode.LABEL, _, MemoryAddressConstant(Label(l))) => flowInfo.labelUseCount(l) == count
      case _ => false
    }

  override def hitRate: Double = 0.31
}

case class MatchElidableCopyOf(i: Int, firstLinePattern: MLinePattern, lastLinePattern: MLinePattern) extends AssemblyPattern {
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, MLine)]): Option[List[(FlowInfo, MLine)]] = {
    val pattern = ctx.get[List[MLine]](i)
    if (code.length < pattern.length) return None
    val (before, after) = code.splitAt(pattern.length)
    val lastIndex = code.length - 1
    for (((a, (f, b)), ix) <- pattern.zip(before).zipWithIndex) {
      if (!b.elidable) return None
      if (a.opcode != b.opcode) return None
      if (a.addrMode != b.addrMode) return None
      if (a.parameter.quickSimplify != b.parameter.quickSimplify) return None
      if (ix == 0 && !firstLinePattern.matchLineTo(ctx, f, b)) return None
      if (ix == lastIndex && !lastLinePattern.matchLineTo(ctx, f, b)) return None
    }
    Some(after)
  }

  override def minimumRequiredLines: Int = 0
}

case object IsNotALabelUsedManyTimes extends MLinePattern {

  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = FlowInfoRequirement.assertLabels(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean = line.opcode match {
    case MOpcode.LABEL => line.parameter match {
      case MemoryAddressConstant(Label(l)) => flowInfo.labelUseCount(l) <= 1
      case _ => false
    }
    case _ => true
  }

  override def hitRate: Double = 0.92 // ?
}


object ParameterIsLocalLabel extends MLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: MLine): Boolean =
    line match {
      case MLine0(MOpcode.LABEL, _, MemoryAddressConstant(Label(l))) => l.startsWith(".")
      case _ => false
    }

  override def hitRate: Double = 0.056
}
