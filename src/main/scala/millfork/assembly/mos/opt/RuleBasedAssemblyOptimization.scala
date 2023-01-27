package millfork.assembly.mos.opt

import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly._
import millfork.assembly.mos.{AddrMode, _}
import millfork.assembly.opt.SingleStatus
import millfork.compiler.LabelGenerator
import millfork.env._
import millfork.error.{FatalErrorReporting, Logger}
import millfork.node.{MosNiceFunctionProperty, NiceFunctionProperty}

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

class RuleBasedAssemblyOptimization(val name: String, val needsFlowInfo: FlowInfoRequirement.Value, val rules: AssemblyRuleSet*) extends AssemblyOptimization[AssemblyLine] {

  private val actualRules = rules.flatMap(_.flatten)
  actualRules.foreach(_.pattern.validate(needsFlowInfo))
  private val actualRulesWithIndex = actualRules.zipWithIndex

  override val minimumRequiredLines: Int = rules.map(_.minimumRequiredLines).min

  override def toString: String = name

  override def optimize(f: NormalFunction, code: List[AssemblyLine], optimizationContext: OptimizationContext): List[AssemblyLine] = {
    val taggedCode = FlowAnalyzer.analyze(f, code, optimizationContext, needsFlowInfo)
    optimizeImpl(f, code, taggedCode, optimizationContext)
  }

  final def optimizeImpl(f: NormalFunction, code: List[AssemblyLine], taggedCode: List[(FlowInfo, AssemblyLine)], optimizationContext: OptimizationContext): List[AssemblyLine] = {
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
            optimizationContext.zreg,
            optimizationContext.identityPage,
            optimizationContext.niceFunctionProperties,
            head._1.labelUseCount(_)
          )
          rule.pattern.matchTo(ctx, taggedCode) match {
            case Some(rest: List[(FlowInfo, AssemblyLine)]) =>
              val optimizedChunkLengthBefore = taggedCode.length - rest.length
              val (matchedChunkToOptimize, restOfCode) = code.splitAt(optimizedChunkLengthBefore)
              val optimizedChunk: List[AssemblyLine] = rule.result(matchedChunkToOptimize, ctx)
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
                              val zeropageRegister: Option[ThingInMemory],
                              val niceFunctionProperties: Set[(NiceFunctionProperty, String)],
                              val labelUseCount: String => Int) {
  @inline
  def log: Logger = compilationOptions.log
  @inline
  def nextLabel: LabelGenerator = compilationOptions.nextLabel

  def functionChangesA(name: String): Boolean = !niceFunctionProperties(MosNiceFunctionProperty.DoesntChangeA -> name)

  def functionChangesX(name: String): Boolean = !niceFunctionProperties(MosNiceFunctionProperty.DoesntChangeX -> name)

  def functionChangesY(name: String): Boolean = !niceFunctionProperties(MosNiceFunctionProperty.DoesntChangeY -> name)

  def functionChangesIZ(name: String): Boolean = !niceFunctionProperties(MosNiceFunctionProperty.DoesntChangeIZ -> name)

  def functionChangesAH(name: String): Boolean = !niceFunctionProperties(MosNiceFunctionProperty.DoesntChangeAH -> name)

  def functionChangesC(name: String): Boolean = !niceFunctionProperties(MosNiceFunctionProperty.DoesntChangeC -> name)

  def functionReadsD(name: String): Boolean = !niceFunctionProperties(MosNiceFunctionProperty.DoesntConcernD -> name)

  def functionReadsC(name: String): Boolean = ReverseFlowAnalyzer.functionsThatReadC(name)

  def functionChangesMemory(name: String): Boolean = !niceFunctionProperties(NiceFunctionProperty.DoesntWriteMemory -> name)

  def functionReadsMemory(name: String): Boolean = !niceFunctionProperties(NiceFunctionProperty.DoesntReadMemory -> name)

  private var m_map: mutable.HashMap[Int, Any] = _

  @inline
  private def map = {
    var m = m_map
    if (m eq null) {
      m = new mutable.HashMap[Int, Any]()
      m_map = m
    }
    m
  }

  override def toString: String = if (map.isEmpty) "<empty context>" else map.mkString(", ")

  def addObject(i: Int, o: Any): Boolean = {
    if (map.contains(i)) {
      map(i) == o
    } else {
      map(i) = o
      true
    }
  }

  def addAddrModeLoosely(i: Int, o: AddrMode.Value): Boolean = {
    if (map.contains(i)) {
      val a = map(i).asInstanceOf[AddrMode.Value]
      a == o || a == AddrMode.ZeroPage && o == AddrMode.Absolute || a == AddrMode.Absolute && o == AddrMode.ZeroPage
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
    get[List[AssemblyLine]](i).foreach {
      // JSR and BSR are allowed
      case AssemblyLine0(Opcode.RTS | Opcode.RTI | Opcode.RTL | Opcode.BRK, _, _) =>
        return false
      case AssemblyLine0(Opcode.JMP, AddrMode.Indirect | AddrMode.AbsoluteIndexedX | AddrMode.LongIndirect, _) =>
        return false
      case AssemblyLine0(Opcode.LABEL, _, MemoryAddressConstant(Label(l))) =>
        labels += l
      case AssemblyLine0(Opcode.JMP, AddrMode.Absolute, MemoryAddressConstant(Label(l))) =>
        jumps += l
      case AssemblyLine0(Opcode.JMP, AddrMode.Absolute | AddrMode.LongAbsolute, _) =>
        return false
      case AssemblyLine0(_, AddrMode.Relative, MemoryAddressConstant(Label(l))) =>
        jumps += l
      case AssemblyLine0(_, AddrMode.ZeroPageWithRelative, StructureConstant(_, List(_, MemoryAddressConstant(Label(l))))) =>
        jumps += l
      case AssemblyLine0(br, _, _) if OpcodeClasses.ShortBranching(br) || OpcodeClasses.SingleBitBranch(br) =>
        return false
      case _ => ()
    }
    // if a jump leads inside the block, then it's internal
    // if a jump leads outside the block, then it's external
    jumps == labels && labels.forall(l => labelUseCount(l) <= 1)
  }

  def zreg(i: Int): Constant = {
    MemoryAddressConstant(zeropageRegister.get) + i
  }

}

case class AssemblyRule(pattern: AssemblyPattern, result: (List[AssemblyLine], AssemblyMatchingContext) => List[AssemblyLine]) extends AssemblyRuleSet {
  override def flatten: Seq[AssemblyRule] = List(this)

  override def minimumRequiredLines: Int = pattern.minimumRequiredLines
}

case class MultipleAssemblyRules(list: Seq[AssemblyRuleSet]) extends AssemblyRuleSet {
  override def flatten: Seq[AssemblyRule] = list.flatMap(_.flatten)

  override val minimumRequiredLines: Int = flatten.map(_.minimumRequiredLines).min
}

trait AssemblyPattern {

  def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = ()

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]]

  def ~(x: AssemblyPattern) = Concatenation(this, x)

  def ~(x: AssemblyLinePattern) = Concatenation(this, x)

  def ~~>(result: (List[AssemblyLine], AssemblyMatchingContext) => List[AssemblyLine]) = AssemblyRule(this, result)

  def ~~>(result: List[AssemblyLine] => List[AssemblyLine]) = AssemblyRule(this, (code, _) => result(code))

  def capture(i: Int) = Capture(i, this)

  def captureLength(i: Int) = CaptureLength(i, this)

  def minimumRequiredLines: Int

}
object HelperCheckers {
  import AddrMode._
  private val badAddrModes = Set(IndexedX, IndexedY, IndexedZ, LongIndexedY, LongIndexedZ, IndexedSY, Indirect, TripleAbsolute, Stack)
  private val goodAddrModes = Set(Implied, Immediate, WordImmediate, Relative, LongRelative)

  def memoryAccessDoesntOverlap(l1: AssemblyLine, l2: AssemblyLine, assumeSameIndices: Boolean = false): Boolean = {
    val a1 = l1.addrMode
    val a2 = l2.addrMode
    if (goodAddrModes(a1) || goodAddrModes(a2)) return true
    if (badAddrModes(a1) || badAddrModes(a2)) return false
    if (l1.opcode == Opcode.CHANGED_MEM || l2.opcode == Opcode.CHANGED_MEM) return false
    if ((a1 == IndexedSY) != (a2 == IndexedSY)) return true // bold assertion, but usually true
    val p1 = if(a1 == ZeroPageWithRelative) l1.parameter.asInstanceOf[StructureConstant].fields.head else l1.parameter
    val p2 = if(a2 == ZeroPageWithRelative) l2.parameter.asInstanceOf[StructureConstant].fields.head else l2.parameter
    val w1 = OpcodeClasses.AccessesWordInMemory(l1.opcode)
    val w2 = OpcodeClasses.AccessesWordInMemory(l2.opcode)

    def distinctThings(a: String, b: String): Boolean = {
      if (a == "__reg") return b != "__reg"
      if (b == "__reg") return a != "__reg"
      if (a == "__sp") return b != "__sp"
      if (b == "__sp") return a != "__sp"
      a.takeWhile(_ != '.') != b.takeWhile(_ != '.')
    }

    def handleKnownDistance(distance: Short): Boolean = {
      // `distance` is the distance between the first byte that can be addressed by l1 (b1) and the first byte that can be addressed by l2 (b2): (b2-b1)
      val indexingAddrModes = Set(AbsoluteIndexedX, AbsoluteX, ZeroPageX, AbsoluteY, ZeroPageY, LongAbsoluteX)
      val indicesCancelOut = assumeSameIndices && a1 == a2
      val a1Indexing = indexingAddrModes(a1) && !indicesCancelOut
      val a2Indexing = indexingAddrModes(a2) && !indicesCancelOut
      (a1Indexing, a2Indexing) match {
        case (false, false) => distance != 0 && (distance != 1 || !w1) && (distance != -1 || !w2)
        case (true, false) => distance > 255 || distance < 0 && (distance != 256 || !w1) && (distance != -1 || !w2)
        case (false, true) => distance > 0 || distance < -255 && (distance != 1 || !w1) && (distance != -256 || !w2)
        case (true, true) => distance > 255 || distance < -255 && (distance != 265 || !w1) && (distance != -256 || !w2)
      }
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
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] =
    for {
      rest <- pattern.matchTo(ctx, code)
    } yield {
      ctx.addObject(i, code.take(code.length - rest.length).map(_._2))
      rest
    }

  override def toString: String = s"(?<$i>$pattern)"

  override def minimumRequiredLines: Int = pattern.minimumRequiredLines
}

case class CaptureLength(i: Int, pattern: AssemblyPattern) extends AssemblyPattern {
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] =
    for {
      rest <- pattern.matchTo(ctx, code)
    } yield {
      ctx.addObject(i, code.length - rest.length)
      rest
    }

  override def toString: String = s"(?<$i>$pattern)"

  override def minimumRequiredLines: Int = pattern.minimumRequiredLines
}


case class Where(predicate: AssemblyMatchingContext => Boolean) extends AssemblyPattern {
  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = {
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

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = {
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

case class Many(rule: AssemblyLinePattern) extends AssemblyPattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    rule.validate(needsFlowInfo)
  }

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = {
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

case class ManyWhereAtLeastOne(rule: AssemblyLinePattern, atLeastOneIsThis: AssemblyLinePattern) extends AssemblyPattern {

  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    rule.validate(needsFlowInfo)
  }

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = {
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

  override def minimumRequiredLines: Int = rule.minimumRequiredLines
}

case class Opt(rule: AssemblyLinePattern) extends AssemblyPattern {

  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    rule.validate(needsFlowInfo)
  }

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = {
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

trait AssemblyLinePattern extends AssemblyPattern {
  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = code match {
    case Nil => None
    case x :: xs => if (matchLineTo(ctx, x._1, x._2)) Some(xs) else None
  }

  def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean

  def unary_! : AssemblyLinePattern = Not(this)

  def ? : AssemblyPattern = Opt(this)

  def * : AssemblyPattern = Many(this)

  def + : AssemblyPattern = this ~ Many(this)

  def |(x: AssemblyLinePattern): AssemblyLinePattern =
    if (this.hitRate >= x.hitRate) EitherPattern(this, x)
    else EitherPattern(x, this)

  def &(x: AssemblyLinePattern): AssemblyLinePattern =
    if (this.hitRate <= x.hitRate) Both(this, x)
    else Both(x, this)

  def hitRate: Double

  override def minimumRequiredLines: Int = 1
}

//noinspection ScalaUnnecessaryParentheses
trait TrivialAssemblyLinePattern extends AssemblyLinePattern with (AssemblyLine => Boolean) {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = this (line)
}

case class Match(predicate: AssemblyMatchingContext => Boolean) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = predicate(ctx)

  override def toString: String = "Match(...)"

  override def hitRate: Double = 0.5 // ?

  override def minimumRequiredLines: Int = 0
}

case class WhereNoMemoryAccessOverlapBetweenTwoLineLists(ix1: Int, ix2: Int) extends AssemblyPattern {
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = {
    val s1s = ctx.get[List[AssemblyLine]](ix1)
    val s2s = ctx.get[List[AssemblyLine]](ix2)
    if (s1s.forall(s1 => s2s.forall(s2 => HelperCheckers.memoryAccessDoesntOverlap(s1, s2)))) Some(code) else None
  }

  override def minimumRequiredLines: Int = 0
}

case class MatchA(i: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.a match {
      case SingleStatus(value) => ctx.addObject(i, value)
      case _ => false
    }

  override def hitRate: Double = 0.42
}

case class MatchStoredRegister(i: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import Opcode._
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

case class MatchX(i: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.x match {
      case SingleStatus(value) => ctx.addObject(i, value)
      case _ => false
    }

  override def hitRate: Double = 0.077
}

case class MatchY(i: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.y match {
      case SingleStatus(value) => ctx.addObject(i, value)
      case _ => false
    }

  override def hitRate: Double = 0.074
}

case class MatchZpReg(i: Int, registerIndex: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.reg(registerIndex) match {
      case SingleStatus(value) => ctx.addObject(i, value)
      case _ => false
    }

  override def hitRate: Double = 0.003
}

case class HasA(value: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.a.contains(value)

  override def hitRate: Double = 0.08
}

case class HasX(value: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.x.contains(value)

  override def hitRate: Double = 0.018
}

case class HasY(value: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.y.contains(value)

  override def hitRate: Double = 0.011
}

case class HasZ(value: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.iz.contains(value)

  override def hitRate: Double = 0.005
}

case class DoesntMatterWhatItDoesWith(states: State.Value*) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertBackward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    states.forall(state => flowInfo.importanceAfter.isUnimportant(state))

  override def toString: String = states.mkString("[¯\\_(ツ)_/¯:", ",", "]")

  override def hitRate: Double = 0.688
}

case class DoesntMatterWhatItDoesWithReg(index: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertBackward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.importanceAfter.isPseudoregisterUnimportant(index)

  override def toString: String = s"[¯\\_(ツ)_/¯: __reg+$index]"

  override def hitRate: Double = 0.45
}

case class HasSet(state: State.Value) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.hasSet(state)

  override def hitRate: Double = 0.026
}

case object XContainsHardwareStackPointer extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.eqSX

  override def hitRate: Double = 0.046
}

case object XContainsSoftwareStackPointer extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.eqSpX

  override def hitRate: Double = 0.046 // ?
}

case class HasSourceOfNZ(state: State.Value) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.src.exists(s => s.matches(state))

  override def hitRate: Double = 0.2
}

object HasAccu8 extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.hasSet(State.M)

  override def hitRate: Double = 0.5 // ?
}

object HasAccu16 extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.hasClear(State.M)

  override def hitRate: Double = 0.5 // ?
}

object HasIndex8 extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.hasSet(State.W)

  override def hitRate: Double = 0.5 // ?
}

object HasIndex16 extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.hasClear(State.W)

  override def hitRate: Double = 0.5 // ?
}

case class HasClear(state: State.Value) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.hasClear(state)

  override def hitRate: Double = 0.48
}

case object HasClearBitA0 extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.a0.contains(false)

  override def hitRate: Double = 0.30
}

case object Anything extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean = true

  override def hitRate: Double = 1
}

case class Not(inner: AssemblyLinePattern) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = inner.validate(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    !inner.matchLineTo(ctx, flowInfo, line)

  override def toString: String = "¬" + inner

  override def hitRate: Double = 1 - inner.hitRate
}

case class Both(l: AssemblyLinePattern, r: AssemblyLinePattern) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    l.validate(needsFlowInfo)
    r.validate(needsFlowInfo)
  }

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    l.matchLineTo(ctx, flowInfo, line) && r.matchLineTo(ctx, flowInfo, line)

  override def toString: String = l + " ∧ " + r

  override def &(x: AssemblyLinePattern): AssemblyLinePattern =
    if (x.hitRate < l.hitRate) Both(x, this)
    else Both(l, r & x)

  override def hitRate: Double = l.hitRate min r.hitRate
}

case class EitherPattern(l: AssemblyLinePattern, r: AssemblyLinePattern) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    l.validate(needsFlowInfo)
    r.validate(needsFlowInfo)
  }

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    l.matchLineTo(ctx, flowInfo, line) || r.matchLineTo(ctx, flowInfo, line)

  override def toString: String = s"($l ∨ $r)"

  override def |(x: AssemblyLinePattern): AssemblyLinePattern =
    if (x.hitRate > l.hitRate) EitherPattern(x, this)
    else EitherPattern(l, r | x)

  override def hitRate: Double = l.hitRate max r.hitRate
}

case object Elidable extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    line.elidable

  override def hitRate: Double = 0.937
}

case object NotFixed extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    line.notFixed

  override def hitRate: Double = 0.926
}

case object DebugMatching extends AssemblyPattern {
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = {
    println(ctx)
    code.foreach(println)
    Some(code)
  }

  override def minimumRequiredLines: Int = 0
}

case object Linear extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    if (line.opcode == Opcode.JSR) line.parameter match {
      case MemoryAddressConstant(f: FunctionInMemory) => f.hasOptimizationHints
      case _ => false
    } else OpcodeClasses.AllLinear(line.opcode)
  }

  override def hitRate: Double = 0.89
}

case object LinearOrBranch extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    if (line.opcode == Opcode.JSR) line.parameter match {
      case MemoryAddressConstant(f: FunctionInMemory) => f.hasOptimizationHints
      case _ => false
    } else OpcodeClasses.AllLinear(line.opcode) || OpcodeClasses.ShortBranching(line.opcode)

  override def hitRate: Double = 0.887
}

case object LinearOrLabel extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    if (line.opcode == Opcode.JSR) line.parameter match {
      case MemoryAddressConstant(f: FunctionInMemory) => f.hasOptimizationHints
      case _ => false
    } else line.opcode == Opcode.LABEL || OpcodeClasses.AllLinear(line.opcode)

  override def hitRate: Double = 0.899
}

case object ReadsA extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ReadsAAlways(line.opcode) || line.addrMode == AddrMode.Implied && OpcodeClasses.ReadsAIfImplied(line.opcode)

  override def hitRate: Double = 0.58
}

case object ReadsAH extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ReadsAHAlways(line.opcode) || line.addrMode == AddrMode.Implied && OpcodeClasses.ReadsAHIfImplied(line.opcode)

  override def hitRate: Double = 0.5 // ?
}

case object ReadsMemory extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    line.addrMode match {
      case AddrMode.Indirect => true
      case AddrMode.TripleAbsolute => true
      case AddrMode.Implied | AddrMode.Immediate => false
      case _ =>
        OpcodeClasses.ReadsMemoryIfNotImpliedOrImmediate(line.opcode)
    }

  override def hitRate: Double = 0.33
}

case object IsNonvolatile extends TrivialAssemblyLinePattern {
  // TODO: this is a big hack
  override def apply(line: AssemblyLine): Boolean =
    line.addrMode match {
      case AddrMode.Implied | AddrMode.Immediate => true
      case _ => line.parameter match {
        case MemoryAddressConstant(_) => true
        case CompoundConstant(_, MemoryAddressConstant(_), _) => true
        case _ => false
      }
    }

  override def hitRate: Double = 0.858
}

case object ReadsX extends TrivialAssemblyLinePattern {
  val XAddrModes: Set[AddrMode.Value] = Set(AddrMode.AbsoluteX, AddrMode.IndexedX, AddrMode.ZeroPageX, AddrMode.AbsoluteIndexedX, AddrMode.ImmediateWithAbsoluteX, AddrMode.ImmediateWithZeroPageX)

  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ReadsXAlways(line.opcode) || XAddrModes(line.addrMode)

  override def hitRate: Double = 0.025
}

case object ReadsY extends TrivialAssemblyLinePattern {
  val YAddrModes: Set[AddrMode.Value] = Set(AddrMode.AbsoluteY, AddrMode.IndexedY, AddrMode.ZeroPageY)

  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ReadsYAlways(line.opcode) || YAddrModes(line.addrMode)

  override def hitRate: Double = 0.0249
}

case object ConcernsC extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ReadsC(line.opcode) || OpcodeClasses.ChangesC(line.opcode)

  override def hitRate: Double = 0.378
}

case object ConcernsA extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ConcernsAAlways(line.opcode) || line.addrMode == AddrMode.Implied && OpcodeClasses.ConcernsAIfImplied(line.opcode)

  override def hitRate: Double = 0.75
}

case object ConcernsAH extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ConcernsAHAlways(line.opcode) || line.addrMode == AddrMode.Implied && OpcodeClasses.ConcernsAHIfImplied(line.opcode)

  override def hitRate: Double = 0.5 // ?
}

case object ConcernsX extends TrivialAssemblyLinePattern {
  val XAddrModes: Set[AddrMode.Value] = Set(AddrMode.AbsoluteX, AddrMode.AbsoluteIndexedX, AddrMode.LongAbsoluteX, AddrMode.IndexedX, AddrMode.ZeroPageX)

  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ConcernsXAlways(line.opcode) || XAddrModes(line.addrMode)

  override def hitRate: Double = 0.072
}

case object ConcernsS extends TrivialAssemblyLinePattern {
  val SAddrModes: Set[AddrMode.Value] = Set(AddrMode.Stack, AddrMode.IndexedSY)

  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ConcernsSAlways(line.opcode) || SAddrModes(line.addrMode)

  override def hitRate: Double = 0.15
}

case object ConcernsY extends TrivialAssemblyLinePattern {
  val YAddrModes: Set[AddrMode.Value] = Set(AddrMode.AbsoluteY, AddrMode.IndexedSY, AddrMode.IndexedY, AddrMode.LongIndexedY, AddrMode.ZeroPageY)

  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ConcernsYAlways(line.opcode) || YAddrModes(line.addrMode)

  override def hitRate: Double = 0.077
}

case object ConcernsStack extends TrivialAssemblyLinePattern {
  val SAddrModes: Set[AddrMode.Value] = Set(AddrMode.IndexedSY, AddrMode.Stack)

  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ConcernsStackAlways(line.opcode) || SAddrModes(line.addrMode)

  override def hitRate: Double = 0.218
}

case object ConcernsIZ extends TrivialAssemblyLinePattern {
  val IZAddrModes: Set[AddrMode.Value] = Set(AddrMode.IndexedZ, AddrMode.LongIndexedZ)

  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ConcernsIZAlways(line.opcode) || IZAddrModes(line.addrMode)

  override def hitRate: Double = 0.1 // ?
}

case object ChangesA extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import Opcode._
    import AddrMode._
    line match {
      case AssemblyLine0(JSR, Absolute | LongAbsolute, MemoryAddressConstant(th)) => ctx.functionChangesA(th.name)
      case AssemblyLine0(_, Implied, _) => OpcodeClasses.ChangesAIfImplied(line.opcode) || OpcodeClasses.ChangesAAlways(line.opcode)
      case _ => OpcodeClasses.ChangesAAlways(line.opcode)
    }
  }

  override def hitRate: Double = 0.345
}

case object ChangesX extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import Opcode._
    import AddrMode._
    line match {
      case AssemblyLine0(JSR, Absolute | LongAbsolute, MemoryAddressConstant(th)) => ctx.functionChangesX(th.name)
      case _ => OpcodeClasses.ChangesX(line.opcode)
    }
  }

  override def hitRate: Double = 0.072
}

case object ChangesY extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import Opcode._
    import AddrMode._
    line match {
      case AssemblyLine0(JSR, Absolute | LongAbsolute, MemoryAddressConstant(th)) => ctx.functionChangesY(th.name)
      case _ => OpcodeClasses.ChangesY(line.opcode)
    }
  }

  override def hitRate: Double = 0.094
}

case object ReadsNOrZ extends HasOpcodeIn(OpcodeClasses.ReadsNOrZ)

case object ReadsC extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import Opcode._
    import AddrMode._
    line match {
      case AssemblyLine0(JSR | BSR, Absolute | LongAbsolute, MemoryAddressConstant(th)) => ctx.functionReadsC(th.name)
      case _ => OpcodeClasses.ReadsC(line.opcode)
    }
  }

  override def hitRate: Double = 0.19
}

case object ReadsD extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import Opcode._
    import AddrMode._
    line match {
      case AssemblyLine0(JSR | BSR, Absolute | LongAbsolute, MemoryAddressConstant(th)) => ctx.functionReadsD(th.name)
      case _ => OpcodeClasses.ReadsD(line.opcode)
    }
  }

  override def hitRate: Double = 0.0277
}

case object ReadsV extends HasOpcodeIn(OpcodeClasses.ReadsV)

case object ChangesNAndZ extends HasOpcodeIn(OpcodeClasses.ChangesNAndZ)

case object ChangesC extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import Opcode._
    import AddrMode._
    line match {
      case AssemblyLine0(JSR | BSR, Absolute | LongAbsolute, MemoryAddressConstant(th)) => ctx.functionChangesC(th.name)
      case _ => OpcodeClasses.ChangesC(line.opcode)
    }
  }

  override def hitRate: Double = 0.0317
}

case object ChangesV extends HasOpcodeIn(OpcodeClasses.ChangesV)

case object ChangesStack extends HasOpcodeIn(OpcodeClasses.ChangesStack)

case object ChangesIZ extends HasOpcodeIn(OpcodeClasses.ChangesIZ)

case object ChangesS extends HasOpcodeIn(OpcodeClasses.ChangesS)

case object SupportsAbsolute extends HasOpcodeIn(OpcodeClasses.SupportsAbsolute)

case object SupportsAbsoluteX extends HasOpcodeIn(OpcodeClasses.SupportsAbsoluteX)

case object SupportsAbsoluteY extends HasOpcodeIn(OpcodeClasses.SupportsAbsoluteY)

case object ShortConditionalBranching extends HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)

case object ShortBranching extends HasOpcodeIn(OpcodeClasses.ShortBranching)

case object OverwritesA extends HasOpcodeIn(OpcodeClasses.OverwritesA)

case object OverwritesX extends HasOpcodeIn(OpcodeClasses.OverwritesX)

case object OverwritesY extends HasOpcodeIn(OpcodeClasses.OverwritesY)

case object NoopDiscardsFlags extends HasOpcodeIn(OpcodeClasses.NoopDiscardsFlags)

case object NoopDiscardsFlagsOrLabel extends HasOpcodeIn(OpcodeClasses.NoopDiscardsFlags + Opcode.LABEL)

case object ChangesAH extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import Opcode._
    import AddrMode._
    line match {
      case AssemblyLine0(JSR, Absolute | LongAbsolute, MemoryAddressConstant(th)) => ctx.functionChangesAH(th.name)
      case AssemblyLine0(_, Implied, _) => OpcodeClasses.ChangesAHIfImplied(line.opcode) || OpcodeClasses.ChangesAHAlways(line.opcode)
      case _ => OpcodeClasses.ChangesAHAlways(line.opcode)
    }
  }

  override def hitRate: Double = 0.1 // ?
}

case object ChangesM extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean = line match {
    case AssemblyLine0(Opcode.SEP | Opcode.REP, AddrMode.Immediate, NumericConstant(n, _)) => (n & 0x20) != 0
    case AssemblyLine0(Opcode.SEP | Opcode.REP | Opcode.PLP | Opcode.XCE, _, _) => true
    case _ => false
  }

  override def hitRate: Double = 0.02 // ?
}
case object ChangesW extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean = line match {
    case AssemblyLine0(Opcode.SEP | Opcode.REP, AddrMode.Immediate, NumericConstant(n, _)) => (n & 0x10) != 0
    case AssemblyLine0(Opcode.SEP | Opcode.REP | Opcode.PLP | Opcode.XCE, _, _) => true
    case _ => false
  }

  override def hitRate: Double = 0.02 // ?
}

case object ChangesMemory extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import AddrMode._
    import Opcode._
    line match {
      case AssemblyLine0(JSR | BSR, Absolute | LongAbsolute, MemoryAddressConstant(th)) => ctx.functionChangesMemory(th.name)
      case AssemblyLine0(op, Implied, _) => OpcodeClasses.ChangesMemoryAlways(op)
      case AssemblyLine0(op, TripleAbsolute, _) => true
      case AssemblyLine0(op, _, _) => OpcodeClasses.ChangesMemoryAlways(op) || OpcodeClasses.ChangesMemoryIfNotImplied(op)
      case _ => false
    }
  }

  override def hitRate: Double = 0.66
}

case class DoesntChangeMemoryAt(addrMode1: Int, param1: Int, opcode: Opcode.Value = Opcode.NOP) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import AddrMode._
    import Opcode._
    line match {
      case AssemblyLine0(JSR | BSR, Absolute | LongAbsolute, MemoryAddressConstant(th)) => !ctx.functionChangesMemory(th.name)
      case _ =>
        val p1 = ctx.get[Constant](param1)
        val a1 = ctx.get[AddrMode.Value](addrMode1)
        val changesSomeMemory = OpcodeClasses.ChangesMemoryAlways(line.opcode) || line.addrMode != AddrMode.Implied && OpcodeClasses.ChangesMemoryIfNotImplied(line.opcode)
        // TODO: NOP
        // this will break if the actual instruction was 16-bit
        !changesSomeMemory || HelperCheckers.memoryAccessDoesntOverlap(AssemblyLine(opcode, a1, p1), line)
    }
  }

  override def hitRate: Double = 0.966
}

case class DoesntChangeMemoryAtAssumingNonchangingIndices(addrMode1: Int, param1: Int, opcode: Opcode.Value = Opcode.NOP) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import AddrMode._
    import Opcode._
    line match {
      case AssemblyLine0(JSR | BSR, Absolute | LongAbsolute, MemoryAddressConstant(th)) => !ctx.functionChangesMemory(th.name)
      case _ =>
        val p1 = ctx.get[Constant](param1)
        val a1 = ctx.get[AddrMode.Value](addrMode1)
        val changesSomeMemory = OpcodeClasses.ChangesMemoryAlways(line.opcode) || line.addrMode != AddrMode.Implied && OpcodeClasses.ChangesMemoryIfNotImplied(line.opcode)
        // TODO: NOP
        // this will break if the actual instruction was 16-bit
        !changesSomeMemory || HelperCheckers.memoryAccessDoesntOverlap(AssemblyLine(opcode, a1, p1), line, assumeSameIndices = true)
    }
  }

  override def hitRate: Double = 0.973
}

case object ConcernsMemory extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    ReadsMemory.matchLineTo(ctx, flowInfo, line) || ChangesMemory.matchLineTo(ctx, flowInfo, line)

  override def hitRate: Double = 0.662
}

case class DoesNotConcernMemoryAt(addrMode1: Int, param1: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    {
      import AddrMode._
      import Opcode._
      line match {
        case AssemblyLine0(JSR | BSR, Absolute | LongAbsolute, MemoryAddressConstant(th)) => !ctx.functionReadsMemory(th.name) && !ctx.functionChangesMemory(th.name)
        case _ =>
          val p1 = ctx.get[Constant](param1)
          val a1 = ctx.get[AddrMode.Value](addrMode1)
          // TODO: NOP
          // this will break if the actual instruction was 16-bit
          HelperCheckers.memoryAccessDoesntOverlap(AssemblyLine(Opcode.NOP, a1, p1), line)
      }
    }
  }

  override def hitRate: Double = 0.968
}

case class HasOpcode(op: Opcode.Value) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    line.opcode == op

  override def toString: String = op.toString

  override def hitRate: Double = 0.071
}

case class RefersTo(identifier: String, offset: Int = 999) extends TrivialAssemblyLinePattern {
  def check(constant: Constant): Boolean = {
    constant match {
      case SubbyteConstant(base, 0) =>
        check(base)
      case MemoryAddressConstant(th) =>
        (offset == 999 || offset == 0) && th.name == identifier
      case CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(nn, _)) =>
        (offset == 999 || offset == nn) && th.name == identifier
      case CompoundConstant(MathOperator.Plus, NumericConstant(nn, _), MemoryAddressConstant(th)) =>
        (offset == 999 || offset == nn) && th.name == identifier
      case StructureConstant(_, list) =>
        list.exists(check)
      case _ => false
    }
  }

  override def apply(line: AssemblyLine): Boolean = {
    (line.addrMode == AddrMode.ZeroPage || line.addrMode == AddrMode.Absolute || line.addrMode == AddrMode.LongAbsolute) && check(line.parameter)
  }

  override def toString: String = s"<$identifier+$offset>"

  override def hitRate: Double = 0.013
}

case class RefersToOrUses(identifier: String, offset: Int = 999) extends TrivialAssemblyLinePattern {

  def check(parameter: Constant, addrMode: AddrMode.Value): Boolean = {
    addrMode match {
      case AddrMode.ZeroPage | AddrMode.Absolute | AddrMode.LongAbsolute | AddrMode.Indirect => parameter match {
        case SubbyteConstant(base, 0) =>
          check(base, addrMode)
        case MemoryAddressConstant(th) =>
          (offset == 999 || offset == 0) && th.name == identifier
        case CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(nn, _)) =>
          (offset == 999 || offset == nn) && th.name == identifier
        case CompoundConstant(MathOperator.Plus, NumericConstant(nn, _), MemoryAddressConstant(th)) =>
          (offset == 999 || offset == nn) && th.name == identifier
        case CompoundConstant(MathOperator.Minus, MemoryAddressConstant(th), NumericConstant(nn, _)) =>
          (offset == 999 || offset == -nn) && th.name == identifier
        case _ => false
      }
      case AddrMode.IndexedY | AddrMode.LongIndexedY | AddrMode.IndexedZ | AddrMode.LongIndexedZ => parameter match {
        case SubbyteConstant(base, 0) =>
          check(base, addrMode)
        case MemoryAddressConstant(th) =>
          (offset == 999 || offset == 0 || offset == 1) && th.name == identifier
        case CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(nn, _)) =>
          (offset == 999 || offset == nn || offset == nn + 1) && th.name == identifier
        case CompoundConstant(MathOperator.Plus, NumericConstant(nn, _), MemoryAddressConstant(th)) =>
          (offset == 999 || offset == nn || offset == nn + 1) && th.name == identifier
        case CompoundConstant(MathOperator.Minus, MemoryAddressConstant(th), NumericConstant(nn, _)) =>
          (offset == 999 || offset == -nn || offset == -nn + 1) && th.name == identifier
        case _ => false
      }
      case AddrMode.AbsoluteX | AddrMode.AbsoluteY | AddrMode.AbsoluteIndexedX => parameter match {
        case SubbyteConstant(base, 0) =>
          check(base, addrMode)
        case MemoryAddressConstant(th) =>
          (offset == 999 || offset >= 0 && offset <= 255) && th.name == identifier
        case CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(nn, _)) =>
          (offset == 999 || offset >= nn && offset <= nn + 255) && th.name == identifier
        case CompoundConstant(MathOperator.Plus, NumericConstant(nn, _), MemoryAddressConstant(th)) =>
          (offset == 999 || offset >= nn && offset <= nn + 255) && th.name == identifier
        case CompoundConstant(MathOperator.Minus, MemoryAddressConstant(th), NumericConstant(nn, _)) =>
          (offset == 999 || offset >= -nn && offset <= -nn + 255) && th.name == identifier
        case _ => false
      }
      case AddrMode.IndexedX => parameter match {
        case SubbyteConstant(base, 0) => check(base, addrMode)
        case MemoryAddressConstant(th) => th.name == identifier
        case CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(nn, _)) => th.name == identifier
        case CompoundConstant(MathOperator.Plus, NumericConstant(nn, _), MemoryAddressConstant(th)) => th.name == identifier
        case CompoundConstant(MathOperator.Minus, MemoryAddressConstant(th), NumericConstant(nn, _)) => th.name == identifier
        case _ => false
      }
      case AddrMode.ZeroPageWithRelative => parameter match {
        case StructureConstant(_, params) => params.exists(x => check(x, AddrMode.ZeroPage))
        case _ => false
      }
      case AddrMode.TripleAbsolute => parameter match {
        case StructureConstant(_, params) => params.exists(x => check(x, AddrMode.Absolute))
        case _ => false // TODO: ???
      }
      case AddrMode.ImmediateWithAbsolute | AddrMode.ImmediateWithZeroPage => parameter match {
        case StructureConstant(_, params) => params.exists(x => check(x, AddrMode.Absolute))
        case _ => false // TODO: ???
      }
      case AddrMode.ImmediateWithAbsoluteX | AddrMode.ImmediateWithZeroPageX => parameter match {
        case StructureConstant(_, params) => params.exists(x => check(x, AddrMode.AbsoluteX))
        case _ => false // TODO: ???
      }
      case _ => false
    }
  }

  override def apply(line: AssemblyLine): Boolean = check(line.parameter, line.addrMode)

  override def toString: String = s"<$identifier+$offset>"

  override def hitRate: Double = 0.014
}

case class CallsAnyOf(identifiers: Set[String]) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean = {
    (line.addrMode == AddrMode.Absolute ||
      line.addrMode == AddrMode.LongAbsolute ||
      line.addrMode == AddrMode.LongRelative) && (line.parameter match {
      case MemoryAddressConstant(th) => identifiers(th.name)
      case _ => false
    })
  }

  override def toString: String = identifiers.mkString("(JSR {", ",", "})")

  override def hitRate: Double = 0.03 // ?
}

case class CallsAnyExcept(identifiers: Set[String]) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean = {
    (line.addrMode == AddrMode.Absolute ||
      line.addrMode == AddrMode.LongAbsolute ||
      line.addrMode == AddrMode.LongRelative) && (line.parameter match {
      case MemoryAddressConstant(th) => th.name.head != '.' && !identifiers(th.name)
      case _ => false
    })
  }

  override def toString: String = identifiers.mkString("(JSR ¬{", ",", "})")

  override def hitRate: Double = 0.056
}

class HasOpcodeIn(val ops: Set[Opcode.Value]) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    ops(line.opcode)

  override def toString: String = ops.mkString("{", ",", "}")

  def |(that: HasOpcodeIn): HasOpcodeIn = new HasOpcodeIn(ops ++ that.ops)

  def --(that: HasOpcodeIn): HasOpcodeIn = new HasOpcodeIn(ops -- that.ops)

  override def hitRate: Double = 0.1312
}

object HasOpcodeIn {
  def apply(ops: Opcode.Value*): HasOpcodeIn = new HasOpcodeIn(ops.toSet)
  def apply(ops: Set[Opcode.Value]): HasOpcodeIn = new HasOpcodeIn(ops)
}

case class HasAddrMode(am: AddrMode.Value) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    line.addrMode == am

  override def toString: String = am.toString

  override def hitRate: Double = 0.295
}

case class HasAddrModeIn(ams: Set[AddrMode.Value]) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    ams(line.addrMode)

  override def toString: String = ams.mkString("{", ",", "}")

  def |(that: HasAddrModeIn): HasAddrModeIn = HasAddrModeIn(ams ++ that.ams)

  def --(that: HasAddrModeIn): HasAddrModeIn = HasAddrModeIn(ams -- that.ams)

  override def hitRate: Double = 0.574
}
object HasAddrModeIn {
  def apply(ams: AddrMode.Value*): HasAddrModeIn = HasAddrModeIn(ams.toSet)
}

case class HasImmediate(i: Int) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    line.addrMode == AddrMode.Immediate && (line.parameter.quickSimplify match {
      case NumericConstant(j, _) => (i & 0xff) == (j & 0xff)
      case _ => false
    })

  override def toString: String = "#" + i

  override def hitRate: Double = 0.039
}

case class HasImmediateWhere(predicate: Int => Boolean) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    line.addrMode == AddrMode.Immediate && (line.parameter.quickSimplify match {
      case NumericConstant(j, _) => predicate(j.toInt & 0xff)
      case _ => false
    })

  override def hitRate: Double = 0.556
}

case class HasParameterWhere(predicate: Constant => Boolean) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean = predicate(line.parameter)

  override def hitRate: Double = 0.332
}

case class MatchObject(i: Int, f: Function[AssemblyLine, Any]) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    ctx.addObject(i, f(line))

  override def toString: String = s"(?<$i>...)"

  override def hitRate: Double = 0.8 // ?
}

case class MatchParameter(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    ctx.addObject(i, line.parameter.quickSimplify)

  override def toString: String = s"(?<$i>Param)"

  override def hitRate: Double = 0.947
}

case class DontMatchParameter(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    ctx.dontMatch(i, line.parameter.quickSimplify)

  override def toString: String = s"¬(?<$i>Param)"

  override def hitRate: Double = 0.7 // ?
}

case class MatchAddrMode(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    ctx.addAddrModeLoosely(i, line.addrMode)

  override def toString: String = s"¬(?<$i>AddrMode)"

  override def hitRate: Double = 0.964
}

case class MatchOpcode(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    ctx.addObject(i, line.opcode)

  override def toString: String = s"¬(?<$i>Op)"

  override def hitRate: Double = 0.953
}

case class MatchImmediate(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    if (line.addrMode == AddrMode.Immediate) {
      ctx.addObject(i, line.parameter.quickSimplify)
    } else false

  override def toString: String = s"(?<$i>#)"

  override def hitRate: Double = 0.664
}

case class MatchNumericImmediate(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    if (line.addrMode == AddrMode.Immediate) {
      line.parameter.quickSimplify match {
        case NumericConstant(value, _) => ctx.addObject(i, value.toInt & 0xff)
        case _ => false
      }
    } else false

  override def toString: String = s"(?<$i>#)"

  override def hitRate: Double = 0.65
}


case class DoesntChangeIndexingInAddrMode(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    ctx.get[AddrMode.Value](i) match {
      case AddrMode.ZeroPageX | AddrMode.AbsoluteX | AddrMode.LongAbsoluteX | AddrMode.IndexedX | AddrMode.AbsoluteIndexedX => !ChangesX.matchLineTo(ctx, flowInfo, line)
      case AddrMode.ZeroPageY | AddrMode.AbsoluteY | AddrMode.IndexedY | AddrMode.LongIndexedY => !ChangesY.matchLineTo(ctx, flowInfo, line)
      case AddrMode.IndexedZ | AddrMode.LongIndexedZ => !ChangesIZ.matchLineTo(ctx, flowInfo, line)
      case AddrMode.Stack => !OpcodeClasses.ChangesS.contains(line.opcode)
      case AddrMode.IndexedSY => !OpcodeClasses.ChangesS.contains(line.opcode) && !ChangesY.matchLineTo(ctx, flowInfo, line)
      case AddrMode.Indirect | AddrMode.LongIndirect => ChangesMemory.matchLineTo(ctx, flowInfo, line)
      case _ => true
    }

  override def toString: String = s"¬(?<$i>AddrMode)"

  override def hitRate: Double = 0.99
}

case class Before(pattern: AssemblyPattern) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    pattern.validate(needsFlowInfo)
  }

  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = code match {
    case Nil => None
    case x :: xs => pattern.matchTo(ctx, xs) match {
      case Some(m) => Some(xs)
      case None => None
    }
  }

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = ???

  override def hitRate: Double = 0.5 // ?
}

case class HasCallerCount(count: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    line match {
      case AssemblyLine0(Opcode.LABEL, _, MemoryAddressConstant(Label(l))) => flowInfo.labelUseCount(l) == count
      case _ => false
    }

  override def hitRate: Double = 0.31
}

object ParameterIsLocalLabel extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    line match {
      case AssemblyLine0(Opcode.LABEL, _, MemoryAddressConstant(Label(l))) => l.startsWith(".")
      case _ => false
    }

  override def hitRate: Double = 0.056
}

case class MatchElidableCopyOf(i: Int, firstLinePattern: AssemblyLinePattern, lastLinePattern: AssemblyLinePattern) extends AssemblyPattern {
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = {
    val pattern = ctx.get[List[AssemblyLine]](i)
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

case object IsZeroPage extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import Opcode._
    line match {
      case AssemblyLine0(_, AddrMode.ZeroPage, _) => true
      case l@AssemblyLine(LDA | STA | CMP |
                          LDX | STX | CPX |
                          LDY | STY | CPY |
                          LDZ | STZ | CPZ |
                          BIT |
                          ADC | SBC | AND | ORA | EOR |
                          INC | DEC | ROL | ROR | ASL | LSR |
                          ISC | DCP | LAX | SAX | RLA | RRA | SLO | SRE, AddrMode.Absolute, p, Elidability.Elidable, _) =>
        p match {
          case NumericConstant(n, _) => n <= 255
          case MemoryAddressConstant(th) => ctx.labelMap.getOrElse(th.name, 0 -> 0x800)._2 < 0x100
          case CompoundConstant(MathOperator.Plus,
          MemoryAddressConstant(th),
          NumericConstant(n, _)) => ctx.labelMap.getOrElse(th.name, 0 -> 0x800)._2 + n < 0x100
          case _ => false
        }
      case _ => false
    }
  }

  override def hitRate: Double = 0.24
}

case object IsNotALabelUsedManyTimes extends AssemblyLinePattern {

  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = FlowInfoRequirement.assertLabels(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = line.opcode match {
    case Opcode.LABEL => line.parameter match {
      case MemoryAddressConstant(Label(l)) => flowInfo.labelUseCount(l) <= 1
      case _ => false
    }
    case _ => true
  }

  override def hitRate: Double = 0.92 // ?
}

case object ParameterIsLabel extends AssemblyLinePattern {

  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = FlowInfoRequirement.assertLabels(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = line.parameter match {
      case MemoryAddressConstant(Label(l)) => true
      case _ => false
    }

  override def hitRate: Double = 0.09 // ?
}
