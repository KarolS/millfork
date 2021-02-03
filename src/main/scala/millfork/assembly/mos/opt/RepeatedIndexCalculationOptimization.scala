package millfork.assembly.mos.opt

import millfork.assembly.{AssemblyOptimization, Elidability, OptimizationContext}
import millfork.assembly.mos.{AddrMode, AssemblyLine, AssemblyLine0, Opcode, OpcodeClasses}
import Opcode._
import AddrMode._
import millfork.env.{MemoryAddressConstant, NormalFunction, NumericConstant}
import millfork.node.{MosNiceFunctionProperty, NiceFunctionProperty}

import scala.collection.mutable.ListBuffer
import scala.util.control.TailCalls.TailRec
import scala.util.control.TailCalls.done
import scala.util.control.TailCalls.tailcall

/**
  * @author Karol Stasiak
  */
case class RepeatedIndexCalculationOptimization(forX: Boolean) extends AssemblyOptimization[AssemblyLine] {
  override def name: String = "Repeated index calculation into " + (if (forX) "X" else "Y")

  override def optimize(f: NormalFunction, code: List[AssemblyLine], context: OptimizationContext): List[AssemblyLine] = {
    val log = context.log
    val allRuns = findAllRuns(code, 0, None).result
    if (allRuns.size <= 1) {
      log.trace("No index calculations found")
      return code
    }
    if (log.traceEnabled) {
      log.trace("All index calculations found: " + allRuns)
      for ((line, ix) <- code.zipWithIndex) {
        val inRun = allRuns.indexWhere { case (from, to) => ix >= from && ix < to }
        if (inRun < 0) {
          log.trace(s"\t$line")
        } else {
          log.trace(s"$inRun\t$line")
        }
      }
    }
    lazy val reverseFlow = ReverseFlowAnalyzer.analyze(code, context.niceFunctionProperties)
    val flow = CoarseFlowAnalyzer.analyze(f, code, context)
    var replacements: Map[Int, Int] = Map()
    for (i <- 1 until allRuns.size) {
      val (r1From, r1To) = allRuns(i - 1)
      val (r2From, r2To) = allRuns(i)
      val codeBetween = code.slice(r1To, r2From)
      val deltaOpt = if (forX) findConstantDeltaX(context, codeBetween) else findConstantDeltaY(context, codeBetween)
      log.trace(s"Delta between ${i - 1} and $i is $deltaOpt")
      deltaOpt match {
        case Some(delta) if delta >= -2 && delta <= 2 =>
          val code1 = code.slice(r1From, r1To)
          val code2 = code.slice(r2From, r2To)
          val dIsUnimportant = !code1.exists(l => OpcodeClasses.ReadsD(l.opcode)) && !code2.exists(l => OpcodeClasses.ReadsD(l.opcode))
          val dIsClear = flow(r1From).d.contains(false) && flow(r2From).d.contains(false)
          val cIsUnimportantAfter = reverseFlow(r2To - 1).c == Unimportant
          val cIsUnmodified = !code2.exists(l => OpcodeClasses.ChangesC(l.opcode))
          if ((dIsUnimportant || dIsClear) && (cIsUnimportantAfter || cIsUnmodified)) {
            getExtraDelta(code1, code2) match {
              case Some(xdelta) =>
                log.trace(s"Runs are identical, extra delta is $xdelta")
                val variablesToPreserve = getAccessedVariables(code2)
                log.trace(s"variablesToPreserve=$variablesToPreserve")
                val variablesNotPreserved = getModifiedVariables(codeBetween)
                log.trace(s"variablesNotPreserved=$variablesNotPreserved")
                if (!variablesToPreserve("?") && (variablesToPreserve & variablesNotPreserved).isEmpty) {
                  replacements += (i -> (delta + xdelta))
                }
              case _ =>
            }
          }
        case _ =>
      }
    }
    if (replacements.isEmpty) return code
    val result = ListBuffer[AssemblyLine]()
    var processedSoFar = 0
    val IN_ = if (forX) INX else INY
    val DE_ = if (forX) DEX else DEY
    val T_A = if (forX) TXA else TYA
    var previousFrom = 0
    var firstFrom = Int.MaxValue
    var lastTo = -1
    for (((from, to), ix) <- allRuns.zipWithIndex) {
      result ++= code.slice(processedSoFar, from)
      replacements.get(ix) match {
        case Some(delta) =>
          val newPos = code(to - 1).source
          val replacement: Seq[AssemblyLine] = (delta, reverseFlow(to - 1).a != Unimportant) match {
            case (0, false) =>
              if (reverseFlow(to - 1).n != Unimportant || reverseFlow(to - 1).z != Unimportant) {
                List(AssemblyLine.implied(T_A).pos(newPos))
              } else Nil
            case (0, true) =>
              List(AssemblyLine.implied(T_A).pos(newPos))
            case (d, false) if d > 0 =>
              List.fill(delta)(AssemblyLine.implied(DE_).pos(newPos))
            case (d, true) if d > 0 =>
              List.fill(delta)(AssemblyLine.implied(DE_).pos(newPos)) :+ AssemblyLine.implied(T_A).pos(newPos)
            case (d, false) if d < 0 =>
              List.fill(-delta)(AssemblyLine.implied(IN_).pos(newPos))
            case (d, true) if d < 0 =>
              List.fill(-delta)(AssemblyLine.implied(IN_).pos(newPos)) :+ AssemblyLine.implied(T_A).pos(newPos)
          }
          result ++= replacement
          if (log.traceEnabled) {
            log.debug(s"Applied $name for run $ix ($from-$to) with delta $delta")
          } else {
            log.debug(s"Applied $name")
          }
          firstFrom = firstFrom min from
          lastTo = lastTo max to
          if (log.traceEnabled) {
            code.slice(previousFrom,  to).filter(_.isPrintable).foreach(l => log.trace(l.toString))
            log.trace("     ↓")
            (code.slice(previousFrom, from) ++ replacement).filter(_.isPrintable).foreach(l => log.trace(l.toString))
          }
        case _ =>
          result ++= code.slice(from, to)
      }
      processedSoFar = to
      previousFrom = from
    }
    result ++= code.drop(processedSoFar)
    result.toList
  }

  def findConstantDeltaX(ctx: OptimizationContext, code: List[AssemblyLine]): Option[Int] = {
    var delta = 0
    for (line <- code) {
      line match {
        case AssemblyLine0(JSR, Absolute | LongAbsolute, MemoryAddressConstant(th)) =>
          if (!ctx.niceFunctionProperties(MosNiceFunctionProperty.DoesntChangeX -> th.name)) return None
          if (!ctx.niceFunctionProperties(NiceFunctionProperty.DoesntWriteMemory -> th.name)) return None
        case AssemblyLine0(INX, _, _) => delta += 1
        case AssemblyLine0(DEX, _, _) => delta -= 1
        case AssemblyLine0(CHANGED_MEM | JSR, _, _) => return None
        case _ =>
          if (!OpcodeClasses.AllLinear(line.opcode)) return None
          if (OpcodeClasses.ChangesX(line.opcode)) return None
      }
    }
    Some(delta)
  }

  def findConstantDeltaY(ctx: OptimizationContext, code: List[AssemblyLine]): Option[Int] = {
    var delta = 0
    for (line <- code) {
      line match {
        case AssemblyLine0(JSR, Absolute | LongAbsolute, MemoryAddressConstant(th)) =>
          if (!ctx.niceFunctionProperties(MosNiceFunctionProperty.DoesntChangeY -> th.name)) return None
          if (!ctx.niceFunctionProperties(NiceFunctionProperty.DoesntWriteMemory -> th.name)) return None
        case AssemblyLine0(INY, _, _) => delta += 1
        case AssemblyLine0(DEY, _, _) => delta -= 1
        case AssemblyLine0(CHANGED_MEM | JSR, _, _) => return None
        case _ =>
          if (!OpcodeClasses.AllLinear(line.opcode)) return None
          if (OpcodeClasses.ChangesY(line.opcode)) return None
      }
    }
    Some(delta)
  }

  def endsWithSecRolTar(code: List[AssemblyLine]): Boolean = {
    if (code.size < 4) return false
    val penpenult = code(code.size - 3)
    val penult = code(code.size - 2)
    val last = code.last
    penpenult.opcode == SEC && penult.opcode == ROL && penult.addrMode == Implied && (last.opcode == TAX || last.opcode == TAY)
  }

  def getExtraDelta(code1: List[AssemblyLine], code2: List[AssemblyLine]): Option[Int] = {
    if (endsWithSecRolTar(code1)) {
      return getExtraDelta(code1.dropRight(3) ++ List(AssemblyLine.implied(ASL), AssemblyLine.implied(CLC), AssemblyLine.immediate(ADC, 1), code1.last) ,
        code2)
    }
    if (endsWithSecRolTar(code2)) {
      return getExtraDelta(code1,
        code2.dropRight(3) ++ List(AssemblyLine.implied(ASL), AssemblyLine.implied(CLC), AssemblyLine.immediate(ADC, 1), code2.last))
    }
    if (code1.size != code2.size) {
      if (code1.size == code2.size + 2) {
        return getExtraDelta(code1, code2.init ++ List(AssemblyLine.implied(CLC), AssemblyLine.immediate(ADC, 0), code2.last))
      }
      if (code1.size + 2 == code2 .size) {
        return getExtraDelta(code1.init ++ List(AssemblyLine.implied(CLC), AssemblyLine.immediate(ADC, 0), code1.last), code2)
      }
      return None
    }
    var delta = 0
    for (i <- code1.indices) {
      val l1 = code1(i)
      val l2 = code2(i)
      if (l1.opcode != l2.opcode) return None
      if (l1.addrMode != l2.addrMode) return None
      if (l1.parameter != l2.parameter) {
        if (i != code2.size - 2) return None
        if (l2.opcode != ADC) return None
        if (l2.addrMode != Immediate) return None
        (l1.parameter, l2.parameter) match {
          case (NumericConstant(n1, _), NumericConstant(n2, _)) =>
            delta += (n1 - n2).toInt
          case _ => return None
        }
      }
    }
    Some(delta)
  }

  def getAccessedVariables(code: List[AssemblyLine]): Set[String] = {
    code.map(_.parameter.rootThingName)
      .filter(_ != "")
      .toSet
  }

  def getModifiedVariables(code: List[AssemblyLine]): Set[String] = {
    code.filter(l => OpcodeClasses.ChangesMemoryAlways(l.opcode) || OpcodeClasses.ChangesMemoryIfNotImplied(l.opcode))
      .map(_.parameter.rootThingName)
      .filter(_ != "")
      .toSet
  }

  def findAllRuns(xs: List[AssemblyLine], offset: Int, latestStart: Option[Int]): TailRec[List[(Int, Int)]] = {
    if (xs.isEmpty) return done(Nil)
    if (xs.head.elidability != Elidability.Elidable) return tailcall(findAllRuns(xs.tail, offset + 1, None))
    val TA_ = if (forX) TAX else TAY
    xs match {
      case AssemblyLine0(LDA, Immediate | ZeroPage | Absolute | LongAbsolute, _) :: tail =>
        tailcall(findAllRuns(tail, offset + 1, Some(offset)))
      case AssemblyLine0(CLC | SEC, _, _) :: (l2@AssemblyLine0(ROL | ROR, Implied, _)) :: tail
        if l2.elidability == Elidability.Elidable =>
        tailcall(findAllRuns(tail, offset + 2, latestStart))
      case AssemblyLine0(CLC | SEC, _, _) :: (l2@AssemblyLine0(ADC | ADC, Immediate | ZeroPage | Absolute | LongAbsolute, _)) :: tail
        if l2.elidability == Elidability.Elidable =>
        tailcall(findAllRuns(tail, offset + 2, latestStart))
      case AssemblyLine0(AND | EOR | ORA, Immediate | ZeroPage | Absolute | LongAbsolute, _) :: tail =>
        tailcall(findAllRuns(tail, offset + 1, latestStart))
      case AssemblyLine0(ASL | LSR, Implied, _) :: tail =>
        tailcall(findAllRuns(tail, offset + 1, latestStart))
      case AssemblyLine0(TA_, Implied, _) :: tail =>
        latestStart match {
          case Some(l) =>
            tailcall(findAllRuns(tail, offset + 1, latestStart)).map((l, offset + 1) :: _)
          case None =>
            tailcall(findAllRuns(tail, offset + 1, latestStart))
        }
      case _ :: tail => tailcall(findAllRuns(tail, offset + 1, None))
      case Nil => done(Nil)
    }
  }
}
