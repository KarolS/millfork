package millfork.assembly.z80.opt

import millfork.{CompilationFlag, NonOverlappingIntervals}
import millfork.assembly.{AssemblyOptimization, Elidability, OptimizationContext}
import millfork.assembly.z80.{TwoRegisters, ZFlag, ZLine, ZLine0}
import millfork.env._
import millfork.error.ConsoleLogger
import millfork.node.ZRegister

import scala.collection.mutable.ListBuffer
import scala.util.control.TailCalls.{TailRec, done, tailcall}

/**
  * @author Karol Stasiak
  */
object WordVariableToRegisterOptimization extends AssemblyOptimization[ZLine] {

  override def requiredFlags: Set[CompilationFlag.Value] = Set(CompilationFlag.RegisterVariables)

  override def name = "Allocating variables to register pairs"

  object CyclesAndBytes {
    val Zero = CyclesAndBytes(0, 0)
  }

  case class CyclesAndBytes(bytes: Int, cycles: Int) {
    def +(that: CyclesAndBytes) = CyclesAndBytes(this.bytes + that.bytes, this.cycles + that.cycles)
  }

  override def optimize(f: NormalFunction, code: List[ZLine], optimizationContext: OptimizationContext): List[ZLine] = {
    val vs = VariableStatus(f, code, optimizationContext, _.size == 2).getOrElse(return code)
    val options = optimizationContext.options
    val log = options.log
    val removeVariablesForReal = !options.flag(CompilationFlag.InternalCurrentlyOptimizingForMeasurement)
    val costFunction: CyclesAndBytes => Int = if (options.flag(CompilationFlag.OptimizeForSpeed)) _.cycles else _.bytes

    val hlCandidates = vs.variablesWithLifetimes.filter {
      case (v, range) =>
        val tuple = vs.codeWithFlow(range.start)
        tuple._1.importanceAfter.h != Important &&
          tuple._1.importanceAfter.l != Important || {
//          println(s"Cannot inline ${v.name} to HL because of early $tuple")
          false
        }
    }.flatMap {
      case (v, range) =>
        canBeInlined(v.name, synced = false, ZRegister.HL, vs.codeWithFlow.slice(range.start, range.end)).map { score =>
          (v.name, range, if (vs.variablesWithRegisterHint(v.name)) score + CyclesAndBytes(16, 16) else score)
        }
    }

    val bcCandidates = vs.variablesWithLifetimes.filter {
      case (v, range) =>
        val tuple = vs.codeWithFlow(range.start)
        tuple._1.importanceAfter.b != Important &&
          tuple._1.importanceAfter.c != Important || {
//          println(s"Cannot inline ${v.name} to BC because of early $tuple")
          false
        }
    }.flatMap {
      case (v, range) =>
        canBeInlined(v.name, synced = false, ZRegister.BC, vs.codeWithFlow.slice(range.start, range.end)).map { score =>
          (v.name, range, if (vs.variablesWithRegisterHint(v.name)) score + CyclesAndBytes(16, 16) else score)
        }
    }

    val deCandidates = vs.variablesWithLifetimes.filter {
      case (v, range) =>
        val tuple = vs.codeWithFlow(range.start)
        tuple._1.importanceAfter.d != Important &&
          tuple._1.importanceAfter.e != Important || {
//          println(s"Cannot inline ${v.name} to DE because of early $tuple")
          false
        }
    }.flatMap {
      case (v, range) =>
        canBeInlined(v.name, synced = false, ZRegister.DE, vs.codeWithFlow.slice(range.start, range.end)).map { score =>
          (v.name, range, if (vs.variablesWithRegisterHint(v.name)) score + CyclesAndBytes(16, 16) else score)
        }
    }

    val hlCandidateSets = NonOverlappingIntervals.apply[(String, Range, CyclesAndBytes)](hlCandidates, _._2.start, _._2.end)
    val bcCandidateSets = NonOverlappingIntervals.apply[(String, Range, CyclesAndBytes)](bcCandidates, _._2.start, _._2.end)
    val deCandidateSets = NonOverlappingIntervals.apply[(String, Range, CyclesAndBytes)](deCandidates, _._2.start, _._2.end)

    val variants = for {
      vhl <- if (options.flag(CompilationFlag.SingleThreaded)) hlCandidateSets else hlCandidateSets.par
      nhl = vhl.map(_._1)

      vbc <- bcCandidateSets
      nbc = vbc.map(_._1)
      if (nhl & nbc).isEmpty

      vde <- deCandidateSets
      nde = vde.map(_._1)
      if (nhl & nde).isEmpty
      if (nhl & nde).isEmpty


      score = vhl.toSeq.map(x => costFunction(x._3)).sum +
        vbc.toSeq.map(x => costFunction(x._3)).sum +
        vde.toSeq.map(x => costFunction(x._3)).sum
    } yield (score, vhl, vbc, vde)

    if (variants.isEmpty) {
      return code
    }

    //    variants.foreach(println)

    val (_, bestHLs, bestBCs, bestDEs) = variants.maxBy(_._1)

    def reportOptimizedBlock[T](oldCode: List[(T, ZLine)], newCode: List[ZLine]): Unit = {
      if (log.traceEnabled) {
        oldCode.foreach(l => log.trace(l._2.toString))
        log.trace("     ↓")
        newCode.foreach(l => log.trace(l.toString))
      }
    }

    if (bestHLs.nonEmpty || bestBCs.nonEmpty || bestDEs.nonEmpty) {
      val output = ListBuffer[ZLine]()
      var i = 0
      while (i < code.length) {
        var done = false
        bestHLs.find(_._2.start == i).foreach {
          case (v, range, _) =>
            log.debug(s"Inlining $v to register pair HL")
            val oldCode = vs.codeWithFlow.slice(range.start, range.end)
            val newCode = inlineVars(v, "", "", oldCode).result
            reportOptimizedBlock(oldCode, newCode)
            output ++= newCode
            i = range.end
            if (removeVariablesForReal && contains(range, vs.variablesWithLifetimesMap(v))) {
              f.environment.removeVariable(v)
            }
            done = true
        }
        if (!done) {
          bestBCs.find(_._2.start == i).foreach {
            case (v, range, _) =>
              log.debug(s"Inlining $v to register pair BC")
              val oldCode = vs.codeWithFlow.slice(range.start, range.end)
              val newCode = inlineVars("", v, "", oldCode).result
              reportOptimizedBlock(oldCode, newCode)
              output ++= newCode
              i = range.end
              if (removeVariablesForReal && contains(range, vs.variablesWithLifetimesMap(v))) {
                f.environment.removeVariable(v)
              }
              done = true
          }
        }
        if (!done) {
          bestDEs.find(_._2.start == i).foreach {
            case (v, range, _) =>
              log.debug(s"Inlining $v to register pair DE")
              val oldCode = vs.codeWithFlow.slice(range.start, range.end)
              val newCode = inlineVars("", "", v, oldCode).result
              reportOptimizedBlock(oldCode, newCode)
              output ++= newCode
              i = range.end
              if (removeVariablesForReal && contains(range, vs.variablesWithLifetimesMap(v))) {
                f.environment.removeVariable(v)
              }
              done = true
          }
        }
        if (!done) {
          output += code(i)
          i += 1
        }
      }
      output.toList
    } else {
      code
    }
  }

  def contains(outer: Range, inner: Range): Boolean = {
    outer.contains(inner.start) && outer.contains(inner.end - 1)
  }

  import millfork.assembly.z80.ZOpcode._
  import millfork.node.ZRegister._

  def add(first: Boolean, ifTrue: CyclesAndBytes, ifFalse: CyclesAndBytes): CyclesAndBytes=>CyclesAndBytes = { c =>
    if (first) c + ifTrue else c + ifFalse
  }
  def add(value: CyclesAndBytes): CyclesAndBytes=>CyclesAndBytes = _ + value

  def canBeInlined(vname: String, synced: Boolean, target: ZRegister.Value, code: List[(FlowInfo, ZLine)]): Option[CyclesAndBytes] = {
    def fail(reason: Int): None.type = {
//      println(s"Cannot inline $vname to $target because of [[$reason]] ${code.head}")
      None
    }
    code match {
      case  (_, ZLine(LD_16, TwoRegisters(BC, IMM_16), _, Elidability.Elidable, _))::
        (i, ZLine(ADD_16, TwoRegisters(HL, BC), _, Elidability.Elidable, _)) :: xs if target == BC =>
        if (i.importanceAfter.getRegister(BC) == Important) fail(22)
        else  if(i.importanceAfter.getRegister(DE) == Important) canBeInlined(vname, synced, target, xs).map(add(CyclesAndBytes(-21, -2)))
        else canBeInlined(vname, synced = true, target, xs)

      case  (_, ZLine(LD_16, TwoRegisters(DE, IMM_16), _, Elidability.Elidable, _))::
        (i, ZLine(ADD_16, TwoRegisters(HL, DE), _, Elidability.Elidable, _)) :: xs if target == DE =>
        if (i.importanceAfter.getRegister(DE) == Important) fail(23)
        else if (i.importanceAfter.getRegister(BC) == Important) canBeInlined(vname, synced, target, xs).map(add(CyclesAndBytes(-21, -2)))
        else canBeInlined(vname, synced = true, target, xs)

      case (_, ZLine(LD_16, TwoRegisters(HL, MEM_ABS_16), MemoryAddressConstant(th1), Elidability.Elidable, _)) ::
        (_, ZLine0(ADD_16, TwoRegisters(HL, BC | DE), _)) ::
        (i, ZLine(LD_16, TwoRegisters(MEM_ABS_16, HL), MemoryAddressConstant(th2), Elidability.Elidable, _)) ::
        xs if target == HL && th1.name != vname && th2.name != vname &&
        i.importanceAfter.getFlag(ZFlag.Z) != Important &&
        i.importanceAfter.getFlag(ZFlag.H) != Important &&
        i.importanceAfter.getFlag(ZFlag.P) != Important &&
        i.importanceAfter.getRegister(A) != Important &&
        i.importanceAfter.getRegister(HL) != Important &&
        i.importanceAfter.getFlag(ZFlag.Z) != Important =>
        // bytes before: 3 + 1 + 3 = 7
        // cycles before: 16 + 11 + 16 = 43
        // bytes after: 3 + 1 + 3 + 3 + 1 + 3 = 14
        // cycles after: 13 + 4 + 13 + 13 + 4 + 13 = 60
        canBeInlined(vname, synced = true, target, xs).map(add(CyclesAndBytes(-17, -7)))

      case (_, ZLine(LD_16, TwoRegisters(t, _), _, Elidability.Elidable, _)) ::
        (_, ZLine(LD_16, TwoRegisters(HL, MEM_ABS_16), MemoryAddressConstant(th), Elidability.Elidable, _)) ::
        (i, ZLine0(ADD_16, TwoRegisters(HL, t2), _)) ::
        xs if th.name == vname && t != HL && t == t2 && i.importanceAfter.getRegister(t) == Unimportant =>
        // LD PP ; LD HL,(qq) ; ADD HL,PP → LD H,P ; LD L,P ; ADD HL,QQ
        canBeInlined(vname, synced = true, target, xs).map(add(target == t, CyclesAndBytes(16, 3), CyclesAndBytes(8, 1)))

//      case (_, ZLine(LD_16, TwoRegisters(t, _), _, true, _)) ::
//        (_, ZLine(LD_16, TwoRegisters(HL, MEM_ABS_16), MemoryAddressConstant(th), true, _)) ::
//        (i, ZLine0(ADD_16, TwoRegisters(HL, t2), _)) ::
//        xs if th.name == vname && t == target && t != HL && t == t2 && i.importanceAfter.getRegister(t) == Unimportant =>
//        canBeInlined(vname, synced = true, target, xs).map(add(CyclesAndBytes(16, 3)))

      case (_, ZLine(LD_16, TwoRegisters(MEM_ABS_16, HL), MemoryAddressConstant(th), Elidability.Elidable, _)) ::
        (_, ZLine(LD_16, TwoRegisters(t, _), _, Elidability.Elidable, _)) ::
        (i, ZLine0(ADD_16, TwoRegisters(HL, t2), _)) ::
        xs if th.name == vname && t != HL && t == t2 && i.importanceAfter.getRegister(t) == Unimportant =>
        // LD (vv),HL ; LD QQ,__ ; ADD HL,QQ  (vv@QQ)→  LD QQ,HL ; LD HL,__, ADD HL,QQ
        canBeInlined(vname, synced = true, target, xs).map(add(target == t, CyclesAndBytes(16, 3), CyclesAndBytes(8, 1)))

      case (_, ZLine(LD_16, TwoRegisters(MEM_ABS_16, HL), MemoryAddressConstant(th), Elidability.Elidable, _)) ::
        (_, ZLine(LD_16, TwoRegisters(t, _), _, Elidability.Elidable, _)) ::
        (i, ZLine0(ADD_16, TwoRegisters(HL, t2), _)) ::
        xs if t == target && th.name == vname && t != HL && t == t2 && i.importanceAfter.getRegister(t) == Unimportant =>
        // LD (vv),HL ; LD QQ,__ ; ADD HL,QQ  (vv@QQ)→  LD QQ,HL ; LD PP,__, ADD HL,PP
        canBeInlined(vname, synced = true, target, xs).map(add(CyclesAndBytes(16, 3)))

      case (_, ZLine(LD_16, TwoRegisters(HL, MEM_ABS_16), MemoryAddressConstant(th), Elidability.Elidable, _)) :: xs if th.name == vname =>
        canBeInlined(vname, synced = true, target, xs).map(add(target == HL, CyclesAndBytes(16, 3), CyclesAndBytes(8, 1)))
      case (_, ZLine(LD_16, TwoRegisters(MEM_ABS_16, HL), MemoryAddressConstant(th), Elidability.Elidable, _)) :: xs if th.name == vname =>
        canBeInlined(vname, synced = true, target, xs).map(add(target == HL, CyclesAndBytes(16, 3), CyclesAndBytes(8, 1)))
      case (_, ZLine(LD_16, TwoRegisters(DE, MEM_ABS_16), MemoryAddressConstant(th), Elidability.Elidable, _)) :: xs if th.name == vname =>
        canBeInlined(vname, synced = true, target, xs).map(add(target == DE, CyclesAndBytes(16, 3), CyclesAndBytes(8, 1)))
      case (_, ZLine(LD_16, TwoRegisters(MEM_ABS_16, DE), MemoryAddressConstant(th), Elidability.Elidable, _)) :: xs if th.name == vname =>
        canBeInlined(vname, synced = true, target, xs).map(add(target == DE, CyclesAndBytes(16, 3), CyclesAndBytes(8, 1)))
      case (_, ZLine(LD_16, TwoRegisters(BC, MEM_ABS_16), MemoryAddressConstant(th), Elidability.Elidable, _)) :: xs if th.name == vname =>
        canBeInlined(vname, synced = true, target, xs).map(add(target == BC, CyclesAndBytes(16, 3), CyclesAndBytes(8, 1)))
      case (_, ZLine(LD_16, TwoRegisters(MEM_ABS_16, BC), MemoryAddressConstant(th), Elidability.Elidable, _)) :: xs if th.name == vname =>
        canBeInlined(vname, synced = true, target, xs).map(add(target == BC, CyclesAndBytes(16, 3), CyclesAndBytes(8, 1)))

      case (_, x) :: (_, ZLine(LD_16, TwoRegisters(MEM_ABS_16, t), MemoryAddressConstant(th), Elidability.Elidable, _)) :: xs
        if th.name == vname && t == target && x.changesRegister(t) =>
        canBeInlined(vname, synced = true, target, xs).map(add(CyclesAndBytes(16, 3)))


      case (_, ZLine(LD, TwoRegisters(A, MEM_ABS_8), MemoryAddressConstant(th), Elidability.Elidable, _)) :: xs if th.name == vname =>
        canBeInlined(vname, synced, target, xs).map(add(CyclesAndBytes(9, 2)))
      case (_, ZLine(LD, TwoRegisters(MEM_ABS_8, A), MemoryAddressConstant(th), Elidability.Elidable, _)) :: xs if th.name == vname =>
        canBeInlined(vname, synced, target, xs).map(add(CyclesAndBytes(9, 2)))

      case (_, ZLine(LD, TwoRegisters(A, MEM_ABS_8), CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(1, _)), Elidability.Elidable, _)) :: xs if th.name == vname =>
        canBeInlined(vname, synced, target, xs).map(add(CyclesAndBytes(9, 2)))
      case (_, ZLine(LD, TwoRegisters(MEM_ABS_8, A), CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(1, _)), Elidability.Elidable, _)) :: xs if th.name == vname =>
        canBeInlined(vname, synced, target, xs).map(add(CyclesAndBytes(9, 2)))

      case (_, ZLine0(_, _, MemoryAddressConstant(th))) :: _ if th.name == vname => fail(4)
      case (_, ZLine0(_, _, CompoundConstant(_, MemoryAddressConstant(th), _))) :: _ if th.name == vname => fail(5)
      case (_, ZLine0(_, _, SubbyteConstant(MemoryAddressConstant(th), _))) :: _ if th.name == vname => fail(6)
      case (_, ZLine0(CALL, _, _)) :: xs => target match {
          // TODO: check return type and allow HL sometimes
        case BC | DE =>
          canBeInlined(vname, synced, target, xs).map(add(CyclesAndBytes(-21, -2)))
        case _ => fail(3)
      }
      case (_, x) :: xs if x.changesRegister(target) => fail(1)
      case (_, x) :: xs if x.readsRegister(target) && !synced => fail(2)
      case (_, ZLine0(LABEL, _, _)) :: xs => canBeInlined(vname, synced = false, target, xs)
      case (_, ZLine0(CALL, _, _)) :: xs => fail(3)
      case _ :: xs => canBeInlined(vname, synced, target, xs)
      case _ => Some(CyclesAndBytes.Zero)
    }
  }

  def inlineVars(hl: String, bc: String, de: String, code: List[(FlowInfo, ZLine)]): TailRec[List[ZLine]] = {
    //    if (code.nonEmpty) println(code.head)
    code match {

      case (_, load@ZLine(LD_16, TwoRegisters(BC, IMM_16), _, _, s1)) ::
        (f, add@ZLine(ADD_16, TwoRegisters(HL, BC), _, _, s2)) ::
        xs if bc != "" =>
        if (f.importanceAfter.getRegister(DE) == Important) {
          tailcall(inlineVars(hl, bc, de, xs)).map(
            ZLine.register(PUSH, BC).pos(s1) :: load :: add :: ZLine.register(POP, BC).pos(s2) :: _)
        } else {
          tailcall(inlineVars(hl, bc, de, xs)).map(
            load.copy(registers = TwoRegisters(DE, IMM_16)) :: ZLine.registers(ADD_16, HL, DE).pos(s2) :: _)
        }

      case (_, load@ZLine(LD_16, TwoRegisters(DE, IMM_16), _, _, s1)) ::
        (f, add@ZLine(ADD_16, TwoRegisters(HL, DE), _, _, s2)) ::
        xs if de != "" =>
        if (f.importanceAfter.getRegister(BC) == Important) {
          tailcall(inlineVars(hl, bc, de, xs)).map(
            ZLine.register(PUSH, DE).pos(s1) :: load :: add :: ZLine.register(POP, DE).pos(s2) :: _)
        } else {
          tailcall(inlineVars(hl, bc, de, xs)).map(
            load.copy(registers = TwoRegisters(BC, IMM_16)) :: ZLine.registers(ADD_16, HL, BC).pos(s2) :: _)
        }

      case (_, ZLine(LD_16, TwoRegisters(HL, MEM_ABS_16), a1@MemoryAddressConstant(th1), _, s1)) ::
        (_, ZLine(ADD_16, TwoRegisters(HL, reg@(DE | BC)), _, _, s2)) ::
        (_, ZLine(LD_16, TwoRegisters(MEM_ABS_16, HL), a2@MemoryAddressConstant(th2), _, s3)) ::
        xs if hl != "" && th1.name != hl && th2.name != hl =>
        // bytes before: 3 + 1 + 3 = 7
        // cycles before: 16 + 11 + 16 = 43
        // bytes after: 3 + 1 + 3 + 3 + 1 + 3 = 14
        // cycles after: 13 + 4 + 13 + 13 + 4 + 13 = 60
        val (h, l) = reg match {
          case BC => (B, C)
          case DE => (D, E)
        }
        tailcall(inlineVars(hl, bc, de, xs)).map(
          ZLine.ldAbs8(A, a1).pos(s1) ::
            ZLine.register(ADD, l).pos(s2) ::
            ZLine.ldAbs8(a2, A).pos(s3) ::
            ZLine.ldAbs8(A, a1 + 1).pos(s1) ::
            ZLine.register(ADC, h).pos(s2) ::
            ZLine.ldAbs8(a2 + 1, A).pos(s3) :: _)

      case (_, ZLine(LD_16, TwoRegisters(MEM_ABS_16, HL), MemoryAddressConstant(th), _, s1)) ::
        (_, loadConst@ZLine(LD_16, TwoRegisters(BC, constSource), _, _, s2)) ::
        (_, add@ZLine(ADD_16, TwoRegisters(HL, BC), _, _, s3)) :: xs if th.name == bc =>
        tailcall(inlineVars(hl, bc, de, xs)).map(
          ZLine.ld8(B, H).pos(s1) :: ZLine.ld8(C, L).pos(s1) ::
            loadConst.copy(registers = TwoRegisters(HL, constSource)) ::
            add.copy(registers = TwoRegisters(HL, BC)) :: _)

      case (_, ZLine(LD_16, TwoRegisters(MEM_ABS_16, HL), MemoryAddressConstant(th), _, s1)) ::
        (_, loadConst@ZLine0(LD_16, TwoRegisters(DE, constSource), _)) ::
        (_, add@ZLine0(ADD_16, TwoRegisters(HL, DE), _)) :: xs if th.name == de =>
        tailcall(inlineVars(hl, bc, de, xs)).map(
          ZLine.ld8(D, H).pos(s1) :: ZLine.ld8(E, L).pos(s1) ::
            loadConst.copy(registers = TwoRegisters(HL, constSource)) ::
            add.copy(registers = TwoRegisters(HL, DE)) :: _)
      // TODO: above with regs swapped

      case (_, loadConst@ZLine0(LD_16, TwoRegisters(t, constSource), _)) ::
        (_, ZLine0(LD_16, TwoRegisters(HL, MEM_ABS_16), MemoryAddressConstant(th))) ::
        (_, add@ZLine0(ADD_16, TwoRegisters(HL, t2), _)) :: xs if th.name == bc && t == t2 && t != HL =>
        tailcall(inlineVars(hl, bc, de, xs)).map(
          loadConst.copy(registers = TwoRegisters(HL, constSource)) ::
            add.copy(registers = TwoRegisters(HL, BC)) :: _)

      case (_, loadConst@ZLine0(LD_16, TwoRegisters(t, constSource), _)) ::
        (_, ZLine0(LD_16, TwoRegisters(HL, MEM_ABS_16), MemoryAddressConstant(th))) ::
        (_, add@ZLine0(ADD_16, TwoRegisters(HL, t2), _)) :: xs if th.name == de && t == t2 && t != HL =>
        tailcall(inlineVars(hl, bc, de, xs)).map(
          loadConst.copy(registers = TwoRegisters(HL, constSource)) ::
            add.copy(registers = TwoRegisters(HL, DE)) :: _)

      case (_, ZLine(LD_16, TwoRegisters(HL, MEM_ABS_16), MemoryAddressConstant(th), _, s)) :: xs if th.name == hl =>
        tailcall(inlineVars(hl, bc, de, xs))
      case (_, ZLine(LD_16, TwoRegisters(MEM_ABS_16, HL), MemoryAddressConstant(th), _, s)) :: xs if th.name == hl =>
        tailcall(inlineVars(hl, bc, de, xs))
      case (_, ZLine(LD_16, TwoRegisters(HL, MEM_ABS_16), MemoryAddressConstant(th), _, s)) :: xs if th.name == bc =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(H, B).pos(s) :: ZLine.ld8(L, C).pos(s) :: _)
      case (_, ZLine(LD_16, TwoRegisters(MEM_ABS_16, HL), MemoryAddressConstant(th), _, s)) :: xs if th.name == bc =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(B, H).pos(s) :: ZLine.ld8(C, L).pos(s) :: _)
      case (_, ZLine(LD_16, TwoRegisters(HL, MEM_ABS_16), MemoryAddressConstant(th), _, s)) :: xs if th.name == de =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(H, D).pos(s) :: ZLine.ld8(L, E).pos(s) :: _)
      case (_, ZLine(LD_16, TwoRegisters(MEM_ABS_16, HL), MemoryAddressConstant(th), _, s)) :: xs if th.name == de =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(D, H).pos(s) :: ZLine.ld8(E, L).pos(s) :: _)

      case (_, ZLine(LD_16, TwoRegisters(DE, MEM_ABS_16), MemoryAddressConstant(th), _, s)) :: xs if th.name == de =>
        tailcall(inlineVars(hl, bc, de, xs))
      case (_, ZLine(LD_16, TwoRegisters(MEM_ABS_16, DE), MemoryAddressConstant(th), _, s)) :: xs if th.name == de =>
        tailcall(inlineVars(hl, bc, de, xs))
      case (_, ZLine(LD_16, TwoRegisters(DE, MEM_ABS_16), MemoryAddressConstant(th), _, s)) :: xs if th.name == bc =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(D, B).pos(s) :: ZLine.ld8(E, C).pos(s) :: _)
      case (_, ZLine(LD_16, TwoRegisters(MEM_ABS_16, DE), MemoryAddressConstant(th), _, s)) :: xs if th.name == bc =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(B, D).pos(s) :: ZLine.ld8(C, E).pos(s) :: _)
      case (_, ZLine(LD_16, TwoRegisters(DE, MEM_ABS_16), MemoryAddressConstant(th), _, s)) :: xs if th.name == hl =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(D, H).pos(s) :: ZLine.ld8(E, L).pos(s) :: _)
      case (_, ZLine(LD_16, TwoRegisters(MEM_ABS_16, DE), MemoryAddressConstant(th), _, s)) :: xs if th.name == hl =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(H, D).pos(s) :: ZLine.ld8(L, E).pos(s) :: _)

      case (_, ZLine(LD_16, TwoRegisters(BC, MEM_ABS_16), MemoryAddressConstant(th), _, s)) :: xs if th.name == bc =>
        tailcall(inlineVars(hl, bc, de, xs))
      case (_, ZLine(LD_16, TwoRegisters(MEM_ABS_16, BC), MemoryAddressConstant(th), _, s)) :: xs if th.name == bc =>
        tailcall(inlineVars(hl, bc, de, xs))
      case (_, ZLine(LD_16, TwoRegisters(BC, MEM_ABS_16), MemoryAddressConstant(th), _, s)) :: xs if th.name == hl =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(B, H).pos(s) :: ZLine.ld8(C, L).pos(s) :: _)
      case (_, ZLine(LD_16, TwoRegisters(MEM_ABS_16, BC), MemoryAddressConstant(th), _, s)) :: xs if th.name == hl =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(H, B).pos(s) :: ZLine.ld8(L, C).pos(s) :: _)
      case (_, ZLine(LD_16, TwoRegisters(BC, MEM_ABS_16), MemoryAddressConstant(th), _, s)) :: xs if th.name == de =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(B, D).pos(s) :: ZLine.ld8(C, E).pos(s) :: _)
      case (_, ZLine(LD_16, TwoRegisters(MEM_ABS_16, BC), MemoryAddressConstant(th), _, s)) :: xs if th.name == de =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(D, B).pos(s) :: ZLine.ld8(E, C).pos(s) :: _)

      case (_, ZLine(LD, TwoRegisters(A, MEM_ABS_8), MemoryAddressConstant(th), _, s)) :: xs if th.name == hl =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(A, L).pos(s) :: _)
      case (_, ZLine(LD, TwoRegisters(MEM_ABS_8, A), MemoryAddressConstant(th), _, s)) :: xs if th.name == hl =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(L, A).pos(s) :: _)
      case (_, ZLine(LD, TwoRegisters(A, MEM_ABS_8), MemoryAddressConstant(th), _, s)) :: xs if th.name == bc =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(A, C).pos(s) :: _)
      case (_, ZLine(LD, TwoRegisters(MEM_ABS_8, A), MemoryAddressConstant(th), _, s)) :: xs if th.name == bc =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(C, A).pos(s) :: _)
      case (_, ZLine(LD, TwoRegisters(A, MEM_ABS_8), MemoryAddressConstant(th), _, s)) :: xs if th.name == de =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(A, E).pos(s) :: _)
      case (_, ZLine(LD, TwoRegisters(MEM_ABS_8, A), MemoryAddressConstant(th), _, s)) :: xs if th.name == de =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(E, A).pos(s) :: _)

      case (_, ZLine(LD, TwoRegisters(A, MEM_ABS_8), CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(1, _)), _, s)) :: xs if th.name == hl =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(A, H).pos(s) :: _)
      case (_, ZLine(LD, TwoRegisters(MEM_ABS_8, A), CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(1, _)), _, s)) :: xs if th.name == hl =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(H, A).pos(s) :: _)
      case (_, ZLine(LD, TwoRegisters(A, MEM_ABS_8), CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(1, _)), _, s)) :: xs if th.name == bc =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(A, B).pos(s) :: _)
      case (_, ZLine(LD, TwoRegisters(MEM_ABS_8, A), CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(1, _)), _, s)) :: xs if th.name == bc =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(B, A).pos(s) :: _)
      case (_, ZLine(LD, TwoRegisters(A, MEM_ABS_8), CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(1, _)), _, s)) :: xs if th.name == de =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(A, D).pos(s) :: _)
      case (_, ZLine(LD, TwoRegisters(MEM_ABS_8, A), CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(1, _)), _, s)) :: xs if th.name == de =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.ld8(D, A).pos(s) :: _)

      case (_, l1@ZLine(LD_16, TwoRegisters(BC, IMM_16), _, _, s1)) :: (_, l2@ZLine(ADD_16, TwoRegisters(HL, BC), _, _, s2)) :: xs if bc != "" =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.register(PUSH, BC).pos(s1) :: l1 :: l2 :: ZLine.register(POP, BC).pos(s2) :: _)

      case (_, l1@ZLine(LD_16, TwoRegisters(DE, IMM_16), _, _, s1)) :: (_, l2@ZLine(ADD_16, TwoRegisters(HL, DE), _, _, s2)) :: xs if de != "" =>
        tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.register(PUSH, DE).pos(s1) :: l1 :: l2 :: ZLine.register(POP, DE).pos(s2) :: _)

      case (_, x@ZLine(CALL, _, _, _, s)) :: xs =>
        if (bc != "") {
          tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.register(PUSH, BC).pos(s) :: x :: ZLine.register(POP, BC).pos(s) :: _)
        } else if (de != "") {
          tailcall(inlineVars(hl, bc, de, xs)).map(ZLine.register(PUSH, DE).pos(s) :: x :: ZLine.register(POP, DE).pos(s) :: _)
        } else {
          throw new IllegalStateException()
        }


      case x :: (_, ZLine0(LD_16, TwoRegisters(MEM_ABS_16, HL), MemoryAddressConstant(th))) :: xs if x._2.changesRegister(HL) && th.name == hl =>
        tailcall(inlineVars(hl, bc, de, xs)).map(x._2 :: _)
      case x :: (_, ZLine0(LD_16, TwoRegisters(MEM_ABS_16, BC), MemoryAddressConstant(th))) :: xs if x._2.changesRegister(BC) && th.name == bc =>
        tailcall(inlineVars(hl, bc, de, xs)).map(x._2 :: _)
      case x :: (_, ZLine0(LD_16, TwoRegisters(MEM_ABS_16, DE), MemoryAddressConstant(th))) :: xs if x._2.changesRegister(DE) && th.name == de =>
        tailcall(inlineVars(hl, bc, de, xs)).map(x._2 :: _)

      case x :: _ if bc != "" && x._2.changesRegister(BC) => throw new IllegalStateException()
      case x :: _ if de != "" && x._2.changesRegister(DE) => throw new IllegalStateException()
      case x :: _ if hl != "" && x._2.changesRegister(HL) => throw new IllegalStateException()

      case x :: xs => tailcall(inlineVars(hl, bc, de, xs)).map(x._2 :: _)
      case Nil => done(Nil)
    }
  }
}
