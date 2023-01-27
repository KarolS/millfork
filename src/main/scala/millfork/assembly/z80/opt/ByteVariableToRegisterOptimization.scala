package millfork.assembly.z80.opt

import millfork.assembly.z80._
import millfork.assembly.{AssemblyOptimization, OptimizationContext}
import millfork.env._
import millfork.node.ZRegister
import millfork.{CompilationFlag, NonOverlappingIntervals}

import scala.collection.mutable.ListBuffer

/**
  * @author Karol Stasiak
  */
object ByteVariableToRegisterOptimization extends AssemblyOptimization[ZLine] {

  override def requiredFlags: Set[CompilationFlag.Value] = Set(CompilationFlag.RegisterVariables)

  override def name = "Allocating variables to single registers"

  override def minimumRequiredLines: Int = 3

  object CyclesAndBytes {
    val Zero = CyclesAndBytes(0, 0)
  }

  case class CyclesAndBytes(bytes: Int, cycles: Int) {
    def +(that: CyclesAndBytes) = CyclesAndBytes(this.bytes + that.bytes, this.cycles + that.cycles)

    def *(scale: Double) = CyclesAndBytes(bytes.*(scale).round.toInt, cycles.*(scale).round.toInt)
  }

  override def optimize(f: NormalFunction, code: List[ZLine], optimizationContext: OptimizationContext): List[ZLine] = {
    val vs = VariableStatus(f, code, optimizationContext, _.size == 1, allowParams = true).getOrElse(return code)
    val options = optimizationContext.options
    val useIx = options.flag(CompilationFlag.UseIxForStack)
    val useIy = options.flag(CompilationFlag.UseIyForStack)
    val log = options.log
    val removeVariablesForReal = !options.flag(CompilationFlag.InternalCurrentlyOptimizingForMeasurement)
    val costFunction: CyclesAndBytes => Int = if (options.flag(CompilationFlag.OptimizeForSpeed)) _.cycles else _.bytes
    lazy val savingsForRemovingOneStackVariable = {
      val localVariableAreaSize = code.flatMap {
        case ZLine0(_, OneRegisterOffset(ZRegister.MEM_IX_D, offset), _) if useIx => Some(offset)
        case ZLine0(_, TwoRegistersOffset(_, ZRegister.MEM_IX_D, offset), _) if useIx => Some(offset)
        case ZLine0(_, TwoRegistersOffset(ZRegister.MEM_IX_D, _, offset), _) if useIx => Some(offset)
        case ZLine0(_, OneRegisterOffset(ZRegister.MEM_IY_D, offset), _) if useIy => Some(offset)
        case ZLine0(_, TwoRegistersOffset(_, ZRegister.MEM_IY_D, offset), _) if useIy => Some(offset)
        case ZLine0(_, TwoRegistersOffset(ZRegister.MEM_IY_D, _, offset), _) if useIy => Some(offset)
        case _ => None
      }.toSet.size
      val prologueAndEpilogue = if (f.returnType.size == 2) CyclesAndBytes(107, 20) else CyclesAndBytes(95, 17)
      localVariableAreaSize match {
        case 1 => prologueAndEpilogue
        case 2 => prologueAndEpilogue * 0.4
        case 3 => prologueAndEpilogue * 0.1
        case _ => CyclesAndBytes.Zero
      }

    }

    val bCandidates = getCandidates(vs, _.b, ZRegister.B, savingsForRemovingOneStackVariable, useIx, useIy)
    val cCandidates = getCandidates(vs, _.c, ZRegister.C, savingsForRemovingOneStackVariable, useIx, useIy)
    val dCandidates = getCandidates(vs, _.d, ZRegister.D, savingsForRemovingOneStackVariable, useIx, useIy)
    val eCandidates = getCandidates(vs, _.e, ZRegister.E, savingsForRemovingOneStackVariable, useIx, useIy)
    val hCandidates = getCandidates(vs, _.h, ZRegister.H, savingsForRemovingOneStackVariable, useIx, useIy)
    val lCandidates = getCandidates(vs, _.l, ZRegister.L, savingsForRemovingOneStackVariable, useIx, useIy)

    val bCandidateSets = NonOverlappingIntervals.apply[(String, Range, CyclesAndBytes)](bCandidates, _._2.start, _._2.end)
    val cCandidateSets = NonOverlappingIntervals.apply[(String, Range, CyclesAndBytes)](cCandidates, _._2.start, _._2.end)
    val dCandidateSets = NonOverlappingIntervals.apply[(String, Range, CyclesAndBytes)](dCandidates, _._2.start, _._2.end)
    val eCandidateSets = NonOverlappingIntervals.apply[(String, Range, CyclesAndBytes)](eCandidates, _._2.start, _._2.end)
    val hCandidateSets = NonOverlappingIntervals.apply[(String, Range, CyclesAndBytes)](hCandidates, _._2.start, _._2.end)
    val lCandidateSets = NonOverlappingIntervals.apply[(String, Range, CyclesAndBytes)](lCandidates, _._2.start, _._2.end)

    val variants = for {
      vb <- if (options.flag(CompilationFlag.SingleThreaded)) bCandidateSets else bCandidateSets.par
      nb = vb.map(_._1)

      vc <- cCandidateSets
      nc = vc.map(_._1)
      if (nb & nc).isEmpty

      vd <- dCandidateSets
      nd = vd.map(_._1)
      if (nb & nd).isEmpty
      if (nc & nd).isEmpty

      ve <- eCandidateSets
      ne = ve.map(_._1)
      if (nb & ne).isEmpty
      if (nc & ne).isEmpty
      if (nd & ne).isEmpty

      vh <- hCandidateSets
      nh = vh.map(_._1)
      if (nb & nh).isEmpty
      if (nc & nh).isEmpty
      if (nd & nh).isEmpty
      if (ne & nh).isEmpty

      vl <- lCandidateSets
      nl = vl.map(_._1)
      if (nb & nl).isEmpty
      if (nc & nl).isEmpty
      if (nd & nl).isEmpty
      if (ne & nl).isEmpty
      if (nh & nl).isEmpty


      score = vb.toSeq.map(x => costFunction(x._3)).sum +
        vc.toSeq.map(x => costFunction(x._3)).sum +
        vd.toSeq.map(x => costFunction(x._3)).sum +
        ve.toSeq.map(x => costFunction(x._3)).sum +
        vh.toSeq.map(x => costFunction(x._3)).sum +
        vl.toSeq.map(x => costFunction(x._3)).sum
    } yield (score, vb, vc, vd, ve, vh, vl)

    if (variants.isEmpty) {
      return code
    }

    //    variants.foreach(println)

    val (_, bestBs, bestCs, bestDs, bestEs, bestHs, bestLs) = variants.maxBy(_._1)

    def reportOptimizedBlock[T](oldCode: List[(T, ZLine)], newCode: List[ZLine]): Unit = {
      if (log.traceEnabled) {
        oldCode.foreach(l => log.trace(l._2.toString))
        log.trace("     ↓")
        newCode.foreach(l => log.trace(l.toString))
      }
    }

    if (bestBs.nonEmpty || bestCs.nonEmpty || bestDs.nonEmpty || bestEs.nonEmpty || bestHs.nonEmpty || bestLs.nonEmpty) {
      val output = ListBuffer[ZLine]()
      var i = 0
      @inline
      def tryInline(bests: Set[(String, Range, CyclesAndBytes)], register: ZRegister.Value): Boolean = {
        bests.find(_._2.start == i).exists {
          case (v, range, _) =>
            log.debug(s"Inlining $v to single register $register")
            val oldCode = vs.codeWithFlow.slice(range.start, range.end)
            val newCode = inlineVars(v, register, addressInHl = false, addressInBc = false, addressInDe = false, oldCode.map(_._2))
            reportOptimizedBlock(oldCode, newCode)
            if (vs.paramVariables(v)) {
              val variable = vs.localVariables.find(_.name.==(v)).get.asInstanceOf[MemoryVariable]
              val addr = variable.toAddress
              if (register == ZRegister.A) {
                output += ZLine.ldAbs8(ZRegister.A, addr, elidability = ZLine.elidability(variable))
              } else {
                output += ZLine.ldAbs8(ZRegister.A, addr, elidability = ZLine.elidability(variable))
                output += ZLine.ld8(register, ZRegister.A)
              }
            }
            output ++= newCode
            i = range.end
            if (removeVariablesForReal &&
              !v.startsWith("IX+") &&
              !v.startsWith("IY+") &&
              !v.startsWith("SP+") &&
              vs.variablesWithLifetimesMap.contains(v) &&
              contains(range, vs.variablesWithLifetimesMap(v))) {
              if (!vs.paramVariables(v)) f.environment.removeVariable(v)
            }
            true
          case _ => false
        }
      }
      while (i < code.length) {
        var done = false
        done =  tryInline(bestBs, ZRegister.B)
        if (!done) {
          done =  tryInline(bestCs, ZRegister.C)
        }
        if (!done) {
          done =  tryInline(bestDs, ZRegister.D)
        }
        if (!done) {
          done =  tryInline(bestEs, ZRegister.E)
        }
        if (!done) {
          done =  tryInline(bestHs, ZRegister.H)
        }
        if (!done) {
          done =  tryInline(bestLs, ZRegister.L)
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

  private def getCandidates(vs: VariableStatus, importanceExtractor: CpuImportance => Importance, register: ZRegister.Value, savingsForRemovingOneStackVariable: =>CyclesAndBytes, useIx: Boolean, useIy: Boolean): Seq[(String, Range, CyclesAndBytes)] = {
    vs.variablesWithLifetimes.filter {
      case (v, range) =>
        val tuple = vs.codeWithFlow(range.start)
        importanceExtractor(tuple._1.importanceAfter) != Important || {
//          println(s"Cannot inline ${v.name} to $register because of early $tuple")
          false
        }
    }.flatMap {
      case (v, range) =>
        val id = v match {
          case MemoryVariable(name, _, _) => name
          case StackVariable(_, _, offset) if useIx => "IX+" + offset
          case StackVariable(_, _, offset) if useIy => "IY+" + offset
          case StackVariable(_, _, offset) => "SP+" + offset
        }
        var bonus = CyclesAndBytes.Zero
        if (vs.variablesWithRegisterHint(v.name)) bonus += CyclesAndBytes(16, 16)
        if (vs.paramVariables(v.name)) bonus += (if (register == ZRegister.A) CyclesAndBytes(-13, -3) else CyclesAndBytes(-17, -4))
        if (id.startsWith("IX+") || id.startsWith("IY+")) bonus += savingsForRemovingOneStackVariable
        if (id.startsWith("SP+") || v.isVolatile) None
        else canBeInlined(id, synced = false, register, Some(false), Some(false), Some(false), vs.codeWithFlow.slice(range.start, range.end)).map { score =>
          (id, range, score + bonus)
        }
    }
  }

  def okPrefix(vs: VariableStatus, v: Variable, range: Range, reg: ZRegister.Value): Boolean = {
    if (!vs.paramVariables(v.name)) return true
    if (vs.paramRegs.contains(reg)) {
//      println(s"okPrefix $v false: reg $reg in use")
      return false
    }
    if (range.size < 2) {
//      println(s"okPrefix $v false: range $range too small")
      return false
    }
    if (range.start < 1) {
//      println(s"okPrefix $v true: range $range early enough")
      return true
    }
    import ZOpcode._
    vs.codeWithFlow.take(range.start) match {
      case Nil =>
      case (_, ZLine0(LABEL, _, _)) :: xs =>
        if (xs.exists { case (_, l) => l.opcode == LABEL || ZOpcodeClasses.NonLinear(l.opcode) }) {
//          println(s"okPrefix false: LABEL in prefix")
          return false
        }
      case _ => return false
    }
    vs.codeWithFlow(range.start - 1)._1.importanceAfter.getRegister(reg) == Unimportant
  }

  def contains(outer: Range, inner: Range): Boolean = {
    outer.contains(inner.start) && outer.contains(inner.end - 1)
  }

  import millfork.assembly.z80.ZOpcode._
  import millfork.node.ZRegister._

  def add(first: Boolean, ifTrue: CyclesAndBytes, ifFalse: CyclesAndBytes): CyclesAndBytes => CyclesAndBytes =
    if (first) _ + ifTrue else _ + ifFalse

  def add(value: CyclesAndBytes): CyclesAndBytes=>CyclesAndBytes = _ + value

  def canBeInlined(vname: String, synced: Boolean, target: ZRegister.Value, addressInHl: Option[Boolean], addressInBc: Option[Boolean], addressInDe: Option[Boolean], code: List[(FlowInfo, ZLine)]): Option[CyclesAndBytes] = {
    def fail(reason: Int): None.type = {
//      println(s"Cannot inline $vname to $target because of [[$reason]] ${code.head}")
      None
    }
    object NotTarget {
      def unapply(r: ZRegister.Value): Option[ZRegister.Value] = if (r == target) None else Some(r)
    }
    object ThisVar {
      def unapply(c: Constant): Option[Constant] = c match {
        case MemoryAddressConstant(th) if th.name == vname => Some(c)
        case _ => None
      }
    }
    object ThisOffsetX {
      def unapply(c: Int): Option[Int] = if ("IX+" + c == vname) Some(c) else None
    }
    object ThisOffsetY {
      def unapply(c: Int): Option[Int] = if ("IY+" + c == vname) Some(c) else None
    }
    code match {
      case (_, ZLine0(LD_16, TwoRegisters(HL | IY, SP), _)) :: _ if vname.startsWith("IX+") => fail(121)
      case (_, ZLine0(LD_16, TwoRegisters(HL | IX, SP), _)) :: _ if vname.startsWith("IY+") => fail(121)
      case (_, ZLine0(LD_16, TwoRegisters(SP, HL | IY), _)) :: _ if vname.startsWith("IX+") => fail(122)
      case (_, ZLine0(LD_16, TwoRegisters(SP, HL | IX), _)) :: _ if vname.startsWith("IY+") => fail(122)
      case (_, ZLine0(ADD_16 | SBC_16, TwoRegisters(HL | IY, SP), _)) :: _ if vname.startsWith("IX+") => fail(123)
      case (_, ZLine0(ADD_16 | SBC_16, TwoRegisters(HL | IX, SP), _)) :: _ if vname.startsWith("IY+") => fail(123)
      case (_, ZLine0(LD_HLSP | LD_DESP, _, _)) :: _ if vname.contains("+") => fail(124)
      case (_, ZLine0(EX_SP, _, _)) :: _ if vname.contains("+") => fail(125)

      case (_, ZLine0(LD, TwoRegisters(A, MEM_ABS_8), ThisVar(_))) :: xs =>
        canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe, xs).map(add(CyclesAndBytes(9, 2)))
      case (_, ZLine0(LD, TwoRegisters(MEM_ABS_8, A), ThisVar(_))) :: xs =>
        canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe, xs).map(add(CyclesAndBytes(9, 2)))

      case (_, ZLine0(LD, TwoRegistersOffset(reg, MEM_IX_D, ThisOffsetX(_)), _)) :: xs =>
        canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe, xs).map(add(reg == target, CyclesAndBytes(19, 3), CyclesAndBytes(15, 2)))
      case (_, ZLine0(LD, TwoRegistersOffset(MEM_IX_D, reg, ThisOffsetX(_)), _)) :: xs =>
        canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe, xs).map(add(reg == target, CyclesAndBytes(19, 3), CyclesAndBytes(15, 2)))
      case (_, ZLine0(_, OneRegisterOffset(MEM_IX_D, ThisOffsetX(_)), _)) :: xs =>
        canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe, xs).map(add(CyclesAndBytes(15, 2)))

      case (_, ZLine0(LD, TwoRegistersOffset(reg, MEM_IY_D, ThisOffsetY(_)), _)) :: xs =>
        canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe, xs).map(add(reg == target, CyclesAndBytes(19, 3), CyclesAndBytes(15, 2)))
      case (_, ZLine0(LD, TwoRegistersOffset(MEM_IY_D, reg, ThisOffsetY(_)), _)) :: xs =>
        canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe, xs).map(add(reg == target, CyclesAndBytes(19, 3), CyclesAndBytes(15, 2)))
      case (_, ZLine0(_, OneRegisterOffset(MEM_IY_D, ThisOffsetY(_)), _)) :: xs =>
        canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe, xs).map(add(CyclesAndBytes(15, 2)))

      case (_, ZLine0(LD_16, TwoRegisters(HL, IMM_16), ThisVar(_))) :: xs =>
        if (target == H || target == L) fail(61) else
        canBeInlined(vname, synced, target, addressInHl = Some(true), addressInBc, addressInDe, xs).map(add(CyclesAndBytes(10, 3)))
      case (_, ZLine0(LD_16, TwoRegisters(BC, IMM_16), ThisVar(_))) :: xs =>
        if (target == B || target == C) fail(61) else
        canBeInlined(vname, synced, target, addressInHl, addressInBc = Some(true), addressInDe, xs).map(add(CyclesAndBytes(10, 3)))
      case (_, ZLine0(LD_16, TwoRegisters(DE, IMM_16), ThisVar(_))) :: xs =>
        if (target == D || target == E) fail(61) else
        canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe = Some(true), xs).map(add(CyclesAndBytes(10, 3)))

        // TODO: other combinations:
      case (_, ZLine0(LD, TwoRegisters(L, C), _)) :: (_, ZLine0(LD, TwoRegisters(H, B), _)) :: xs if addressInBc.contains(true) =>
        canBeInlined(vname, synced, target, addressInHl = Some(true), addressInBc, addressInDe, xs).map(add(CyclesAndBytes(8, 2)))
      case (_, ZLine0(LD, TwoRegisters(H, B), _)) :: (_, ZLine0(LD, TwoRegisters(L, C), _)) :: xs if addressInBc.contains(true) =>
        canBeInlined(vname, synced, target, addressInHl = Some(true), addressInBc, addressInDe, xs).map(add(CyclesAndBytes(8, 2)))
      case (_, ZLine0(LD, TwoRegisters(L, E), _)) :: (_, ZLine0(LD, TwoRegisters(H, D), _)) :: xs if addressInDe.contains(true) =>
        canBeInlined(vname, synced, target, addressInHl = Some(true), addressInBc, addressInDe, xs).map(add(CyclesAndBytes(8, 2)))
      case (_, ZLine0(LD, TwoRegisters(H, D), _)) :: (_, ZLine0(LD, TwoRegisters(L, E), _)) :: xs if addressInDe.contains(true) =>
        canBeInlined(vname, synced, target, addressInHl = Some(true), addressInBc, addressInDe, xs).map(add(CyclesAndBytes(8, 2)))

      case (_, ZLine0(LD, TwoRegisters(C, L), _)) :: (_, ZLine0(LD, TwoRegisters(B, H), _)) :: xs if addressInHl.contains(true) =>
        canBeInlined(vname, synced, target, addressInHl, addressInBc = Some(true), addressInDe, xs).map(add(CyclesAndBytes(8, 2)))
      case (_, ZLine0(LD, TwoRegisters(B, H), _)) :: (_, ZLine0(LD, TwoRegisters(C, L), _)) :: xs if addressInHl.contains(true) =>
        canBeInlined(vname, synced, target, addressInHl, addressInBc = Some(true), addressInDe, xs).map(add(CyclesAndBytes(8, 2)))
      case (_, ZLine0(LD, TwoRegisters(E, L), _)) :: (_, ZLine0(LD, TwoRegisters(D, H), _)) :: xs if addressInHl.contains(true) =>
        canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe = Some(true), xs).map(add(CyclesAndBytes(8, 2)))
      case (_, ZLine0(LD, TwoRegisters(D, H), _)) :: (_, ZLine0(LD, TwoRegisters(E, L), _)) :: xs if addressInHl.contains(true) =>
        canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe = Some(true), xs).map(add(CyclesAndBytes(8, 2)))

      case (_, ZLine0(_, OneRegister(MEM_HL), _)) :: xs => addressInHl match {
        case Some(true) => canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe, xs).map(add(CyclesAndBytes(3, 0)))
        case Some(false) => canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe, xs)
        case None => fail(70)
      }

      case (_, ZLine0(LD, TwoRegisters(MEM_HL, NotTarget(_))  | TwoRegisters(NotTarget(_), MEM_HL), _)) :: xs => addressInHl match {
        case Some(true) => canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe, xs).map(add(CyclesAndBytes(3, 0)))
        case Some(false) => canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe, xs)
        case None => fail(71)
      }
      case (_, ZLine0(LD, TwoRegisters(MEM_BC, NotTarget(_)) | TwoRegisters(NotTarget(_), MEM_BC), _)) :: xs => addressInBc match {
        case Some(true) => canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe, xs).map(add(CyclesAndBytes(3, 0)))
        case Some(false) => canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe, xs)
        case None => fail(72)
      }
      case (_, ZLine0(LD, TwoRegisters(MEM_DE, NotTarget(_)) | TwoRegisters(NotTarget(_), MEM_DE), _)) :: xs => addressInDe match {
        case Some(true) => canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe, xs).map(add(CyclesAndBytes(3, 0)))
        case Some(false) => canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe, xs)
        case None => fail(73)
      }

      case (_, ZLine0(_, _, SubbyteConstant(ThisVar(_), _))) :: _ => fail(6)
      case (_, ZLine0(_, _, ThisVar(_))) :: _ => fail(4)
      case (_, ZLine0(_, _, CompoundConstant(_, ThisVar(_), _))) :: _ => fail(5)

      case (_, ZLine0(CALL, _, _)) :: xs =>
        // TODO: check return type and allow HL sometimes
        target match {
          case ZRegister.B | ZRegister.C | ZRegister.D | ZRegister.E =>
            canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe, xs).map(add(CyclesAndBytes(-21, -2)))
          case _ =>
            fail(3)
        }

      case (_, x) :: xs if x.changesRegister(target) => fail(1)
      case (_, x) :: xs if x.readsRegister(target) && !synced => fail(2)

        // TODO: tossing addresses around:
      // case (_, ZLine0(LD, TwoRegisters(H, B), _)) :: (_, ZLine0(LD, TwoRegisters(L, C), _)) :: xs =>


      case (_, x) :: xs if x.changesRegister(HL) => canBeInlined(vname, synced, target, addressInHl = Some(false), addressInBc, addressInDe, xs)
      case (_, x) :: xs if x.changesRegister(BC) => canBeInlined(vname, synced, target, addressInHl, addressInBc = Some(false), addressInDe, xs)
      case (_, x) :: xs if x.changesRegister(DE) => canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe = Some(false), xs)

      case (_, x) :: xs if x.readsRegister(HL) && addressInHl.contains(true) => fail(81)
      case (_, x) :: xs if x.readsRegister(BC) && addressInBc.contains(true) => fail(82)
      case (_, x) :: xs if x.readsRegister(DE) && addressInDe.contains(true) => fail(83)

      case (_, ZLine0(LABEL, _, _)) :: xs => canBeInlined(vname, synced = false, target, addressInHl, addressInBc, addressInDe, xs)
      case _ :: xs => canBeInlined(vname, synced, target, addressInHl, addressInBc, addressInDe, xs)
      case _ => Some(CyclesAndBytes.Zero)
    }
  }

  def inlineVars(vname: String, target: ZRegister.Value, addressInHl: Boolean, addressInBc: Boolean, addressInDe: Boolean, code: List[ZLine]): List[ZLine] = {
//    if (code.nonEmpty) println(code.head)
    code match {
      case ZLine(LD, TwoRegisters(A, MEM_ABS_8), MemoryAddressConstant(th), _, s) :: xs if th.name == vname =>
        ZLine.ld8(A, target).pos(s) :: inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)
      case ZLine(LD, TwoRegisters(MEM_ABS_8, A), MemoryAddressConstant(th), _, s) :: xs if th.name == vname =>
        ZLine.ld8(target, A).pos(s) :: inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)

      case ZLine(LD, TwoRegistersOffset(reg, MEM_IX_D, off), _, _, s) :: xs if "IX+" + off == vname =>
        if (reg == target) inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)
        else ZLine.ld8(reg, target).pos(s) :: inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)
      case ZLine(LD, TwoRegistersOffset(MEM_IX_D, IMM_8, off), param, _, s) :: xs if "IX+" + off == vname =>
        ZLine.ldImm8(target, param).pos(s) :: inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)
      case ZLine(LD, TwoRegistersOffset(MEM_IX_D, reg, off), _, _, s) :: xs if "IX+" + off == vname =>
        if (reg == target) inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)
        else ZLine.ld8(target, reg).pos(s) :: inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)
      case (l@ZLine0(_, OneRegisterOffset(MEM_IX_D, off), _)) :: xs if "IX+" + off == vname =>
        l.copy(registers = OneRegister(target)) :: inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)

      case ZLine(LD, TwoRegistersOffset(reg, MEM_IY_D, off), _, _, s) :: xs if "IY+" + off == vname =>
        if (reg == target) inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)
        else ZLine.ld8(reg, target).pos(s) :: inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)
      case ZLine(LD, TwoRegistersOffset(MEM_IY_D, IMM_8, off), param, _, s) :: xs if "IY+" + off == vname =>
        ZLine.ldImm8(target, param).pos(s) :: inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)
      case ZLine(LD, TwoRegistersOffset(MEM_IY_D, reg, off), _, _, s) :: xs if "IY+" + off == vname =>
        if (reg == target) inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)
        else ZLine.ld8(target, reg).pos(s) :: inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)
      case (l@ZLine0(_, OneRegisterOffset(MEM_IY_D, off), _)) :: xs if "IY+" + off == vname =>
        l.copy(registers = OneRegister(target)) :: inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)

      case  ZLine0(LD_16, TwoRegisters(HL, IMM_16), MemoryAddressConstant(th)) :: xs if th.name == vname =>
        inlineVars(vname, target, addressInHl = true, addressInBc, addressInDe, xs)
      case  ZLine0(LD_16, TwoRegisters(BC, IMM_16), MemoryAddressConstant(th)) :: xs if th.name == vname =>
        inlineVars(vname, target, addressInHl, addressInBc = true, addressInDe, xs)
      case ZLine0(LD_16, TwoRegisters(DE, IMM_16), MemoryAddressConstant(th)) :: xs if th.name == vname =>
        inlineVars(vname, target, addressInHl, addressInBc, addressInDe = true, xs)

      case (x@ZLine0(_, OneRegister(MEM_HL), _)) :: xs if addressInHl =>
        x.copy(registers = OneRegister(target)) ::
        inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)

      case (x@ZLine0(LD, TwoRegisters(MEM_HL, reg), p)) :: xs if addressInHl =>
        x.copy(registers = TwoRegisters(target, reg), parameter = p) ::
          inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)
      case (x@ZLine0(LD, TwoRegisters(reg, MEM_HL), p)) :: xs if addressInHl =>
        x.copy(registers = TwoRegisters(reg, target), parameter = p) ::
          inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)

      case (x@ZLine0(LD, TwoRegisters(MEM_BC, reg), p)) :: xs if addressInBc =>
        x.copy(registers = TwoRegisters(target, reg), parameter = p) ::
          inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)
      case (x@ZLine0(LD, TwoRegisters(reg, MEM_BC), p)) :: xs if addressInBc =>
        x.copy(registers = TwoRegisters(reg, target), parameter = p) ::
          inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)

      case (x@ZLine0(LD, TwoRegisters(MEM_DE, reg), p)) :: xs if addressInDe =>
        x.copy(registers = TwoRegisters(target, reg), parameter = p) ::
          inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)
      case (x@ZLine0(LD, TwoRegisters(reg, MEM_DE), p)) :: xs if addressInDe =>
        x.copy(registers = TwoRegisters(reg, target), parameter = p) ::
          inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)

        // TODO: other combinations
      case ZLine0(LD, TwoRegisters(L, C), _) :: ZLine0(LD, TwoRegisters(H, B), _) :: xs if addressInBc =>
        inlineVars(vname, target, addressInHl = true, addressInBc = true, addressInDe = addressInDe, xs)
      case ZLine0(LD, TwoRegisters(H, B), _) :: ZLine0(LD, TwoRegisters(L, C), _) :: xs if addressInBc =>
        inlineVars(vname, target, addressInHl = true, addressInBc = true, addressInDe = addressInDe, xs)
      case ZLine0(LD, TwoRegisters(L, E), _) :: ZLine0(LD, TwoRegisters(H, D), _) :: xs if addressInDe =>
        inlineVars(vname, target, addressInHl = true, addressInBc = addressInBc, addressInDe = true, xs)
      case ZLine0(LD, TwoRegisters(H, D), _) :: ZLine0(LD, TwoRegisters(L, E), _) :: xs if addressInDe =>
        inlineVars(vname, target, addressInHl = true, addressInBc = addressInBc, addressInDe = true, xs)

      case ZLine0(LD, TwoRegisters(C, L), _) :: ZLine0(LD, TwoRegisters(B, H), _) :: xs if addressInHl =>
        inlineVars(vname, target, addressInHl = true, addressInBc = true, addressInDe = addressInDe, xs)
      case ZLine0(LD, TwoRegisters(B, H), _) :: ZLine0(LD, TwoRegisters(C, L), _) :: xs if addressInHl =>
        inlineVars(vname, target, addressInHl = true, addressInBc = true, addressInDe = addressInDe, xs)
      case ZLine0(LD, TwoRegisters(E, L), _) :: ZLine0(LD, TwoRegisters(D, H), _) :: xs if addressInHl =>
        inlineVars(vname, target, addressInHl = true, addressInBc = addressInBc, addressInDe = true, xs)
      case ZLine0(LD, TwoRegisters(D, H), _) :: ZLine0(LD, TwoRegisters(E, L), _) :: xs if addressInHl =>
        inlineVars(vname, target, addressInHl = true, addressInBc = addressInBc, addressInDe = true, xs)

      case (x@ZLine(CALL,_,_,_,s))::xs =>
        // TODO: this push/pull pair shouldn't prevent the inlining to the other register in the pair
        target match {
          case ZRegister.B | ZRegister.C =>
            ZLine.register(PUSH, BC).pos(s) :: x :: ZLine.register(POP, BC).pos(s) ::
              inlineVars(vname, target, addressInHl = false, addressInBc, addressInDe = false, xs)
          case ZRegister.D | ZRegister.E =>
            ZLine.register(PUSH, DE).pos(s) :: x :: ZLine.register(POP, DE).pos(s) ::
              inlineVars(vname, target, addressInHl = false, addressInBc = false, addressInDe, xs)
        }

      case x :: xs if x.changesRegister(HL) =>
        x :: inlineVars(vname, target, addressInHl = false, addressInBc, addressInDe, xs)
      case x :: xs if x.changesRegister(BC) =>
        x :: inlineVars(vname, target, addressInHl, addressInBc = false, addressInDe, xs)
      case x :: xs if x.changesRegister(DE) =>
        x :: inlineVars(vname, target, addressInHl, addressInBc, addressInDe = false, xs)

      case x :: _ if x.changesRegister(target) => ???
      case x :: xs => x :: inlineVars(vname, target, addressInHl, addressInBc, addressInDe, xs)
      case Nil => Nil
    }
  }
}
