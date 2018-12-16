package millfork.assembly.z80.opt

import millfork.assembly.opt.FlowCache
import millfork.assembly.z80._
import millfork.env._
import millfork.node.ZRegister

/**
  * @author Karol Stasiak
  */

sealed trait Importance {
  def ~(that: Importance): Importance = (this, that) match {
    case (_, Important) | (Important, _) => Important
    case (_, Unimportant) | (Unimportant, _) => Unimportant
    case (UnknownImportance, UnknownImportance) => UnknownImportance
  }
}

case object Important extends Importance {
  override def toString = "!"
}

case object Unimportant extends Importance {
  override def toString = "*"
}

case object UnknownImportance extends Importance {
  override def toString = "?"
}

//noinspection RedundantNewCaseClass
case class CpuImportance(a: Importance = UnknownImportance,
                         b: Importance = UnknownImportance,
                         c: Importance = UnknownImportance,
                         d: Importance = UnknownImportance,
                         e: Importance = UnknownImportance,
                         h: Importance = UnknownImportance,
                         l: Importance = UnknownImportance,
                         hlNumeric: Importance = UnknownImportance,
                         ixh: Importance = UnknownImportance,
                         ixl: Importance = UnknownImportance,
                         iyh: Importance = UnknownImportance,
                         iyl: Importance = UnknownImportance,
                         memIx: Map[Int, Importance] = Map(),
                         zf: Importance = UnknownImportance,
                         nf: Importance = UnknownImportance,
                         cf: Importance = UnknownImportance,
                         sf: Importance = UnknownImportance,
                         pf: Importance = UnknownImportance,
                         hf: Importance = UnknownImportance
                        ) {
  override def toString: String = {
    val memRepr = if (memIx.isEmpty) "" else (0 to memIx.keys.max).map(i => memIx.getOrElse(i, UnknownImportance)).mkString("")
    s"A=$a,B=$b,C=$c,D=$d,E=$e,H=$h,L=$l,IX=$ixh$ixl,IY=$iyh$iyl; Z=$zf,C=$cf,N=$nf,S=$sf,P=$pf,H=$hf; HL=$hlNumeric; M=" ++ memRepr.padTo(4, ' ')
  }

  def ~(that: CpuImportance) = new CpuImportance(
    a = this.a ~ that.a,
    b = this.b ~ that.b,
    c = this.c ~ that.c,
    d = this.d ~ that.d,
    e = this.e ~ that.e,
    h = this.h ~ that.h,
    l = this.l ~ that.l,
    hlNumeric = this.hlNumeric ~ that.hlNumeric,
    ixh = this.ixh ~ that.ixh,
    ixl = this.ixl ~ that.ixl,
    iyh = this.iyh ~ that.iyh,
    iyl = this.iyl ~ that.iyl,
    memIx = (this.memIx.keySet | that.memIx.keySet).map(k => k -> (this.memIx.getOrElse(k, UnknownImportance) ~ that.memIx.getOrElse(k, UnknownImportance))).toMap,
    zf = this.zf ~ that.zf,
    nf = this.nf ~ that.nf,
    cf = this.cf ~ that.cf,
    pf = this.pf ~ that.pf,
    hf = this.hf ~ that.hf,
    sf = this.sf ~ that.sf,
  )

  def getRegister(register: ZRegister.Value, offset: Int = -1): Importance = register match {
    case ZRegister.A => a
    case ZRegister.B => b
    case ZRegister.C => c
    case ZRegister.D => d
    case ZRegister.E => e
    case ZRegister.H => h
    case ZRegister.L => l
    case ZRegister.IXH => ixh
    case ZRegister.IXL => ixl
    case ZRegister.IYH => iyh
    case ZRegister.IYL => iyl
    case ZRegister.MEM_IX_D => if (offset < 0) ??? else memIx.getOrElse(offset, UnknownImportance)
    case ZRegister.HL => h ~ l
    case ZRegister.BC => b ~ c
    case ZRegister.DE => d ~ e
    case ZRegister.IX => ixh ~ ixl
    case ZRegister.IY => iyh ~ iyl
  }

  def getFlag(register: ZFlag.Value): Importance = register match {
    case ZFlag.C => cf
    case ZFlag.H => hf
    case ZFlag.P => pf
    case ZFlag.Z => zf
    case ZFlag.S => sf
    case ZFlag.N => nf
  }

  def butReadsRegister(r: ZRegister.Value, offset: Int = -1): CpuImportance = r match {
    case ZRegister.A => this.copy(a = Important)
    case ZRegister.AF => this.copy(a = Important, zf = Important, pf = Important, hf = Important, cf = Important, sf = Important)
    case ZRegister.B => this.copy(b = Important)
    case ZRegister.C => this.copy(c = Important)
    case ZRegister.BC | ZRegister.MEM_BC => this.copy(b = Important, c = Important)
    case ZRegister.D => this.copy(d = Important)
    case ZRegister.E => this.copy(e = Important)
    case ZRegister.DE | ZRegister.MEM_DE => this.copy(d = Important, e = Important)
    case ZRegister.H => this.copy(h = Important, hlNumeric = Important)
    case ZRegister.L => this.copy(l = Important, hlNumeric = Important)
    case ZRegister.HL => this.copy(h = Important, l = Important, hlNumeric = Important)
    case ZRegister.MEM_HL => this.copy(h = Important, l = Important)
    case ZRegister.IXH => this.copy(ixh = Important)
    case ZRegister.IXL => this.copy(ixl = Important)
    case ZRegister.IYH => this.copy(iyh = Important)
    case ZRegister.IYL => this.copy(iyl = Important)
    case ZRegister.IX => this.copy(ixh = Important, ixl = Important)
    case ZRegister.MEM_IX_D => this.copy(ixh = Important, ixl = Important, memIx = if (offset < 0) memIx.mapValues(_ => Important) else memIx + (offset -> Important))
    case ZRegister.IY | ZRegister.MEM_IY_D => this.copy(iyh = Important, iyl = Important)
    case _ => this
  }

  def butWritesRegister(r: ZRegister.Value, offset: Int = -1): CpuImportance = r match {
    case ZRegister.A => this.copy(a = Unimportant)
    case ZRegister.AF => this.copy(a = Unimportant, zf = Unimportant, pf = Unimportant, hf = Unimportant, cf = Unimportant, sf = Unimportant, nf = Unimportant)
    case ZRegister.B => this.copy(b = Unimportant)
    case ZRegister.C => this.copy(c = Unimportant)
    case ZRegister.BC => this.copy(b = Unimportant, c = Unimportant)
    case ZRegister.MEM_BC => this.copy(b = Important, c = Important)
    case ZRegister.D => this.copy(d = Unimportant)
    case ZRegister.E => this.copy(e = Unimportant)
    case ZRegister.DE => this.copy(d = Unimportant, e = Unimportant)
    case ZRegister.MEM_DE => this.copy(d = Important, e = Important)
    case ZRegister.H => this.copy(h = Unimportant)
    case ZRegister.L => this.copy(l = Unimportant)
    case ZRegister.HL => this.copy(h = Unimportant, l = Unimportant, hlNumeric = Unimportant)
    case ZRegister.MEM_HL => this.copy(h = Important, l = Important)
    case ZRegister.IXH => this.copy(ixh = Unimportant)
    case ZRegister.IXL => this.copy(ixl = Unimportant)
    case ZRegister.IYH => this.copy(iyh = Unimportant)
    case ZRegister.IYL => this.copy(iyl = Unimportant)
    case ZRegister.IX => this.copy(ixh = Unimportant, ixl = Unimportant, memIx = memIx.mapValues(_ => Unimportant))
    case ZRegister.MEM_IX_D => this.copy(ixh = Important, ixl = Important, memIx = if (offset < 0) Map() else memIx + (offset -> Unimportant))
    case ZRegister.IY => this.copy(iyh = Important, iyl = Important)
    case ZRegister.MEM_IY_D => this.copy(iyh = Important, iyl = Important)
    case _ => this
  }

  def butReadsFlag(f: ZFlag.Value): CpuImportance = f match {
    case ZFlag.P => this.copy(pf = Important)
    case ZFlag.S => this.copy(sf = Important)
    case ZFlag.C => this.copy(cf = Important)
    case ZFlag.Z => this.copy(zf = Important)
    case ZFlag.H => this.copy(hf = Important)
    case _ => this
  }

  def butWritesFlag(f: ZFlag.Value): CpuImportance = f match {
    case ZFlag.P => this.copy(pf = Unimportant)
    case ZFlag.S => this.copy(sf = Unimportant)
    case ZFlag.C => this.copy(cf = Unimportant)
    case ZFlag.Z => this.copy(zf = Unimportant)
    case ZFlag.H => this.copy(hf = Unimportant)
    case _ => this
  }
}

object ReverseFlowAnalyzer {

  val cache = new FlowCache[ZLine, CpuImportance]("z80 reverse")

  val readsA = Set("__mul_u8u8u8", "__mul_u16u8u16")
  val readsB = Set("")
  val readsC = Set("")
  val readsD = Set("__mul_u8u8u8","__mul_u16u8u16")
  val readsE = Set("__mul_u16u8u16")
  val readsH = Set("")
  val readsL = Set("")

  //noinspection RedundantNewCaseClass
  def analyze(f: NormalFunction, code: List[ZLine]): List[CpuImportance] = {
    cache.get(code).foreach(return _)
    val importanceArray = Array.fill[CpuImportance](code.length)(new CpuImportance())
    val codeArray = code.toArray

    var changed = true
    val finalImportance = new CpuImportance(
      a = Important, b = Important, c = Important, d = Important, e = Important, h = Important, l = Important,
      ixh = Important, ixl = Important, iyh = Important, iyl = Important,
      zf = Important, cf = Important, sf = Important, pf = Important, hf = Important)
    changed = true
    while (changed) {
      changed = false
      var currentImportance: CpuImportance = finalImportance
      for (i <- codeArray.indices.reverse) {
        import millfork.assembly.z80.ZOpcode._
        if (importanceArray(i) != currentImportance) {
          changed = true
          importanceArray(i) = currentImportance
        }
        val currentLine = codeArray(i)
        currentLine match {
          case ZLine0(LABEL | EI | DI | NOP, _, _) => ()
          case ZLine0(DJNZ, _, MemoryAddressConstant(Label(l))) =>
            val labelIndex = getLabelIndex(codeArray, l)
            currentImportance = if (labelIndex < 0) finalImportance else (importanceArray(labelIndex) ~ currentImportance).butReadsRegister(ZRegister.B).butReadsFlag(ZFlag.Z)
          case ZLine0(JP | JR, IfFlagSet(flag), MemoryAddressConstant(Label(l))) =>
            val labelIndex = getLabelIndex(codeArray, l)
            currentImportance = if (labelIndex < 0) finalImportance else importanceArray(labelIndex) ~ currentImportance.butReadsFlag(flag)
          case ZLine0(JP | JR, IfFlagClear(flag), MemoryAddressConstant(Label(l))) =>
            val labelIndex = getLabelIndex(codeArray, l)
            currentImportance = if (labelIndex < 0) finalImportance else importanceArray(labelIndex) ~ currentImportance.butReadsFlag(flag)
          case ZLine0(JP | JR, NoRegisters, MemoryAddressConstant(Label(l))) =>
            val labelIndex = getLabelIndex(codeArray, l)
            currentImportance = if (labelIndex < 0) finalImportance else importanceArray(labelIndex)
          case ZLine0(DISCARD_HL, _, _) =>
            currentImportance = currentImportance.copy(h = Unimportant, l = Unimportant)
          case ZLine0(DISCARD_DE, _, _) =>
            currentImportance = currentImportance.copy(d = Unimportant, e = Unimportant)
          case ZLine0(DISCARD_BC, _, _) =>
            currentImportance = currentImportance.copy(b = Unimportant, c = Unimportant)
          case ZLine0(DISCARD_IX, _, _) =>
            currentImportance = currentImportance.copy(ixh = Unimportant, ixl = Unimportant)
          case ZLine0(DISCARD_IY, _, _) =>
            currentImportance = currentImportance.copy(iyh = Unimportant, iyl = Unimportant)
          case ZLine0(DISCARD_A, _, _) =>
            currentImportance = currentImportance.copy(a = Unimportant)
          case ZLine0(DISCARD_F, _, _) =>
            currentImportance = currentImportance.copy(cf = Unimportant, zf = Unimportant, sf = Unimportant, pf = Unimportant, hf = Unimportant, nf = Unimportant)
          case ZLine0(LD, TwoRegistersOffset(t, s, o), _) =>
            currentImportance = currentImportance.butWritesRegister(t, o).butReadsRegister(s, o)
          case ZLine0(LD | LD_16, TwoRegisters(t, s), _) =>
            currentImportance = currentImportance.butWritesRegister(t).butReadsRegister(s)
          case ZLine0(EX_DE_HL, TwoRegisters(t, s), _) =>
            currentImportance = currentImportance.copy(d = currentImportance.h, e = currentImportance.l, h = currentImportance.d, l = currentImportance.e)
          case ZLine0(ADD_16, TwoRegisters(t, s), _) =>
            currentImportance = currentImportance.butReadsRegister(t).butReadsRegister(s)
          case ZLine0(ADC_16 | SBC_16, TwoRegisters(t, s), _) =>
            currentImportance = currentImportance.butReadsRegister(t).butReadsRegister(s).butReadsFlag(ZFlag.C)

          case ZLine0(XOR, OneRegister(ZRegister.A), _) =>
            currentImportance = currentImportance.copy(
              a = Unimportant,
              cf = Unimportant,
              zf = Unimportant,
              sf = Unimportant,
              hf = Unimportant,
              pf = Unimportant
            )
          case ZLine0(OR | AND, OneRegister(ZRegister.A), _) =>
            currentImportance = currentImportance.copy(
              a = currentImportance.zf ~ currentImportance.sf ~ currentImportance.pf ~ currentImportance.a,
              cf = Unimportant,
              zf = Unimportant,
              sf = Unimportant,
              hf = Unimportant,
              pf = Unimportant
            )

          case ZLine0(ADD | SUB | CP, OneRegister(s), _) =>
            currentImportance = currentImportance.butReadsRegister(s).copy(
              a = Important,
              cf = Unimportant,
              zf = Unimportant,
              sf = Unimportant,
              hf = Unimportant,
              pf = Unimportant,
              nf = Unimportant
            )
          case ZLine0(ADD | SUB | CP, OneRegisterOffset(s, o), _) =>
            currentImportance = currentImportance.butReadsRegister(s, o).copy(
              a = Important,
              cf = Unimportant,
              zf = Unimportant,
              sf = Unimportant,
              hf = Unimportant,
              pf = Unimportant,
              nf = Unimportant
            )

          case ZLine0(AND | OR | XOR, OneRegister(s), _) =>
            currentImportance = currentImportance.butReadsRegister(s).copy(
              a = Important,
              cf = Unimportant,
              zf = Unimportant,
              pf = Unimportant,
              sf = Unimportant
            )

          case ZLine0(AND | OR | XOR, OneRegisterOffset(s, o), _) =>
            currentImportance = currentImportance.butReadsRegister(s, o).copy(
              a = Important,
              cf = Unimportant,
              zf = Unimportant,
              pf = Unimportant,
              sf = Unimportant
            )
          case ZLine0(ADC | SBC, OneRegister(s), _) =>
            currentImportance = currentImportance.butReadsRegister(s).copy(
              a = Important,
              cf = Important,
              zf = Unimportant,
              sf = Unimportant,
              hf = Unimportant,
              pf = Unimportant,
              nf = Unimportant
            )
          case ZLine0(ADC | SBC, OneRegisterOffset(s, o), _) =>
            currentImportance = currentImportance.butReadsRegister(s, o).copy(
              a = Important,
              cf = Important,
              zf = Unimportant,
              sf = Unimportant,
              hf = Unimportant,
              pf = Unimportant,
              nf = Unimportant
            )


          case ZLine0(DAA, _, _) =>
            currentImportance = currentImportance.copy(
              a = Important,
              hf = Important
            )
          case ZLine0(NEG, _, _) =>
            currentImportance = currentImportance.copy(
              a = Important
            )
          case ZLine0(INC | DEC | INC_16 | DEC_16, OneRegister(s), _) =>
            currentImportance = currentImportance.butReadsRegister(s)
          case ZLine0(INC | DEC | INC_16 | DEC_16, OneRegisterOffset(s, o), _) =>
            currentImportance = currentImportance.butReadsRegister(s, o)
          case ZLine0(op, OneRegister(s), _) if ZOpcodeClasses.AllSingleBit(op)=>
            currentImportance = currentImportance.butReadsRegister(s)
          case ZLine0(op, OneRegisterOffset(s, o), _) if ZOpcodeClasses.AllSingleBit(op)=>
            currentImportance = currentImportance.butReadsRegister(s, o)
          case ZLine0(POP, OneRegister(r), _) =>
            currentImportance = currentImportance.butWritesRegister(r)
          case ZLine0(PUSH, OneRegister(r), _) =>
            currentImportance = currentImportance.butReadsRegister(r)
          case ZLine0(CALL | JP, NoRegisters, MemoryAddressConstant(fun: FunctionInMemory)) =>
            val n = fun.name
            fun.params match {
              case NormalParamSignature(List(v)) if v.typ.size == 1 =>
                currentImportance = currentImportance.copy(
                  a = Important,
                  b = if (readsB(n)) Important else Unimportant,
                  c = if (readsC(n)) Important else Unimportant,
                  d = if (readsD(n)) Important else Unimportant,
                  e = if (readsE(n)) Important else Unimportant,
                  h = if (readsH(n)) Important else Unimportant,
                  l = if (readsL(n)) Important else Unimportant,
                  hlNumeric = Unimportant,
                  iyh = Unimportant,
                  iyl = Unimportant,
                  zf = Unimportant,
                  cf = Unimportant,
                  nf = Unimportant,
                  sf = Unimportant,
                  hf = Unimportant
                )
              case NormalParamSignature(List(v)) if v.typ.size == 2 =>
                currentImportance = currentImportance.copy(
                  a = if (readsA(n)) Important else Unimportant,
                  b = if (readsB(n)) Important else Unimportant,
                  c = if (readsC(n)) Important else Unimportant,
                  d = if (readsD(n)) Important else Unimportant,
                  e = if (readsE(n)) Important else Unimportant,
                  h = Important,
                  l = Important,
                  hlNumeric = Unimportant,
                  iyh = Unimportant,
                  iyl = Unimportant,
                  zf = Unimportant,
                  cf = Unimportant,
                  nf = Unimportant,
                  sf = Unimportant,
                  hf = Unimportant
                )
              case NormalParamSignature(List(v)) if v.typ.size == 3 =>
                currentImportance = currentImportance.copy(
                  a = if (readsA(n)) Important else Unimportant,
                  b = if (readsB(n)) Important else Unimportant,
                  c = if (readsC(n)) Important else Unimportant,
                  d = if (readsD(n)) Important else Unimportant,
                  e = Important,
                  h = Important,
                  l = Important,
                  hlNumeric = Unimportant,
                  iyh = Unimportant,
                  iyl = Unimportant,
                  zf = Unimportant,
                  cf = Unimportant,
                  nf = Unimportant,
                  sf = Unimportant,
                  hf = Unimportant
                )
              case NormalParamSignature(List(v)) if v.typ.size == 4 =>
                currentImportance = currentImportance.copy(
                  a = if (readsA(n)) Important else Unimportant,
                  b = if (readsB(n)) Important else Unimportant,
                  c = if (readsC(n)) Important else Unimportant,
                  d = Important,
                  e = Important,
                  h = Important,
                  l = Important,
                  hlNumeric = Unimportant,
                  iyh = Unimportant,
                  iyl = Unimportant,
                  zf = Unimportant,
                  cf = Unimportant,
                  nf = Unimportant,
                  sf = Unimportant,
                  hf = Unimportant
                )
              case NormalParamSignature(_) | AssemblyParamSignature(Nil) =>
                currentImportance = currentImportance.copy(
                  a = if (readsA(n)) Important else Unimportant,
                  b = if (readsB(n)) Important else Unimportant,
                  c = if (readsC(n)) Important else Unimportant,
                  d = if (readsD(n)) Important else Unimportant,
                  e = if (readsE(n)) Important else Unimportant,
                  h = if (readsH(n)) Important else Unimportant,
                  l = if (readsL(n)) Important else Unimportant,
                  hlNumeric = Unimportant,
                  iyh = Unimportant,
                  iyl = Unimportant,
                  zf = Unimportant,
                  cf = Unimportant,
                  nf = Unimportant,
                  sf = Unimportant,
                  hf = Unimportant
                )
              case _ =>
                currentImportance = finalImportance.copy(memIx = currentImportance.memIx)
            }

          case ZLine0(SLA | SRL, OneRegister(r), _) =>
            currentImportance = currentImportance.butReadsRegister(r).copy(cf = Unimportant, zf = Unimportant, hf = Unimportant, nf = Unimportant, pf = Unimportant)
          case ZLine0(RL | RR | RLC | RRC, OneRegister(r), _) =>
            currentImportance = currentImportance.butReadsRegister(r).copy(cf = Important, zf = Unimportant, hf = Unimportant, nf = Unimportant, pf = Unimportant)
          case ZLine0(SWAP, OneRegister(r), _) =>
            currentImportance = currentImportance.butReadsRegister(r).copy(cf = Unimportant, zf = Unimportant, hf = Unimportant, nf = Unimportant, pf = Unimportant)
          case ZLine0(RLA | RRA | RLCA | RRCA, _, _) =>
            currentImportance = currentImportance.butReadsRegister(ZRegister.A).copy(cf = Important, hf = Unimportant, nf = Unimportant)
          case ZLine0(SCF, _, _) =>
            currentImportance = currentImportance.copy(cf = Unimportant, hf = Unimportant, nf = Unimportant)
          case _ =>
            currentImportance = finalImportance // TODO
        }
      }
    }
//            importanceArray.zip(codeArray).foreach{
//              case (i, y) => if (y.isPrintable) println(f"$y%-32s $i%-32s")
//            }
//            println("---------------------")

    cache.put(code, importanceArray.toList)
  }

  private def getLabelIndex(codeArray: Array[ZLine], L: String) = {
    codeArray.indexWhere {
      case ZLine0(ZOpcode.LABEL, _, MemoryAddressConstant(Label(L))) => true
      case _ => false
    }
  }
}
