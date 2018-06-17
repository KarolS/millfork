package millfork.assembly.z80.opt

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
                         ixh: Importance = UnknownImportance,
                         ixl: Importance = UnknownImportance,
                         iyh: Importance = UnknownImportance,
                         iyl: Importance = UnknownImportance,
                         zf: Importance = UnknownImportance,
                         nf: Importance = UnknownImportance,
                         cf: Importance = UnknownImportance,
                         sf: Importance = UnknownImportance,
                         pf: Importance = UnknownImportance,
                         hf: Importance = UnknownImportance
                        ) {
  override def toString: String = s"A=$a,B=$b,C=$c,D=$d,E=$e,H=$h,L=$l,IX=$ixh$ixl,Y=$iyh$iyl; Z=$zf,C=$cf,N=$nf,S=$sf,P=$pf,H=$hf"

  def ~(that: CpuImportance) = new CpuImportance(
    a = this.a ~ that.a,
    b = this.a ~ that.a,
    c = this.a ~ that.a,
    d = this.a ~ that.a,
    e = this.a ~ that.a,
    h = this.a ~ that.a,
    l = this.a ~ that.a,
    ixh = this.ixh ~ that.ixh,
    ixl = this.ixl ~ that.ixl,
    iyh = this.iyh ~ that.iyh,
    iyl = this.iyl ~ that.iyl,
    zf = this.zf ~ that.zf,
    nf = this.nf ~ that.nf,
    cf = this.cf ~ that.cf,
    pf = this.pf ~ that.pf,
    hf = this.hf ~ that.hf,
  )

  def getRegister(register: ZRegister.Value): Importance = register match {
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

  def butReadsRegister(r: ZRegister.Value) = r match {
    case ZRegister.A => this.copy(a = Important)
    case ZRegister.AF => this.copy(a = Important, zf = Important, pf = Important, hf = Important, cf = Important, sf = Important)
    case ZRegister.B => this.copy(b = Important)
    case ZRegister.C => this.copy(c = Important)
    case ZRegister.BC | ZRegister.MEM_BC => this.copy(b = Important, c = Important)
    case ZRegister.D => this.copy(d = Important)
    case ZRegister.E => this.copy(e = Important)
    case ZRegister.DE | ZRegister.MEM_DE => this.copy(d = Important, e = Important)
    case ZRegister.H => this.copy(h = Important)
    case ZRegister.L => this.copy(l = Important)
    case ZRegister.HL | ZRegister.MEM_HL => this.copy(h = Important, l = Important)
    case ZRegister.IXH => this.copy(ixh = Important)
    case ZRegister.IXL => this.copy(ixl = Important)
    case ZRegister.IYH => this.copy(iyh = Important)
    case ZRegister.IYL => this.copy(iyl = Important)
    case ZRegister.IX | ZRegister.MEM_IX_D => this.copy(ixh = Important, ixl = Important)
    case ZRegister.IY | ZRegister.MEM_IY_D => this.copy(iyh = Important, iyl = Important)
    case _ => this
  }

  def butWritesRegister(r: ZRegister.Value): CpuImportance = r match {
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
    case ZRegister.HL => this.copy(h = Unimportant, l = Unimportant)
    case ZRegister.MEM_HL => this.copy(h = Important, l = Important)
    case ZRegister.IXH => this.copy(ixh = Unimportant)
    case ZRegister.IXL => this.copy(ixl = Unimportant)
    case ZRegister.IYH => this.copy(iyh = Unimportant)
    case ZRegister.IYL => this.copy(iyl = Unimportant)
    case ZRegister.IX => this.copy(ixh = Unimportant, ixl = Unimportant)
    case ZRegister.MEM_IX_D => this.copy(ixh = Important, ixl = Important)
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

  //noinspection RedundantNewCaseClass
  def analyze(f: NormalFunction, code: List[ZLine]): List[CpuImportance] = {
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
          case ZLine(DJNZ, _, MemoryAddressConstant(Label(l)), _) =>
            val L = l
            val labelIndex = codeArray.indexWhere {
              case ZLine(LABEL, _, MemoryAddressConstant(Label(L)), _) => true
              case _ => false
            }
            currentImportance = if (labelIndex < 0) finalImportance else (importanceArray(labelIndex) ~ currentImportance).butReadsRegister(ZRegister.B).butReadsFlag(ZFlag.Z)
          case ZLine(JP | JR, IfFlagSet(_) | IfFlagClear(_), MemoryAddressConstant(Label(l)), _) =>
            val L = l
            val labelIndex = codeArray.indexWhere {
              case ZLine(LABEL, _, MemoryAddressConstant(Label(L)), _) => true
              case _ => false
            }
            currentImportance = if (labelIndex < 0) finalImportance else importanceArray(labelIndex) ~ currentImportance
          case ZLine(DISCARD_HL, _, _, _) =>
            currentImportance = currentImportance.copy(h =  Unimportant, l = Unimportant)
          case ZLine(DISCARD_BCDEIX, _, _, _) =>
            currentImportance = currentImportance.copy(b = Unimportant, c = Unimportant, d = Unimportant, e = Unimportant, ixh = Unimportant, ixl = Unimportant)
          case ZLine(DISCARD_A, _, _, _) =>
            currentImportance = currentImportance.copy(a = Unimportant)
          case ZLine(DISCARD_F, _, _, _) =>
            currentImportance = currentImportance.copy(cf = Unimportant, zf= Unimportant, sf = Unimportant , pf = Unimportant, hf = Unimportant)
          case ZLine(LD | LD_16, TwoRegisters(t, s), _, _) =>
            currentImportance = currentImportance.butWritesRegister(t).butReadsRegister(s)
          case ZLine(ADD_16, TwoRegisters(t, s), _, _) =>
            currentImportance = currentImportance.butReadsRegister(t).butReadsRegister(s)
          case ZLine(XOR, OneRegister(ZRegister.A), _, _) =>
            currentImportance = currentImportance.butWritesRegister(ZRegister.A)
          case ZLine(OR | AND, OneRegister(ZRegister.A), _, _) =>
            currentImportance = currentImportance.butReadsRegister(ZRegister.A)
          case ZLine(AND | ADD | SUB | OR | XOR, OneRegister(s), _, _) =>
            currentImportance = currentImportance.butReadsRegister(ZRegister.A).butReadsRegister(s)
          case ZLine(ADC | SBC, OneRegister(s), _, _) =>
            currentImportance = currentImportance.butReadsRegister(ZRegister.A).butReadsRegister(s).butReadsFlag(ZFlag.C)
          case ZLine(DAA, _, _, _) =>
            currentImportance = currentImportance.butReadsRegister(ZRegister.A).butReadsFlag(ZFlag.H)
          case ZLine(CP, OneRegister(s), _, _) =>
            currentImportance = currentImportance.butReadsRegister(s)
          case ZLine(INC | DEC | INC_16 | DEC_16, OneRegister(s), _, _) =>
            currentImportance = currentImportance.butReadsRegister(s)
          case _ =>
            currentImportance = finalImportance // TODO
        }
      }
    }
//            importanceArray.zip(codeArray).foreach{
//              case (i, y) => if (y.isPrintable) println(f"$y%-32s $i%-32s")
//            }
//            println("---------------------")

    importanceArray.toList
  }
}
