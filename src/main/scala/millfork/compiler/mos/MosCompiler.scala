package millfork.compiler.mos

import millfork.CompilationFlag
import millfork.assembly.Elidability
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos._
import millfork.compiler.{AbstractCompiler, CompilationContext}
import millfork.env._

/**
  * @author Karol Stasiak
  */
//noinspection NotImplementedCode,ScalaUnusedSymbol
object MosCompiler extends AbstractCompiler[AssemblyLine] {


  override def compile(ctx: CompilationContext): List[AssemblyLine] = {
    ctx.env.nameCheck(ctx.function.code)
    val chunk = packHalves(MosStatementCompiler.compile(ctx, new MosStatementPreprocessor(ctx, ctx.function.code)()))
    val zpRegisterSize = ctx.options.zpRegisterSize

    val storeParamsFromRegisters = (ctx.function.params match {
      case NormalParamSignature(List(param@MemoryVariable(_, typ, _))) if typ.size == 1 =>
        List(AssemblyLine.absolute(STA, param))
      case _ => Nil
    }).map(_.position(ctx.function.position))
    val phReg =
      (if (zpRegisterSize > 0) {
        val reg = ctx.env.get[VariableInMemory]("__reg")
        (0 until zpRegisterSize).flatMap { i =>
          List(
            AssemblyLine.zeropage(LDA, reg, i),
            AssemblyLine.implied(PHA))
        }.toList.map(_.position(ctx.function.position))
      } else Nil) ++ (
      if (ctx.options.flag(CompilationFlag.SoftwareStack)) {
        List(AssemblyLine.absolute(LDA, ctx.env.get[ThingInMemory]("__sp")), AssemblyLine.implied(PHA))
      } else Nil)

    val prefix = storeParamsFromRegisters ++ (if (ctx.function.interrupt) {

      if (ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)) {
        if (zpRegisterSize > 0) {
          val physicalRegisters = List(
            AssemblyLine.implied(PHB),
            AssemblyLine.implied(PHD),
            AssemblyLine.immediate(REP, 0x30),
            AssemblyLine.implied(PHA_W),
            AssemblyLine.implied(PHX_W),
            AssemblyLine.implied(PHY_W),
            AssemblyLine.implied(PHY_W))
          val reg = ctx.env.get[VariableInMemory]("__reg")
          val initialBytes = (0 to zpRegisterSize.&(0xfe).-(2) by 2).flatMap{ i=>
            List(
              AssemblyLine.zeropage(LDA_W, reg, i),
              AssemblyLine.implied(PHA_W))
          }
          val lastByte = if (zpRegisterSize % 2 != 0){
            List(
              AssemblyLine.immediate(SEP, 0x30),
              AssemblyLine.zeropage(LDA, reg, zpRegisterSize - 1),
              AssemblyLine.implied(PHA))
          } else {
            List(AssemblyLine.immediate(SEP, 0x30))
          }
          physicalRegisters ++ initialBytes ++ lastByte
        } else {
          List(
            AssemblyLine.implied(PHB),
            AssemblyLine.implied(PHD),
            AssemblyLine.immediate(REP, 0x30),
            AssemblyLine.implied(PHA),
            AssemblyLine.implied(PHX),
            AssemblyLine.implied(PHY),
            AssemblyLine.immediate(SEP, 0x30))
        }
      } else if (ctx.options.flag(CompilationFlag.EmitEmulation65816Opcodes)) {
        List(
          AssemblyLine.implied(PHB),
          AssemblyLine.implied(PHD),
          AssemblyLine.implied(PHA),
          AssemblyLine.implied(PHX),
          AssemblyLine.implied(PHY)) ++ phReg
      } else if (ctx.options.flag(CompilationFlag.Emit65CE02Opcodes)) {
        List(
          AssemblyLine.implied(PHA),
          AssemblyLine.implied(PHX),
          AssemblyLine.implied(PHY),
          AssemblyLine.implied(PHZ),
          AssemblyLine.implied(CLD)) ++ phReg
      } else if (ctx.options.flag(CompilationFlag.EmitCmosOpcodes)) {
        List(
          AssemblyLine.implied(PHA),
          AssemblyLine.implied(PHX),
          AssemblyLine.implied(PHY),
          AssemblyLine.implied(CLD)) ++ phReg
      } else {
        List(
          AssemblyLine.implied(PHA),
          AssemblyLine.implied(TXA),
          AssemblyLine.implied(PHA),
          AssemblyLine.implied(TYA),
          AssemblyLine.implied(PHA),
          AssemblyLine.implied(CLD)) ++ phReg
      }
    } else if (ctx.function.kernalInterrupt && zpRegisterSize > 0) {
      if (ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)) {
        val reg = ctx.env.get[VariableInMemory]("__reg")
        val initialBytes = (0 to zpRegisterSize.&(0xfe).-(2) by 2).flatMap { i =>
          List(
            AssemblyLine.zeropage(LDA_W, reg, i),
            AssemblyLine.implied(PHA_W))
        }.toList
        val lastByte = if (zpRegisterSize % 2 != 0) {
          List(AssemblyLine.accu8,
            AssemblyLine.zeropage(LDA_W, reg, zpRegisterSize - 1),
            AssemblyLine.implied(PHA_W))
        } else {
          List(AssemblyLine.accu8)
        }
        AssemblyLine.accu16 :: (initialBytes ++ lastByte)
      } else phReg
    } else Nil).map(_.position(ctx.function.position)) ++ stackPointerFixAtBeginning(ctx)
    val label = AssemblyLine.label(Label(ctx.function.name)).copy(elidability = Elidability.Fixed)
    label :: (prefix ++ chunk)
  }

  def stackPointerFixAtBeginning(ctx: CompilationContext): List[AssemblyLine] = {
    val m = ctx.function
    if (m.stackVariablesSize == 0 && m.name != "main") return Nil
    if (ctx.options.flag(CompilationFlag.SoftwareStack)) {
      val stackPointer = ctx.env.get[ThingInMemory]("__sp")
      if (m.name == "main") {
        List(
          AssemblyLine.immediate(LDX, 0xff - m.stackVariablesSize),
          AssemblyLine.absolute(STX, stackPointer)).map(_.position(m.position))
      } else if (m.stackVariablesSize < 3 || m.stackVariablesSize == 3 && ctx.prologueShouldAvoidA) {
        List.fill(m.stackVariablesSize)(AssemblyLine.absolute(DEC, stackPointer)).map(_.position(m.position))
      } else {
        List(AssemblyLine.absolute(LDA, stackPointer),
          AssemblyLine.implied(SEC),
          AssemblyLine.immediate(SBC, m.stackVariablesSize),
          AssemblyLine.absolute(STA, stackPointer)).map(_.position(m.position))
      }
    } else {
      if (ctx.options.flag(CompilationFlag.EmitIllegals)) {
        // TODO
        if (m.stackVariablesSize > 4 && !ctx.prologueShouldAvoidA)
          return List(
            AssemblyLine.implied(TSX),
            AssemblyLine.immediate(LDA, 0xff),
            AssemblyLine.immediate(SBX, m.stackVariablesSize),
            AssemblyLine.implied(TXS)).map(_.position(m.position)) // this TXS is fine, it won't appear in 65816 code
      }
      if (ctx.prologueShouldAvoidA && ctx.options.flag(CompilationFlag.EmitCmosOpcodes)) {
        return List.fill(m.stackVariablesSize)(AssemblyLine.implied(PHX)).map(_.position(m.position))
      }
      List.fill(m.stackVariablesSize)(AssemblyLine.implied(PHA)).map(_.position(m.position))
    }
  }

}
