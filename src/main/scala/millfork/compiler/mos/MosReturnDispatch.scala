package millfork.compiler.mos

import millfork.CompilationFlag
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos.{AddrMode, _}
import millfork.compiler.{AbstractReturnDispatch, BranchSpec, CompilationContext}
import millfork.env._
import millfork.error.ConsoleLogger
import millfork.node._

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object MosReturnDispatch extends AbstractReturnDispatch[AssemblyLine] {

  override def compileImpl(ctx: CompilationContext,
                  stmt: ReturnDispatchStatement,
                  label: String,
                  actualMin: Int,
                  actualMax: Int,
                  paramArrays: IndexedSeq[InitializedArray],
                  paramMins: IndexedSeq[Int],
                  map: mutable.Map[Int, (Option[ThingInMemory], List[Expression])]): List[AssemblyLine] = {
    val env = ctx.env.root
    val b = env.get[VariableType]("byte")

    val useJmpaix = ctx.options.flag(CompilationFlag.EmitCmosOpcodes) && !ctx.options.flag(CompilationFlag.LUnixRelocatableCode) && (actualMax - actualMin) <= 127

    import AddrMode._
    import millfork.assembly.mos.Opcode._

    val ctxForStoringParams = ctx.neverCheckArrayBounds
    val copyParams = stmt.params.zipWithIndex.flatMap { case (paramVar, paramIndex) =>
      val storeParam = MosExpressionCompiler.compileByteStorage(ctxForStoringParams, MosRegister.A, paramVar)
      if (storeParam.exists(l => OpcodeClasses.ChangesX(l.opcode)))
        ctx.log.error("Invalid/too complex target parameter variable", paramVar.position)
      AssemblyLine.absoluteX(LDA, paramArrays(paramIndex), -paramMins(paramIndex)) :: storeParam
    }

    if (useJmpaix) {
      val jumpTable = InitializedArray(label + "$jt.array", None, (actualMin to actualMax).flatMap(i => List(lobyte0(map(i)._1), hibyte0(map(i)._1))).toList, ctx.function.declaredBank, b, b)
      env.registerUnnamedArray(jumpTable)
      if (copyParams.isEmpty) {
        val loadIndex = MosExpressionCompiler.compile(ctx, stmt.indexer, Some(b -> RegisterVariable(MosRegister.A, b)), BranchSpec.None)
        loadIndex ++ List(AssemblyLine.implied(ASL), AssemblyLine.implied(TAX)) ++ copyParams :+ AssemblyLine(JMP, AbsoluteIndexedX, jumpTable.toAddress - actualMin * 2)
      } else {
        val loadIndex = MosExpressionCompiler.compile(ctx, stmt.indexer, Some(b -> RegisterVariable(MosRegister.X, b)), BranchSpec.None)
        loadIndex ++ copyParams ++ List(
          AssemblyLine.implied(TXA),
          AssemblyLine.implied(ASL),
          AssemblyLine.implied(TAX),
          AssemblyLine(JMP, AbsoluteIndexedX, jumpTable.toAddress - actualMin * 2))
      }
    } else {
      val loadIndex = MosExpressionCompiler.compile(ctx, stmt.indexer, Some(b -> RegisterVariable(MosRegister.X, b)), BranchSpec.None)
      val jumpTableLo = InitializedArray(label + "$jl.array", None, (actualMin to actualMax).map(i => lobyte1(map(i)._1)).toList, ctx.function.declaredBank, b, b)
      val jumpTableHi = InitializedArray(label + "$jh.array", None, (actualMin to actualMax).map(i => hibyte1(map(i)._1)).toList, ctx.function.declaredBank, b, b)
      env.registerUnnamedArray(jumpTableLo)
      env.registerUnnamedArray(jumpTableHi)
      val actualJump = if (ctx.options.flag(CompilationFlag.LUnixRelocatableCode)) {
        List(
          AssemblyLine.absoluteX(LDA, jumpTableHi.toAddress - actualMin),
          AssemblyLine.implied(CLC),
          AssemblyLine.absolute(ADC, env.get[ThingInMemory]("relocation_offset")),
          AssemblyLine.implied(PHA),
          AssemblyLine.absoluteX(LDA, jumpTableLo.toAddress - actualMin),
          AssemblyLine.implied(PHA),
          AssemblyLine.implied(RTS))
      } else {
        List(
          AssemblyLine.absoluteX(LDA, jumpTableHi.toAddress - actualMin),
          AssemblyLine.implied(PHA),
          AssemblyLine.absoluteX(LDA, jumpTableLo.toAddress - actualMin),
          AssemblyLine.implied(PHA),
          AssemblyLine.implied(RTS))
      }
      loadIndex ++ copyParams ++ actualJump
    }
  }

  override def nextLabel(prefix: String): String = MosCompiler.nextLabel("di")
}
