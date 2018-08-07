package millfork.compiler.z80

import millfork.assembly.z80.{ZLine, ZOpcode}
import millfork.compiler.{AbstractReturnDispatch, CompilationContext}
import millfork.env.{Constant, InitializedArray, ThingInMemory, VariableType}
import millfork.error.ConsoleLogger
import millfork.node.{Expression, ReturnDispatchStatement, ZRegister}
import millfork.output.NoAlignment

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object Z80ReturnDispatch extends AbstractReturnDispatch[ZLine] {

  override def compileImpl(ctx: CompilationContext,
                           stmt: ReturnDispatchStatement,
                           label: String,
                           actualMin: Int,
                           actualMax: Int,
                           paramArrays: IndexedSeq[InitializedArray],
                           paramMins: IndexedSeq[Int],
                           map: mutable.Map[Int, (Option[ThingInMemory], List[Expression])]): List[ZLine] = {

    import ZRegister._
    import ZOpcode._
    val env = ctx.env.root
    val b = env.get[VariableType]("byte")
    val loadIndex = Z80ExpressionCompiler.compileToHL(ctx, stmt.indexer)
    val ctxForStoringParams = ctx.neverCheckArrayBounds
    val pair = stmt.params.zipWithIndex.foldLeft(Constant.Zero -> List[List[ZLine]]()) { (p1, p2) =>
      (p1, p2) match {
        case ((offset, reversedResult), (paramVar, paramIndex)) =>
          val storeParam =
            Z80ExpressionCompiler.stashBCIfChanged(ctx,
              Z80ExpressionCompiler.stashHLIfChanged(ctx,
                Z80ExpressionCompiler.storeA(ctxForStoringParams, paramVar, signedSource = false)))
          if (storeParam.exists(l => l.changesRegister(A))) {
            ctx.log.error("Invalid/too complex target parameter variable", paramVar.position)
            storeParam.foreach(l => ctx.log.debug(l.toString))
          }
          val nextArray = (paramArrays(paramIndex).toAddress - paramMins(paramIndex)).quickSimplify
          nextArray -> ((
            ZLine.ldImm16(BC, (nextArray - offset).quickSimplify) ::
              ZLine.registers(ADD_16, HL, BC) ::
              ZLine.ld8(A, MEM_HL) ::
              storeParam
            ) :: reversedResult)
      }
    }
    val copyParams = pair._2.reverse.flatten
    val offsetAfterParams = pair._1
    val jumpTableLo = InitializedArray(label + "$jl.array", None, (actualMin to actualMax).map(i => lobyte0(map(i)._1)).toList, ctx.function.declaredBank, b, b, NoAlignment)
    val jumpTableHi = InitializedArray(label + "$jh.array", None, (actualMin to actualMax).map(i => hibyte0(map(i)._1)).toList, ctx.function.declaredBank, b, b, NoAlignment)
    env.registerUnnamedArray(jumpTableLo)
    env.registerUnnamedArray(jumpTableHi)

    val moveOffsetToLo = (jumpTableLo.toAddress - offsetAfterParams - actualMin).quickSimplify
    val moveOffsetToHi = (jumpTableHi.toAddress - jumpTableLo.toAddress).quickSimplify
    val loadAddressToDE = List(
      ZLine.ldImm16(BC, moveOffsetToLo),
      ZLine.registers(ADD_16, HL, BC),
      ZLine.ld8(E, MEM_HL),
      ZLine.ldImm16(BC, moveOffsetToHi.quickSimplify),
      ZLine.registers(ADD_16, HL, BC),
      ZLine.ld8(D, MEM_HL))

    loadIndex ++ copyParams ++ loadAddressToDE ++ List(
      ZLine.ld8(H, D),
      ZLine.ld8(L, E),
      ZLine.register(JP, HL))
  }
}
