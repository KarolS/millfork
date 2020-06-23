package millfork.compiler.m6809

import millfork.assembly.m6809.MLine
import millfork.compiler.{AbstractReturnDispatch, CompilationContext}
import millfork.env.{Constant, InitializedArray, ThingInMemory, VariableType}
import millfork.node.{Expression, M6809Register, ReturnDispatchStatement}
import millfork.output.NoAlignment

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object M6809ReturnDispatch extends AbstractReturnDispatch[MLine] {
  override def compileImpl(ctx: CompilationContext,
                           stmt: ReturnDispatchStatement,
                           label: String,
                           actualMin: Int,
                           actualMax: Int,
                           paramArrays: IndexedSeq[InitializedArray],
                           paramMins: IndexedSeq[Int],
                           map: mutable.Map[Int, (Option[ThingInMemory], List[Expression])]): List[MLine] = {
    import millfork.assembly.m6809.MOpcode._
    import M6809Register.{A, B, D, X}
    val env = ctx.env.root
    val b = env.get[VariableType]("byte")
    val ctxForStoringParams = ctx.neverCheckArrayBounds
    val loadIndex = M6809ExpressionCompiler.compileToX(ctx, stmt.indexer)

    val pair = stmt.params.zipWithIndex.foldLeft(List[List[MLine]]()) { (p1, p2) =>
      (p1, p2) match {
        case (reversedResult, (paramVar, paramIndex)) =>
          val storeParam = M6809ExpressionCompiler.stashXIfNeeded(ctx, M6809ExpressionCompiler.storeB(ctxForStoringParams, paramVar))
          if (storeParam.exists(l => l.changesRegister(B))) {
            ctx.log.error("Invalid/too complex target parameter variable", paramVar.position)
            storeParam.foreach(l => ctx.log.debug(l.toString))
          }
          val nextArray = (paramArrays(paramIndex).toAddress - paramMins(paramIndex)).quickSimplify
          ((
            MLine.indexedX(LDB, nextArray) ::
              storeParam
            ) :: reversedResult)
      }
    }
    val copyParams = pair.reverse.flatten
    // TODO: would it be better to use one table of words and do TFR X,D / LEAX D,X / LDX array,X ?
    val jumpTableLo = InitializedArray(label + "$jl.array", None, (actualMin to actualMax).map(i => lobyte0(map(i)._1)).toList, ctx.function.declaredBank, b, b, readOnly = true, NoAlignment)
    val jumpTableHi = InitializedArray(label + "$jh.array", None, (actualMin to actualMax).map(i => hibyte0(map(i)._1)).toList, ctx.function.declaredBank, b, b, readOnly = true, NoAlignment)
    env.registerUnnamedArray(jumpTableLo)
    env.registerUnnamedArray(jumpTableHi)
    val moveOffsetToLo = (jumpTableLo.toAddress - actualMin).quickSimplify
    val moveOffsetToHi = (jumpTableHi.toAddress - actualMin).quickSimplify
    val loadAddressToD = List(
      MLine.indexedX(LDB, moveOffsetToLo),
      MLine.indexedX(LDA, moveOffsetToHi))
    loadIndex ++ copyParams ++ loadAddressToD ++ List(MLine.tfr(D, X), MLine.indexedX(JMP, 0))
  }
}
