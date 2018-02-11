package millfork.compiler

import millfork.CompilationFlag
import millfork.assembly.{AssemblyLine, OpcodeClasses}
import millfork.env._
import millfork.error.ErrorReporting
import millfork.node._

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object ReturnDispatch {

  def compile(ctx: CompilationContext, stmt: ReturnDispatchStatement): List[AssemblyLine] = {
    if (stmt.branches.isEmpty) {
      ErrorReporting.error("At least one branch is required", stmt.position)
      return Nil
    }

    def toConstant(e: Expression) = {
      ctx.env.eval(e).getOrElse {
        ErrorReporting.error("Non-constant parameter for dispatch branch", e.position)
        Constant.Zero
      }
    }

    def toInt(e: Expression): Int = {
      ctx.env.eval(e) match {
        case Some(NumericConstant(i, _)) =>
          if (i < 0 || i > 255) ErrorReporting.error("Branch labels have to be in the 0-255 range", e.position)
          i.toInt & 0xff
        case _ =>
          ErrorReporting.error("Branch labels have to early resolvable constants", e.position)
          0
      }
    }

    val indexerType = ExpressionCompiler.getExpressionType(ctx, stmt.indexer)
    if (indexerType.size != 1) {
      ErrorReporting.error("Return dispatch index expression type has to be a byte", stmt.indexer.position)
    }
    if (indexerType.isSigned) {
      ErrorReporting.warn("Return dispatch index expression type will be automatically casted to unsigned", ctx.options, stmt.indexer.position)
    }
    stmt.params.foreach{
      case e@VariableExpression(name) =>
        if (ctx.env.get[Variable](name).typ.size != 1) {
          ErrorReporting.error("Dispatch parameters should be bytes", e.position)
        }
      case _ => ()
    }

    val returnType = ctx.function.returnType
    val map = mutable.Map[Int, (Constant, List[Constant])]()
    var min = Option.empty[Int]
    var max = Option.empty[Int]
    var default = Option.empty[(Constant, List[Constant])]
    stmt.branches.foreach { branch =>
      val function = ctx.env.evalForAsm(branch.function).getOrElse {
        ErrorReporting.error("Non-constant function address for dispatch branch", branch.function.position)
        Constant.Zero
      }
      if (returnType.name != "void") {
        function match {
          case MemoryAddressConstant(f: FunctionInMemory) =>
            if (f.returnType.name != returnType.name) {
              ErrorReporting.warn(s"Dispatching to a function of different return type: dispatcher return type: ${returnType.name}, dispatchee return type: ${f.returnType.name}", ctx.options, branch.function.position)
            }
          case _ => ()
        }
      }
      val params = branch.params.map(toConstant)
      if (params.length > stmt.params.length) {
        ErrorReporting.error("Too many parameters for dispatch branch", branch.params.head.position)
      }
      branch.label match {
        case DefaultReturnDispatchLabel(start, end) =>
          if (default.isDefined) {
            ErrorReporting.error(s"Duplicate default dispatch label", branch.position)
          }
          min = start.map(toInt)
          max = end.map(toInt)
          default = Some(function -> params)
        case StandardReturnDispatchLabel(labels) =>
          labels.foreach { label =>
            val i = toInt(label)
            if (map.contains(i)) {
              ErrorReporting.error(s"Duplicate dispatch label: $label = $i", label.position)
            }
            map(i) = function -> params
          }
      }
    }
    val nonDefaultMin = map.keys.reduceOption(_ min _)
    val nonDefaultMax = map.keys.reduceOption(_ max _)
    val defaultMin = min.orElse(nonDefaultMin).getOrElse(0)
    val defaultMax = max.orElse(nonDefaultMax).getOrElse {
      ErrorReporting.error("Undefined maximum label for dispatch", stmt.position)
      defaultMin
    }
    val actualMin = defaultMin min nonDefaultMin.getOrElse(defaultMin)
    val actualMax = defaultMax max nonDefaultMax.getOrElse(defaultMax)
    val zeroes = Constant.Zero -> List[Constant]()
    for (i <- actualMin to actualMax) {
      if (!map.contains(i)) map(i) = default.getOrElse {
        // TODO: warning?
        zeroes
      }
    }

    val compactParams = ctx.options.flag(CompilationFlag.CompactReturnDispatchParams)
    val paramMins = stmt.params.indices.map { paramIndex =>
      if (compactParams) map.filter(_._2._2.length > paramIndex).keys.reduceOption(_ min _).getOrElse(0)
      else actualMin
    }
    val paramMaxes = stmt.params.indices.map { paramIndex =>
      if (compactParams) map.filter(_._2._2.length > paramIndex).keys.reduceOption(_ max _).getOrElse(0)
      else actualMax
    }

    var env = ctx.env
    while (env.parent.isDefined) env = env.parent.get
    val label = MfCompiler.nextLabel("di")
    val paramArrays = stmt.params.indices.map { ix =>
      val a = InitializedArray(label + "$" + ix + ".array", None, (paramMins(ix) to paramMaxes(ix)).map { key =>
        map(key)._2.lift(ix).getOrElse(Constant.Zero)
      }.toList)
      env.registerUnnamedArray(a)
      a
    }

    val useJmpaix = ctx.options.flag(CompilationFlag.EmitCmosOpcodes) && (actualMax - actualMin) <= 127
    val b = ctx.env.get[Type]("byte")

    import millfork.assembly.AddrMode._
    import millfork.assembly.Opcode._

    val ctxForStoringParams = ctx.neverCheckArrayBounds
    val copyParams = stmt.params.zipWithIndex.flatMap { case (paramVar, paramIndex) =>
      val storeParam = ExpressionCompiler.compileByteStorage(ctxForStoringParams, Register.A, paramVar)
      if (storeParam.exists(l => OpcodeClasses.ChangesX(l.opcode)))
        ErrorReporting.error("Invalid/too complex target parameter variable", paramVar.position)
      AssemblyLine.absoluteX(LDA, paramArrays(paramIndex), -paramMins(paramIndex)) :: storeParam
    }

    if (useJmpaix) {
      val jumpTable = InitializedArray(label + "$jt.array", None, (actualMin to actualMax).flatMap(i => List(map(i)._1.loByte, map(i)._1.hiByte)).toList)
      env.registerUnnamedArray(jumpTable)
      if (copyParams.isEmpty) {
        val loadIndex = ExpressionCompiler.compile(ctx, stmt.indexer, Some(b -> RegisterVariable(Register.A, b)), BranchSpec.None)
        loadIndex ++ List(AssemblyLine.implied(ASL), AssemblyLine.implied(TAX)) ++ copyParams :+ AssemblyLine(JMP, AbsoluteIndexedX, jumpTable.toAddress - actualMin * 2)
      } else {
        val loadIndex = ExpressionCompiler.compile(ctx, stmt.indexer, Some(b -> RegisterVariable(Register.X, b)), BranchSpec.None)
        loadIndex ++ copyParams ++ List(
          AssemblyLine.implied(TXA),
          AssemblyLine.implied(ASL),
          AssemblyLine.implied(TAX),
          AssemblyLine(JMP, AbsoluteIndexedX, jumpTable.toAddress - actualMin * 2))
      }
    } else {
      val loadIndex = ExpressionCompiler.compile(ctx, stmt.indexer, Some(b -> RegisterVariable(Register.X, b)), BranchSpec.None)
      val jumpTableLo = InitializedArray(label + "$jl.array", None, (actualMin to actualMax).map(i => (map(i)._1 - 1).loByte).toList)
      val jumpTableHi = InitializedArray(label + "$jh.array", None, (actualMin to actualMax).map(i => (map(i)._1 - 1).hiByte).toList)
      env.registerUnnamedArray(jumpTableLo)
      env.registerUnnamedArray(jumpTableHi)
      loadIndex ++ copyParams ++ List(
        AssemblyLine.absoluteX(LDA, jumpTableHi.toAddress - actualMin),
        AssemblyLine.implied(PHA),
        AssemblyLine.absoluteX(LDA, jumpTableLo.toAddress - actualMin),
        AssemblyLine.implied(PHA),
        AssemblyLine.implied(RTS))
    }
  }
}
