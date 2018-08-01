package millfork.compiler

import millfork.CompilationFlag
import millfork.assembly.AbstractCode
import millfork.env._
import millfork.error.ConsoleLogger
import millfork.node._

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
abstract class AbstractReturnDispatch[T <: AbstractCode] {

  def compile(ctx: CompilationContext, stmt: ReturnDispatchStatement): List[T] = {
    if (stmt.branches.isEmpty) {
      ctx.log.error("At least one branch is required", stmt.position)
      return Nil
    }

    if (ctx.function.interrupt) {
      ctx.log.error(s"Return dispatch in interrupt function ${ctx.function.name}", stmt.position)
    }
    if (ctx.function.kernalInterrupt) {
      ctx.log.error(s"Return dispatch in kernal interrupt function ${ctx.function.name}", stmt.position)
    }

    def toConstant(e: Expression) = {
      ctx.env.eval(e).getOrElse {
        ctx.log.error("Non-constant parameter for dispatch branch", e.position)
        Constant.Zero
      }
    }

    def toInt(e: Expression): Int = {
      ctx.env.eval(e) match {
        case Some(NumericConstant(i, _)) =>
          if (i < 0 || i > 255) ctx.log.error("Branch labels have to be in the 0-255 range", e.position)
          i.toInt & 0xff
        case _ =>
          ctx.log.error("Branch labels have to early resolvable constants", e.position)
          0
      }
    }

    val indexerType = AbstractExpressionCompiler.getExpressionType(ctx, stmt.indexer)
    if (indexerType.size != 1) {
      ctx.log.error("Return dispatch index expression type has to be a byte", stmt.indexer.position)
    }
    if (indexerType.isSigned) {
      ctx.log.warn("Return dispatch index expression type will be automatically casted to unsigned", stmt.indexer.position)
    }
    stmt.params.foreach {
      case e@VariableExpression(name) =>
        if (ctx.env.get[Variable](name).typ.size != 1) {
          ctx.log.error("Dispatch parameters should be bytes", e.position)
        }
      case _ => ()
    }

    val returnType = ctx.function.returnType
    val map: mutable.Map[Int, (Option[ThingInMemory], List[Expression])] = mutable.Map()
    var min = Option.empty[Int]
    var max = Option.empty[Int]
    var default = Option.empty[(Option[ThingInMemory], List[Expression])]
    stmt.branches.foreach { branch =>
      val function: String = ctx.env.evalForAsm(branch.function) match {
        case Some(MemoryAddressConstant(f: FunctionInMemory)) =>
          if (f.returnType.name != returnType.name) {
            ctx.log.warn(s"Dispatching to a function of different return type: dispatcher return type: ${returnType.name}, dispatchee return type: ${f.returnType.name}", branch.function.position)
          }
          f.name
        case _ =>
          ctx.log.error("Undefined function or Non-constant function address for dispatch branch", branch.function.position)
          ""
      }
      branch.params.foreach(toConstant)
      val params = branch.params
      if (params.length > stmt.params.length) {
        ctx.log.error("Too many parameters for dispatch branch", branch.params.head.position)
      }
      branch.label match {
        case DefaultReturnDispatchLabel(start, end) =>
          if (default.isDefined) {
            ctx.log.error(s"Duplicate default dispatch label", branch.position)
          }
          min = start.map(toInt)
          max = end.map(toInt)
          default = Some(Some(ctx.env.get[FunctionInMemory](function)) -> params)
        case StandardReturnDispatchLabel(labels) =>
          labels.foreach { label =>
            val i = toInt(label)
            if (map.contains(i)) {
              ctx.log.error(s"Duplicate dispatch label: $label = $i", label.position)
            }
            map(i) = Some(ctx.env.get[FunctionInMemory](function)) -> params
          }
      }
    }
    val nonDefaultMin = map.keys.reduceOption(_ min _)
    val nonDefaultMax = map.keys.reduceOption(_ max _)
    val defaultMin = min.orElse(nonDefaultMin).getOrElse(0)
    val defaultMax = max.orElse(nonDefaultMax).getOrElse {
      ctx.log.error("Undefined maximum label for dispatch", stmt.position)
      defaultMin
    }
    val actualMin = defaultMin min nonDefaultMin.getOrElse(defaultMin)
    val actualMax = defaultMax max nonDefaultMax.getOrElse(defaultMax)
    val zeroes = None -> List[Expression]()
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

    val env = ctx.env.root
    val b = env.get[VariableType]("byte")
    val label = ctx.nextLabel("di")
    val paramArrays = stmt.params.indices.map { ix =>
      val a = InitializedArray(label + "$" + ix + ".array", None, (paramMins(ix) to paramMaxes(ix)).map { key =>
        map(key)._2.lift(ix).getOrElse(LiteralExpression(0, 1))
      }.toList,
        ctx.function.declaredBank, b, b)
      env.registerUnnamedArray(a)
      a
    }
    compileImpl(ctx, stmt, label, actualMin, actualMax, paramArrays, paramMins, map)
  }

  def compileImpl(ctx: CompilationContext,
                           stmt: ReturnDispatchStatement,
                           label: String,
                           actualMin: Int,
                           actualMax: Int,
                           paramArrays: IndexedSeq[InitializedArray],
                           paramMins: IndexedSeq[Int],
                           map: mutable.Map[Int, (Option[ThingInMemory], List[Expression])]): List[T]

  protected def zeroOr(function: Option[ThingInMemory])(F: ThingInMemory => Constant): Expression =
    function.fold[Expression](LiteralExpression(0, 1))(F andThen ConstantArrayElementExpression)

  protected def lobyte0(function: Option[ThingInMemory]): Expression = {
    zeroOr(function)(f => MemoryAddressConstant(f).loByte)
  }

  protected def hibyte0(function: Option[ThingInMemory]): Expression = {
    zeroOr(function)(f => MemoryAddressConstant(f).hiByte)
  }

  protected def lobyte1(function: Option[ThingInMemory]): Expression = {
    zeroOr(function)(f => MemoryAddressConstant(f).-(1).loByte)
  }

  protected def hibyte1(function: Option[ThingInMemory]): Expression = {
    zeroOr(function)(f => MemoryAddressConstant(f).-(1).hiByte)
  }

}
