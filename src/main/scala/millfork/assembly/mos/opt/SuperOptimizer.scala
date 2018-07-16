package millfork.assembly.mos.opt

import millfork.{CompilationFlag, CompilationOptions, OptimizationPresets}
import millfork.assembly.{AssemblyOptimization, OptimizationContext}
import millfork.assembly.mos.{AddrMode, AssemblyLine, Opcode}
import millfork.env.NormalFunction
import millfork.error.ErrorReporting

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object SuperOptimizer extends AssemblyOptimization[AssemblyLine] {

  override def optimize(m: NormalFunction, code: List[AssemblyLine], optimizationContext: OptimizationContext): List[AssemblyLine] = {
    val options = optimizationContext.options
    val oldVerbosity = ErrorReporting.verbosity
    ErrorReporting.verbosity = -1
    var allOptimizers = OptimizationPresets.Good ++ LaterOptimizations.All
    if (options.flag(CompilationFlag.EmitIllegals)) {
      allOptimizers ++= UndocumentedOptimizations.All
    }
    if (options.flag(CompilationFlag.EmitCmosOpcodes)) {
      allOptimizers ++= CmosOptimizations.All
    } else {
      allOptimizers ++= LaterOptimizations.Nmos
    }
    if (options.flag(CompilationFlag.EmitEmulation65816Opcodes)) {
      allOptimizers ++= SixteenOptimizations.AllForEmulation
    }
    if (options.flag(CompilationFlag.EmitNative65816Opcodes)) {
      allOptimizers ++= SixteenOptimizations.AllForNative
    }
    if (options.flag(CompilationFlag.EmitHudsonOpcodes)) {
      allOptimizers ++= HudsonOptimizations.All
    }
    if (options.flag(CompilationFlag.Emit65CE02Opcodes)) {
      allOptimizers ++= CE02Optimizations.All
    }
    if (options.zpRegisterSize > 0) {
      allOptimizers ++= ZeropageRegisterOptimizations.All
    }
    allOptimizers ++= List(
      LocalVariableReadOptimization,
      ChangeIndexRegisterOptimizationPreferringX2Y,
      ChangeIndexRegisterOptimizationPreferringY2X)
    val seenSoFar = mutable.Set[CodeView]()
    val queue = mutable.Queue[(List[AssemblyOptimization[AssemblyLine]], List[AssemblyLine])]()
    val leaves = mutable.ListBuffer[(List[AssemblyOptimization[AssemblyLine]], List[AssemblyLine])]()

    val quickScrub = List(
      UnusedLabelRemoval,
      AlwaysGoodOptimizations.BranchInPlaceRemoval,
      AlwaysGoodOptimizations.PointlessLoadBeforeReturn,
      AlwaysGoodOptimizations.UnusedCodeRemoval
    )
    val optionsForMeasurements = options.copy(commandLineFlags = options.commandLineFlags + (CompilationFlag.InternalCurrentlyOptimizingForMeasurement -> true))
    val optimizationContextForMeasurements = optimizationContext.copy(options = optionsForMeasurements)
    val quicklyCleanedCode = quickScrub.foldLeft(code)((c, o) => o.optimize(m, c, optimizationContextForMeasurements))
    seenSoFar += viewCode(quicklyCleanedCode)
    queue.enqueue(quickScrub.reverse -> quicklyCleanedCode)

    while(queue.nonEmpty) {
      val (optsSoFar, codeSoFar) = queue.dequeue()
      var isLeaf = true
      (if (optionsForMeasurements.flag(CompilationFlag.SingleThreaded)) allOptimizers
      else allOptimizers.par).foreach { o =>
        val optimized = o.optimize(m, codeSoFar, optimizationContextForMeasurements)
        val view = viewCode(optimized)
        seenSoFar.synchronized{
          if (!seenSoFar(view)) {
            isLeaf = false
            seenSoFar += view
            queue.enqueue((o :: optsSoFar) -> optimized)
          }
        }
      }
      if (isLeaf) {
//        println(codeSoFar.map(_.sizeInBytes).sum + " B:   " + optsSoFar.reverse.map(_.name).mkString(" -> "))
//        val view = viewCode(codeSoFar)
//        println(f"${view.hashCode}%08X ${view.content.map(_._1).mkString(" ")}%s")
        leaves += optsSoFar -> codeSoFar
      }
    }

    val result = leaves.minBy(_._2.map(_.cost).sum)
    if (oldVerbosity != 0) {
      ErrorReporting.verbosity = oldVerbosity
    }
    ErrorReporting.debug(s"Visited ${leaves.size} leaves")
    ErrorReporting.debug(s"${code.map(_.sizeInBytes).sum} B -> ${result._2.map(_.sizeInBytes).sum} B:   ${result._1.reverse.map(_.name).mkString(" -> ")}")
    result._1.reverse.foldLeft(code){(c, opt) =>
      val n = opt.optimize(m, c, optimizationContext)
//      println(c.mkString("","",""))
//      println(n.mkString("","",""))
      n
    }
    result._2
  }

  override val name = "Superoptimizer"

  def viewCode(code: List[AssemblyLine]): CodeView = {
    val indexedSeq = code.view.map(l => l.opcode -> l.addrMode).toIndexedSeq
    CodeView(indexedSeq.hashCode(), indexedSeq)
  }
}

case class CodeView(override val hashCode: Int, content: IndexedSeq[(Opcode.Value, AddrMode.Value)])
