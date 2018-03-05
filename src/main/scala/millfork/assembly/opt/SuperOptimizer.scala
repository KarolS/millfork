package millfork.assembly.opt

import millfork.{CompilationFlag, CompilationOptions, OptimizationPresets}
import millfork.assembly.{AddrMode, AssemblyLine, Opcode}
import millfork.env.NormalFunction
import millfork.error.ErrorReporting

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object SuperOptimizer extends AssemblyOptimization {

  def optimize(m: NormalFunction, code: List[AssemblyLine], options: CompilationOptions): List[AssemblyLine] = {
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
    if (options.flag(CompilationFlag.ZeropagePseudoregister)) {
      allOptimizers ++= ZeropageRegisterOptimizations.All
    }
    allOptimizers ++= List(
      VariableToRegisterOptimization,
      ChangeIndexRegisterOptimizationPreferringX2Y,
      ChangeIndexRegisterOptimizationPreferringY2X,
      UnusedLabelRemoval)
    val seenSoFar = mutable.Set[CodeView]()
    val queue = mutable.Queue[(List[AssemblyOptimization], List[AssemblyLine])]()
    val leaves = mutable.ListBuffer[(List[AssemblyOptimization], List[AssemblyLine])]()
    seenSoFar += viewCode(code)
    queue.enqueue(Nil -> code)
    while(queue.nonEmpty) {
      val (optsSoFar, codeSoFar) = queue.dequeue()
      var isLeaf = true
      allOptimizers.par.foreach { o =>
        val optimized = o.optimize(m, codeSoFar, options)
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
        leaves += optsSoFar -> codeSoFar
      }
    }

    val result = leaves.minBy(_._2.map(_.cost).sum)
    ErrorReporting.verbosity = oldVerbosity
    ErrorReporting.debug(s"Visited ${leaves.size} leaves")
    ErrorReporting.debug(s"${code.map(_.sizeInBytes).sum} B -> ${result._2.map(_.sizeInBytes).sum} B:   ${result._1.reverse.map(_.name).mkString(" -> ")}")
    result._1.reverse.foldLeft(code){(c, opt) =>
      val n = opt.optimize(m, c, options)
//      println(c.mkString("","",""))
//      println(n.mkString("","",""))
      n
    }
    result._2
  }

  override val name = "Superoptimizer"

  def viewCode(code: List[AssemblyLine]): CodeView = {
    CodeView(code.map(l => l.opcode -> l.addrMode))
  }
}

case class CodeView(content: List[(Opcode.Value, AddrMode.Value)])
