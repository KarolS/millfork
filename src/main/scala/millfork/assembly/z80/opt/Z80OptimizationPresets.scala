package millfork.assembly.z80.opt

import millfork.assembly.AssemblyOptimization
import millfork.assembly.z80.ZLine

/**
  * @author Karol Stasiak
  */
object Z80OptimizationPresets {

  val GoodForZ80: List[AssemblyOptimization[ZLine]] = {
    List.fill(5)(
      List.fill(5)(
        AlwaysGoodZ80Optimizations.All ++
          List(
            EmptyParameterStoreRemoval,
            EmptyMemoryStoreRemoval)
      ).flatten ++
        List(WordVariableToRegisterOptimization, ByteVariableToRegisterOptimization, CompactStackFrame)
    ).flatten
  }

  val GoodForIntel8080: List[AssemblyOptimization[ZLine]] = Nil // TODO

  val GoodForSharp: List[AssemblyOptimization[ZLine]] = Nil // TODO

}
