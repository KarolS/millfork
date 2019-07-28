package millfork.assembly

import millfork.assembly.m6809.MOpcode
import millfork.assembly.mos.Opcode
import millfork.assembly.z80.{ZOpcode, ZRegisters}

/**
  * @author Karol Stasiak
  */
case class BranchingOpcodeMapping(mosOpcode: Opcode.Value, z80Flags: ZRegisters, m6809: MOpcode.Value) {

}
