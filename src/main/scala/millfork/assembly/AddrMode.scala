package millfork.assembly

/**
  * @author Karol Stasiak
  */
object AddrMode extends Enumeration {
  val Implied,
  Immediate,
  WordImmediate,
  Relative,
  LongRelative,
  ZeroPage,
  ZeroPageX,
  ZeroPageY,
  Absolute,
  AbsoluteX,
  AbsoluteY,
  LongAbsolute,
  LongAbsoluteX,
  Indirect,
  LongIndirect,
  IndexedX,
  IndexedY,
  IndexedSY,
  IndexedZ,
  Stack,
  LongIndexedY,
  LongIndexedZ,
  AbsoluteIndexedX,
  TripleAbsolute,
  Undecided,
  RawByte,
  DoesNotExist = Value


  def addrModeToMosString(am: AddrMode.Value, argument: String): String = {
    am match {
      case Implied => ""
      case Immediate => "#" + argument
      case WordImmediate => "##" + argument
      case AbsoluteX | ZeroPageX => argument + ", X"
      case AbsoluteY | ZeroPageY => argument + ", Y"
      case IndexedX | AbsoluteIndexedX => "(" + argument + ", X)"
      case Stack => argument + ", S"
      case IndexedY => "(" + argument + "), Y"
      case IndexedSY => "(" + argument + ", S), Y"
      case IndexedZ => "(" + argument + "), Z"
      case Indirect => "(" + argument + ")"
      case LongIndexedY => "[" + argument + "], Y"
      case LongIndexedZ => "[" + argument + "], Z"
      case LongIndirect => "[" + argument + "]"
      case ZeroPage => argument // + "\t;zp"
      case LongAbsolute => "FAR " + argument
      case LongAbsoluteX => "FAR " + argument + ", X"
      case _ => argument;
    }
  }
}
