package millfork.output

import millfork.error.ErrorReporting
import millfork.node.{CallGraph, VariableVertex}
import millfork.{CompilationFlag, CompilationOptions}

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */

sealed trait ByteAllocator {
  def notifyAboutEndOfCode(org: Int): Unit

  def allocateBytes(count: Int, options: CompilationOptions): Int
}

class UpwardByteAllocator(startAt: Int, endBefore: Int) extends ByteAllocator {
  private var nextByte = startAt

  def allocateBytes(count: Int, options: CompilationOptions): Int = {
    if (count == 2 && (nextByte & 0xff) == 0xff && options.flag(CompilationFlag.PreventJmpIndirectBug)) nextByte += 1
    val t = nextByte
    nextByte += count
    if (nextByte > endBefore) {
      ErrorReporting.fatal("Out of high memory")
    }
    t
  }

  def notifyAboutEndOfCode(org: Int): Unit = ()
}

class AfterCodeByteAllocator(endBefore: Int) extends ByteAllocator {
  var nextByte = 0x200

  def allocateBytes(count: Int, options: CompilationOptions): Int = {
    if (count == 2 && (nextByte & 0xff) == 0xff && options.flag(CompilationFlag.PreventJmpIndirectBug)) nextByte += 1
    val t = nextByte
    nextByte += count
    if (nextByte > endBefore) {
      ErrorReporting.fatal("Out of high memory")
    }
    t
  }

  def notifyAboutEndOfCode(org: Int): Unit = nextByte = org
}

class VariableAllocator(private var pointers: List[Int], private val bytes: ByteAllocator) {

  private var pointerMap = mutable.Map[Int, Set[VariableVertex]]()
  private var variableMap = mutable.Map[Int, mutable.Map[Int, Set[VariableVertex]]]()

  var onEachByte: (Int => Unit) = _

  def allocatePointer(callGraph: CallGraph, p: VariableVertex): Int = {
    pointerMap.foreach { case (addr, alreadyThere) =>
      if (alreadyThere.forall(q => callGraph.canOverlap(p, q))) {
        pointerMap(addr) += p
        return addr
      }
    }
    pointers match {
      case Nil =>
        ErrorReporting.fatal("Out of zero-page memory")
      case next :: rest =>
        pointers = rest
        onEachByte(next)
        onEachByte(next + 1)
        pointerMap(next) = Set(p)
        next
    }
  }

  def allocateByte(callGraph: CallGraph, p: VariableVertex, options: CompilationOptions): Int = allocateBytes(callGraph, p, options, 1)

  def allocateBytes(callGraph: CallGraph, p: VariableVertex, options: CompilationOptions, count: Int): Int = {
    if (!variableMap.contains(count)) {
      variableMap(count) = mutable.Map()
    }
    variableMap(count).foreach { case (a, alreadyThere) =>
      if (alreadyThere.forall(q => callGraph.canOverlap(p, q))) {
        variableMap(count)(a) += p
        return a
      }
    }
    val addr = bytes.allocateBytes(count, options)
    (addr to (addr + count)).foreach(onEachByte)
    variableMap(count)(addr) = Set(p)
    addr
  }

  def notifyAboutEndOfCode(org: Int): Unit = bytes.notifyAboutEndOfCode(org)
}
