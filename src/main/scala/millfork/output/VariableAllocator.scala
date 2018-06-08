package millfork.output

import millfork.error.ErrorReporting
import millfork.node.{CallGraph, VariableVertex}
import millfork.{CompilationFlag, CompilationOptions}

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * @author Karol Stasiak
  */

sealed trait ByteAllocator {
  protected def startAt: Int
  protected def endBefore: Int

  def notifyAboutEndOfCode(org: Int): Unit

  def findFreeBytes(mem: MemoryBank, count: Int, options: CompilationOptions): Int = {
    var lastFree = startAt
    var counter = 0
    val occupied = mem.occupied
    for(i <- startAt until endBefore) {
      if (occupied(i) || counter == 0 && count == 2 && i.&(0xff) == 0xff && options.flags(CompilationFlag.PreventJmpIndirectBug)) {
        counter = 0
      } else {
        if (counter == 0) {
          lastFree = i
        }
        if (count == 0) {
          return lastFree
        }
        counter += 1
        if (counter == count) {
          return lastFree
        }
      }
    }
    ErrorReporting.fatal("Out of high memory")
  }
}

class UpwardByteAllocator(val startAt: Int, val endBefore: Int) extends ByteAllocator {
  def notifyAboutEndOfCode(org: Int): Unit = ()
}

class AfterCodeByteAllocator(val endBefore: Int) extends ByteAllocator {
  var startAt = 0x200
  def notifyAboutEndOfCode(org: Int): Unit = startAt = org
}

class VariableAllocator(val pointers: List[Int], private val bytes: ByteAllocator) {

  private val pointerMap = mutable.Map[Int, Set[VariableVertex]]()
  private val variableMap = mutable.Map[Int, mutable.Map[Int, Set[VariableVertex]]]()

  def allocatePointer(mem: MemoryBank, callGraph: CallGraph, p: VariableVertex): Int = {
    // TODO: search for free zeropage locations
    pointerMap.foreach { case (addr, alreadyThere) =>
      if (alreadyThere.forall(q => callGraph.canOverlap(p, q))) {
        pointerMap(addr) += p
        return addr
      }
    }
    @tailrec
    def pickFreePointer(ps: List[Int]): Int =
      ps match {
        case Nil =>
          ErrorReporting.trace(pointerMap.mkString(", "))
          ErrorReporting.fatal("Out of zero-page memory")
        case next :: rest =>
          if (mem.occupied(next) || mem.occupied(next + 1)) {
            pickFreePointer(rest)
          } else {
            mem.readable(next) = true
            mem.readable(next + 1) = true
            mem.occupied(next) = true
            mem.occupied(next + 1) = true
            mem.writeable(next) = true
            mem.writeable(next + 1) = true
            pointerMap(next) = Set(p)
            next
          }
      }
    pickFreePointer(pointers)
  }

  def allocateBytes(mem: MemoryBank, callGraph: CallGraph, p: VariableVertex, options: CompilationOptions, count: Int, initialized: Boolean, writeable: Boolean): Int = {
    if (!variableMap.contains(count)) {
      variableMap(count) = mutable.Map()
    }
    variableMap(count).foreach { case (a, alreadyThere) =>
      if (alreadyThere.forall(q => callGraph.canOverlap(p, q))) {
        variableMap(count)(a) += p
        return a
      }
    }
    val addr = allocateBytes(mem, options, count, initialized, writeable)
    variableMap(count)(addr) = Set(p)
    addr
  }

  def allocateBytes(mem: MemoryBank, options: CompilationOptions, count: Int, initialized: Boolean, writeable: Boolean): Int = {
    val addr = bytes.findFreeBytes(mem, count, options)
    ErrorReporting.trace(s"allocating $count bytes at $$${addr.toHexString}")
    (addr until (addr + count)).foreach { i =>
      if (mem.occupied(i)) ErrorReporting.fatal("Overlapping objects")
      mem.readable(i) = true
      mem.occupied(i) = true
      mem.initialized(i) = initialized
      mem.writeable(i) = writeable
    }
    addr
  }

  def notifyAboutEndOfCode(org: Int): Unit = bytes.notifyAboutEndOfCode(org)
}
