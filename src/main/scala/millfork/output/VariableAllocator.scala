package millfork.output

import millfork.error.ErrorReporting
import millfork.node.{CallGraph, VariableVertex}
import millfork.{CompilationFlag, CompilationOptions}

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * @author Karol Stasiak
  */

object AllocationLocation extends Enumeration {
  val Zeropage, High, Either = Value
  def matches(addr:Int, location: AllocationLocation.Value): Boolean = location match {
    case Zeropage => addr < 0x100
    case High => addr >= 0x100
    case Either => true
  }
}

sealed trait ByteAllocator {
  def startAt: Int
  def endBefore: Int

  def preferredOrder: Option[List[Int]]

  def notifyAboutEndOfCode(org: Int): Unit

  def findFreeBytes(mem: MemoryBank, count: Int, options: CompilationOptions): Int = {
    var lastFree = startAt
    var counter = 0
    val occupied = mem.occupied
    for(i <- preferredOrder.getOrElse(startAt until endBefore)) {
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
    -1
  }
}

class UpwardByteAllocator(val startAt: Int, val endBefore: Int) extends ByteAllocator {
  def notifyAboutEndOfCode(org: Int): Unit = ()
  override def preferredOrder: Option[List[Int]] = None
}

class ZeropageAllocator(val freeZpPointers: List[Int]) extends ByteAllocator {

  def notifyAboutEndOfCode(org: Int): Unit = ()
  override def preferredOrder: Option[List[Int]] = if (freeZpPointers.isEmpty) None else Some(freeZpPointers.flatMap(i => Seq(i,i+1)))

  override def startAt: Int = if (freeZpPointers.isEmpty) 2 else freeZpPointers.min

  override def endBefore: Int = if (freeZpPointers.isEmpty) 2 else freeZpPointers.max + 2
}

class AfterCodeByteAllocator(val endBefore: Int) extends ByteAllocator {
  var startAt = 0x200
  def notifyAboutEndOfCode(org: Int): Unit = startAt = org

  override def preferredOrder: Option[List[Int]] = None
}

class VariableAllocator(pointers: List[Int], private val bytes: ByteAllocator) {

  val zeropage: ByteAllocator = new ZeropageAllocator(pointers)

  private val variableMap = mutable.Map[Int, mutable.Map[Int, Set[VariableVertex]]]()

  def allocateBytes(mem: MemoryBank, callGraph: CallGraph, p: VariableVertex, options: CompilationOptions, count: Int, initialized: Boolean, writeable: Boolean, location: AllocationLocation.Value): Int = {
    if (!variableMap.contains(count)) {
      variableMap(count) = mutable.Map()
    }
    variableMap(count).foreach { case (a, alreadyThere) =>
      if (AllocationLocation.matches(a, location) && alreadyThere.forall(q => callGraph.canOverlap(p, q))) {
        variableMap(count)(a) += p
        return a
      }
    }
    val addr = allocateBytes(mem, options, count, initialized, writeable, location)
    variableMap(count)(addr) = Set(p)
    addr
  }

  def tryAllocateZeropageBytes(mem: MemoryBank, callGraph: CallGraph, p: VariableVertex, options: CompilationOptions, count: Int): Option[Int]={
    if (!variableMap.contains(count)) {
      variableMap(count) = mutable.Map()
    }
    variableMap(count).foreach { case (a, alreadyThere) =>
      if (a < 0x100 && alreadyThere.forall(q => callGraph.canOverlap(p, q))) {
        variableMap(count)(a) += p
        return Some(a)
      }
    }
    val addr = zeropage.findFreeBytes(mem, count, options)
    if (addr < 0) None else {
      markBytes(mem, addr, count, initialized = false, writeable = true)
      Some(addr)
    }
  }

  def allocateBytes(mem: MemoryBank, options: CompilationOptions, count: Int, initialized: Boolean, writeable: Boolean, location: AllocationLocation.Value): Int = {
    val addr = location match {
      case AllocationLocation.Zeropage =>
        val a = zeropage.findFreeBytes(mem, count, options)
        if (a < 0) {
          ErrorReporting.fatal("Out of zeropage memory")
        }
        a
      case AllocationLocation.High =>
        val a = bytes.findFreeBytes(mem, count, options)
        if (a < 0) {
          ErrorReporting.fatal("Out of high memory")
        }
        a
      case AllocationLocation.Either =>
        var a = zeropage.findFreeBytes(mem, count, options)
        if (a < 0) {
          a = bytes.findFreeBytes(mem, count, options)
          if (a < 0) {
            ErrorReporting.fatal("Out of high memory")
          }
        }
        a
    }
    markBytes(mem, addr, count, initialized, writeable)
    addr
  }

  private def markBytes(mem: MemoryBank, addr: Int, count: Int, initialized: Boolean, writeable: Boolean): Unit = {
    ErrorReporting.trace(s"allocating $count bytes at $$${addr.toHexString}")
    (addr until (addr + count)).foreach { i =>
      if (mem.occupied(i)) ErrorReporting.fatal("Overlapping objects")
      mem.readable(i) = true
      mem.occupied(i) = true
      mem.initialized(i) = initialized
      mem.writeable(i) = writeable
    }
  }

  def notifyAboutEndOfCode(org: Int): Unit = bytes.notifyAboutEndOfCode(org)
}
