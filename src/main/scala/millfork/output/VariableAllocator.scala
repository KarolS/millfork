package millfork.output

import millfork.error.{ConsoleLogger, Logger}
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
  def heapStart: Int

  def preferredOrder: Option[List[Int]]

  def notifyAboutEndOfCode(org: Int): Unit

  def findFreeBytes(mem: MemoryBank, count: Int, options: CompilationOptions, alignment: MemoryAlignment): Int = {
    var lastFree = startAt
    var counter = 0
    val occupied = mem.occupied
    var previous = -800
    for(i <- preferredOrder.getOrElse(startAt until endBefore)) {
      if (occupied(i)) {
        counter = 0
      } else if (counter == 0 && (alignment match {
        case WithinPageAlignment => count <= 256 && i.&(0xff00) != i.+(count - 1).&(0xff00)
        case DivisibleAlignment(divisor) => i % divisor != 0
        case NoAlignment => false
      })) {
        counter = 0
      } else {
        if (previous != i - 1) {
          counter = 0
        }
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
      previous = i
    }
    -1
  }
}

class UpwardByteAllocator(val startAt: Int, val endBefore: Int) extends ByteAllocator {
  var heapStart: Int = startAt
  def notifyAboutEndOfCode(org: Int): Unit = {
    heapStart = org
  }
  override def preferredOrder: Option[List[Int]] = None
}

class ZeropageAllocator(val freeZpBytes: List[Int]) extends ByteAllocator {

  def notifyAboutEndOfCode(org: Int): Unit = ()
  override def preferredOrder: Option[List[Int]] = if (freeZpBytes.isEmpty) None else Some(freeZpBytes)

  override def startAt: Int = if (freeZpBytes.isEmpty) 2 else freeZpBytes.min

  override def endBefore: Int = if (freeZpBytes.isEmpty) 2 else freeZpBytes.max + 1

  override def heapStart: Int = 0
}

class AfterCodeByteAllocator(startIfNoCode: Int, val endBefore: Int) extends ByteAllocator {
  var startAt = startIfNoCode
  def notifyAboutEndOfCode(org: Int): Unit = startAt = org

  override def preferredOrder: Option[List[Int]] = None

  override def heapStart: Int = startAt
}

class VariableAllocator(zpBytes: List[Int], private val bytes: ByteAllocator) {

  def startAt: Int = bytes.startAt
  def endBefore: Int = bytes.endBefore

  def totalHimemSize: Int = bytes.endBefore - bytes.startAt

  val zeropage: ByteAllocator = new ZeropageAllocator(zpBytes)

  private val variableMap = mutable.Map[Int, mutable.Map[Int, Set[VariableVertex]]]()

  var heapStart: Int = bytes.heapStart

  def allocateBytes(mem: MemoryBank, callGraph: CallGraph, p: VariableVertex, options: CompilationOptions, count: Int, initialized: Boolean, writeable: Boolean, location: AllocationLocation.Value, alignment: MemoryAlignment): Int = {
    if (!variableMap.contains(count)) {
      variableMap(count) = mutable.Map()
    }
    variableMap(count).foreach { case (a, alreadyThere) =>
      if (AllocationLocation.matches(a, location) && alreadyThere.forall(q => callGraph.canOverlap(p, q))) {
        variableMap(count)(a) += p
        return a
      }
    }
    val addr = allocateBytes(mem, options, count, initialized, writeable, location, alignment)
    variableMap(count)(addr) = Set(p)
    addr
  }

  def tryAllocateZeropageBytes(mem: MemoryBank, callGraph: CallGraph, p: VariableVertex, options: CompilationOptions, count: Int, alignment: MemoryAlignment): Option[Int]={
    if (!variableMap.contains(count)) {
      variableMap(count) = mutable.Map()
    }
    variableMap(count).foreach { case (a, alreadyThere) =>
      if (a < 0x100 && alreadyThere.forall(q => callGraph.canOverlap(p, q))) {
        variableMap(count)(a) += p
        return Some(a)
      }
    }
    val addr = zeropage.findFreeBytes(mem, count, options, alignment)
    if (addr < 0) None else {
      markBytes(options.log, mem, addr, count, initialized = false, writeable = true)
      Some(addr)
    }
  }

  def allocateBytes(mem: MemoryBank, options: CompilationOptions, count: Int, initialized: Boolean, writeable: Boolean, location: AllocationLocation.Value, alignment: MemoryAlignment): Int = {
    val addr = if (options.platform.hasZeroPage) {
      location match {
        case AllocationLocation.Zeropage =>
          val a = zeropage.findFreeBytes(mem, count, options, alignment)
          if (a < 0) {
            options.log.fatal("Out of zeropage memory")
          }
          a
        case AllocationLocation.High =>
          val a = bytes.findFreeBytes(mem, count, options, alignment)
          if (a < 0) {
            options.log.fatal("Out of high memory in bank ${mem.index}")
          }
          a
        case AllocationLocation.Either =>
          var a = zeropage.findFreeBytes(mem, count, options, alignment)
          if (a < 0) {
            a = bytes.findFreeBytes(mem, count, options, alignment)
            if (a < 0) {
              options.log.fatal("Out of high memory in bank ${mem.index}")
            }
          }
          a
      }
    } else {
      val a = bytes.findFreeBytes(mem, count, options, alignment)
      if (a < 0) {
        options.log.fatal(s"Out of high memory in bank ${mem.index}")
      }
      a
    }
    markBytes(options.log, mem, addr, count, initialized, writeable)
    heapStart = heapStart max (addr + count)
    addr
  }

  private def markBytes(log: Logger, mem: MemoryBank, addr: Int, count: Int, initialized: Boolean, writeable: Boolean): Unit = {
    log.trace(s"allocating $count bytes at $$${addr.toHexString}")
    (addr until (addr + count)).foreach { i =>
      if (mem.occupied(i)) log.fatal("Overlapping objects")
      mem.readable(i) = true
      mem.occupied(i) = true
      mem.initialized(i) = initialized
      mem.writeable(i) = writeable
    }
  }

  //TODO: Everything about the three methods below is ugly and wrong. Fix later.

  def notifyAboutEndOfCode(org: Int): Unit = {
    heapStart = heapStart max org
    bytes.notifyAboutEndOfCode(org)
  }
  def notifyAboutEndOfData(org: Int): Unit = {
    heapStart = heapStart max org
  }

  def notifyAboutHole(mem: MemoryBank, addr: Int, size: Int): Unit = {
    if (Math.abs(addr - heapStart) <= 1) {
      heapStart += size
      while (mem.occupied(heapStart)) heapStart += 1
      bytes.notifyAboutEndOfCode(heapStart)
    }
  }
}
