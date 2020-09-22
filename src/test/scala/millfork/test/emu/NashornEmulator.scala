package millfork.test.emu

import java.nio.file.{Files, Paths}

import org.graalvm.polyglot.{Context, Value}

import scala.language.dynamics

/**
  * Despite its name, it no longer uses Nashorn, but GraalJS instead.
  * @author Karol Stasiak
  */
object NashornEmulator {

  lazy val engine: Context = {
    val jsFile = Paths.get(classOf[Nothing].getResource("/cpu.js").toURI)
    import org.graalvm.polyglot.Context
    val polyglot = Context.create()
    polyglot.eval("js", new String(Files.readAllBytes(jsFile)))
    polyglot
  }

  private def newCpu(): JsObject = {
    JsObject(engine.eval("js", "CPU_65816")).construct()
  }

  def run(memory: Array[Byte], steps: Long, start: Int): (Long, Array[Byte]) = {
    val hex = memory.map(b => f"${b&0xff}%02x").mkString("")
    val cpu = newCpu()
    cpu.reset()
    cpu.load_binary(memory.map(_ & 0xff), 0)
    val memory0 = cpu.mmu.memory.!("0")
    for (i <- 0 until 1 << 16) memory0.!(i.toString, memory(i) & 0xff)
    cpu.r.pc = start
    cpu.r.sp = 0x1ff
    cpu.r.k = 0
    var count = 0L
    while (count < steps && cpu.r.s.toInt.&(0xff) > 1) {
      cpu.step()
      count += 1
    }
    val newMemory = (0 until 1 << 16).map(i => memory0.!(i.toString).toByte).toArray
    val cycles = cpu.cycle_count.toLong
    cycles -> newMemory
  }

  def main(args: Array[String]): Unit = {
    val cpu = newCpu()
    cpu.reset()
    cpu.load_binary("00", 0x200)
    cpu.execute(0x200)
    println(cpu.r.pc)
  }
}

case class JsObject(private val mirror: Value) extends Dynamic {

  def !(index: Long): JsObject = {
    if (mirror.hasArrayElements) return JsObject(mirror.getArrayElement(index))
    else throw new IllegalArgumentException(s"Accessing index $index of $getUnderlyingClass")
  }

  def !(index: String): JsObject = {
    if (mirror.hasMembers) return JsObject(mirror.getMember(index))
    else throw new IllegalArgumentException(s"Accessing field $index of $getUnderlyingClass")
  }

  def !(index: String, value: JsObject): Unit = {
    if (mirror.hasMembers) mirror.putMember(index, value.mirror)
    else throw new IllegalArgumentException(s"Setting field $index of $getUnderlyingClass")
  }

  def !(index: String, value:Any): Unit = {
    if (mirror.hasMembers) mirror.putMember(index, value)
    else throw new IllegalArgumentException(s"Setting field $index of $getUnderlyingClass")
  }

  def !(index: Long, value: JsObject): Unit = {
    if (mirror.hasArrayElements) mirror.setArrayElement(index, value.mirror)
    else throw new IllegalArgumentException(s"Setting field $index of $getUnderlyingClass")
  }

  def !(index: Long, value:Any): Unit = {
    if (mirror.hasArrayElements) mirror.setArrayElement(index, value)
    else throw new IllegalArgumentException(s"Setting field $index of $getUnderlyingClass")
  }

  def selectDynamic(name: String): JsObject = {
    if (mirror.hasMembers) return JsObject(mirror.getMember(name))
    else throw new IllegalArgumentException(s"Accessing field $name of $getUnderlyingClass")
  }

  private def getUnderlyingClass = {
    if (mirror.asInstanceOf[AnyRef] eq null) "null" else mirror.getClass.getSimpleName
  }

  def updateDynamic(name: String)(value: Any): Unit = {
    if (mirror.hasMembers) {
      value match {
        case o:JsObject =>
          mirror.putMember(name, o.mirror)
        case _ =>
          mirror.putMember(name, value)
      }
    }
    else throw new IllegalArgumentException(s"Setting field $name of $getUnderlyingClass")
  }

  def applyDynamic(name: String)(params: Any*): JsObject = {
    if (mirror.canInvokeMember(name)) JsObject(mirror.invokeMember(name, params.toArray))
    else throw new IllegalArgumentException(s"Invoking method $name of $getUnderlyingClass")
  }

  def construct(params: Any*): JsObject = {
    if (mirror.canInstantiate) JsObject(mirror.newInstance(params.toArray))
    else throw new IllegalArgumentException(s"Using $getUnderlyingClass as a constructor")
  }

  def toInt: Int = mirror.asInt()

  def toLong: Long = mirror.asLong()

  def toByte: Byte = mirror.asInt().toByte

  override def toString: String = String.valueOf(mirror)
}
