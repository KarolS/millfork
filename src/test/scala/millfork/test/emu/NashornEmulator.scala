package millfork.test.emu

import javax.script.{Bindings, ScriptEngine, ScriptEngineManager}
import java.io.FileReader
import java.nio.file.Paths

import jdk.nashorn.api.scripting.ScriptObjectMirror

import scala.language.dynamics

/**
  * @author Karol Stasiak
  */
object NashornEmulator {

  lazy val engine: ScriptEngine = {
    val jsFile = Paths.get(classOf[Nothing].getResource("/cpu.js").toURI).toFile
    val engine: ScriptEngine = new ScriptEngineManager().getEngineByName("nashorn")
    engine.eval(new FileReader(jsFile))
    engine
  }

  private def newCpu(): JsObject = {
    JsObject(engine.get("CPU_65816").asInstanceOf[ScriptObjectMirror]).construct()
  }

  def run(memory: Array[Byte], steps: Long, start: Int): (Long, Array[Byte]) = {
    val hex = memory.map(b => f"${b&0xff}%02x").mkString("")
    val cpu = newCpu()
    cpu.reset()
    cpu.load_binary(memory.map(_ & 0xff), 0)
    val memory0 = cpu.mmu.memory.!("0")
    for (i <- 0 until 1 << 16) memory0.!(i.toString, memory(i) & 0xff)
    cpu.r.pc = start
    cpu.r.k = 0
    var count = 0L
    while (count < steps && cpu.r.s.*[Number].intValue().&(0xff) > 1) {
      cpu.step()
      count += 1
    }
    val newMemory = (0 until 1 << 16).map(i => memory0.!(i.toString).*[Number].byteValue()).toArray
    val cycles = cpu.cycle_count.*[Number].longValue()
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

case class JsObject(private val mirror: Any) extends Dynamic {


  def !(index: JsObject): JsObject =
    mirror match {
      case x: ScriptObjectMirror => JsObject(x.get(index.mirror))
      case _ => throw new IllegalArgumentException(s"Accessing field $index of $getUnderlyingClass")
    }

  def !(index: Any): JsObject =
    mirror match {
      case x: ScriptObjectMirror => JsObject(x.get(index))
      case _ => throw new IllegalArgumentException(s"Accessing field $index of $getUnderlyingClass")
    }

  def !(index: String, value:Any): Unit =
    mirror match {
      case x: ScriptObjectMirror => x.setMember(index, value)
      case _ => throw new IllegalArgumentException(s"Setting field $index of $getUnderlyingClass")
    }

  def selectDynamic(name: String): JsObject =
    mirror match {
      case x: ScriptObjectMirror => JsObject(x.get(name).asInstanceOf[Any] match {
        case y: Double => if (y.isValidInt) y.toInt else y
        case y => y
      })
      case _ => throw new IllegalArgumentException(s"Accessing field $name of $getUnderlyingClass")
    }

  private def getUnderlyingClass = {
    if (mirror.asInstanceOf[AnyRef] eq null) "null" else mirror.getClass.getSimpleName
  }

  def updateDynamic(name: String)(value: Any): Unit =
    mirror match {
      case x: ScriptObjectMirror => x.setMember(name, value)
      case _ => throw new IllegalArgumentException(s"Setting field $name of $getUnderlyingClass")
    }

  def applyDynamic(name: String)(params: Any*): JsObject =
    mirror match {
      case x: ScriptObjectMirror => JsObject(x.callMember(name, params.toArray))
      case _ => throw new IllegalArgumentException(s"Accessing field $name of $getUnderlyingClass")
    }

  def construct(params: Any*): JsObject =
    mirror match {
      case x: ScriptObjectMirror => JsObject(x.newObject(params.toArray))
      case _ => throw new IllegalArgumentException(s"Using $getUnderlyingClass as a constructor")
    }

  @inline
  def *[T]: T = mirror.asInstanceOf[T]

  override def toString: String = String.valueOf(mirror)
}
