package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuUnoptimizedCrossPlatformRun
import org.scalactic.Prettifier
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ForLoopSecondSuite extends FunSuite with Matchers with AppendedClues {

  sealed trait IterationBound

  case class ViaVar(i: Int) extends IterationBound {
    override def toString: String = s"v(=$i)"
  }

  case class ViaConst(i: Int) extends IterationBound {
    override def toString: String = i.toString
  }


  def subcase(typ: String, from: IterationBound, dir: String, to: IterationBound, expectedCount: Int)(platforms: Cpu.Value*)(implicit pos: org.scalactic.source.Position, prettifier: Prettifier): Unit = {
    val params = (from, to) match {
      case (ViaVar(s), ViaVar(e)) => s"$typ s, $typ e"
      case (ViaVar(s), ViaConst(e)) => s"$typ s"
      case (ViaConst(s), ViaVar(e)) => s"$typ e"
      case (ViaConst(s), ViaConst(e)) => ""
    }
    val args = (from, to) match {
      case (ViaVar(s), ViaVar(e)) => s"$s, $e"
      case (ViaVar(s), ViaConst(_)) => s"$s"
      case (ViaConst(_), ViaVar(e)) => s"$e"
      case (ViaConst(_), ViaConst(_)) => ""
    }
    val start = from match {
      case ViaVar(_) => "s"
      case ViaConst(s) => s"$s"
    }
    val end = to match {
      case ViaVar(_) => "e"
      case ViaConst(e) => s"$e"
    }
    val src =
      s"""
         | word output @0xc000
         | void main () {
         |   output = 0
         |   loopy($args)
         | }
         | noinline void loopy($params) {
         |   $typ i
         |   for i,$start,$dir,$end { output += 1}
         | }
    """.stripMargin
    EmuUnoptimizedCrossPlatformRun(platforms: _*)(src) { m =>
      m.readWord(0xc000) should equal(expectedCount) withClue s"(for i,$from,$dir,$to)"
    }
  }
  def byteSubcase(from: IterationBound, dir: String, to: IterationBound, expectedCount: Int)(implicit pos: org.scalactic.source.Position, prettifier: Prettifier): Unit = {
    subcase("byte", from, dir, to, expectedCount)(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)
  }
  def wordSubcase(from: IterationBound, dir: String, to: IterationBound, expectedCount: Int)(implicit pos: org.scalactic.source.Position, prettifier: Prettifier): Unit = {
    subcase("word", from, dir, to, expectedCount)(Cpu.Mos, Cpu.Z80/*, Cpu.Motorola6809*/)
  }

  test("Basic iteration count test") {
    byteSubcase(ViaConst(1), "to", ViaConst(3), 3)
    byteSubcase(ViaVar(1), "to", ViaConst(3), 3)
    byteSubcase(ViaConst(1), "to", ViaVar(3), 3)
    byteSubcase(ViaVar(1), "to", ViaVar(3), 3)
    byteSubcase(ViaConst(1), "until", ViaConst(3), 2)
    byteSubcase(ViaVar(1), "until", ViaConst(3), 2)
    byteSubcase(ViaConst(1), "until", ViaVar(3), 2)
    byteSubcase(ViaVar(1), "until", ViaVar(3), 2)
    byteSubcase(ViaConst(11), "downto", ViaConst(3), 9)
    byteSubcase(ViaVar(11), "downto", ViaConst(3), 9)
    byteSubcase(ViaConst(11), "downto", ViaVar(3), 9)
    byteSubcase(ViaVar(11), "downto", ViaVar(3), 9)

    wordSubcase(ViaConst(1), "to", ViaConst(3), 3)
    wordSubcase(ViaVar(1), "to", ViaConst(3), 3)
    wordSubcase(ViaConst(1), "to", ViaVar(3), 3)
    wordSubcase(ViaVar(1), "to", ViaVar(3), 3)
    wordSubcase(ViaConst(1), "until", ViaConst(3), 2)
    wordSubcase(ViaVar(1), "until", ViaConst(3), 2)
    wordSubcase(ViaConst(1), "until", ViaVar(3), 2)
    wordSubcase(ViaVar(1), "until", ViaVar(3), 2)
    wordSubcase(ViaConst(11), "downto", ViaConst(3), 9)
    wordSubcase(ViaVar(11), "downto", ViaConst(3), 9)
    wordSubcase(ViaConst(11), "downto", ViaVar(3), 9)
    wordSubcase(ViaVar(11), "downto", ViaVar(3), 9)
  }

  test("Edge case iteration count test") {
    byteSubcase(ViaConst(0), "to", ViaConst(255), 256)
    byteSubcase(ViaVar(0), "to", ViaConst(255), 256)
    byteSubcase(ViaConst(0), "to", ViaVar(255), 256)
    byteSubcase(ViaVar(0), "to", ViaVar(255), 256)
    byteSubcase(ViaConst(0), "until", ViaConst(255), 255)
    byteSubcase(ViaVar(0), "until", ViaConst(255), 255)
    byteSubcase(ViaConst(0), "until", ViaVar(255), 255)
    byteSubcase(ViaVar(0), "until", ViaVar(255), 255)
    byteSubcase(ViaConst(255), "downto", ViaConst(0), 256)
    byteSubcase(ViaVar(255), "downto", ViaConst(0), 256)
    byteSubcase(ViaConst(255), "downto", ViaVar(0), 256)
    byteSubcase(ViaVar(255), "downto", ViaVar(0), 256)
  }

  test("Empty case iteration count test") {
    byteSubcase(ViaConst(5), "until", ViaConst(5), 0)
    byteSubcase(ViaVar(5), "until", ViaConst(5), 0)
    byteSubcase(ViaConst(5), "until", ViaVar(5), 0)
    byteSubcase(ViaVar(5), "until", ViaVar(5), 0)

    wordSubcase(ViaConst(5), "until", ViaConst(5), 0)
    wordSubcase(ViaVar(5), "until", ViaConst(5), 0)
    wordSubcase(ViaConst(5), "until", ViaVar(5), 0)
    wordSubcase(ViaVar(5), "until", ViaVar(5), 0)
  }

  test("Wrong directions in 'until' cases") {
    byteSubcase(ViaConst(5), "until", ViaConst(4), 255)
    byteSubcase(ViaVar(5), "until", ViaConst(4), 255)
    byteSubcase(ViaConst(5), "until", ViaVar(4), 255)
    byteSubcase(ViaVar(5), "until", ViaVar(4), 255)

    wordSubcase(ViaConst(65000), "until", ViaConst(0), 536)
    wordSubcase(ViaVar(65000), "until", ViaConst(0), 536)
    wordSubcase(ViaConst(65000), "until", ViaVar(0), 536)
    wordSubcase(ViaVar(65000), "until", ViaVar(0), 536)

    wordSubcase(ViaConst(65000), "until", ViaConst(1), 537)
    wordSubcase(ViaVar(65000), "until", ViaConst(1), 537)
    wordSubcase(ViaConst(65000), "until", ViaVar(1), 537)
    wordSubcase(ViaVar(65000), "until", ViaVar(1), 537)
  }

  test("Wrong directions in 'to' cases") {
    byteSubcase(ViaConst(1), "to", ViaConst(0), 256)
    byteSubcase(ViaVar(1), "to", ViaConst(0), 256)
    byteSubcase(ViaConst(1), "to", ViaVar(0), 256)
    byteSubcase(ViaVar(1), "to", ViaVar(0), 256)

    wordSubcase(ViaConst(65000), "to", ViaConst(0), 537)
    wordSubcase(ViaVar(65000), "to", ViaConst(0), 537)
    wordSubcase(ViaConst(65000), "to", ViaVar(0), 537)
    wordSubcase(ViaVar(65000), "to", ViaVar(0), 537)
  }

  test("Wrong directions in 'downto' cases") {
    byteSubcase(ViaConst(1), "downto", ViaConst(2), 256)
    byteSubcase(ViaVar(1), "downto", ViaConst(2), 256)
    byteSubcase(ViaConst(1), "downto", ViaVar(2), 256)
    byteSubcase(ViaVar(1), "downto", ViaVar(2), 256)

    wordSubcase(ViaConst(1000), "downto", ViaConst(65000), 1537)
    wordSubcase(ViaVar(1000), "downto", ViaConst(65000), 1537)
    wordSubcase(ViaConst(1000), "downto", ViaVar(65000), 1537)
    wordSubcase(ViaVar(1000), "downto", ViaVar(65000), 1537)
  }
}
