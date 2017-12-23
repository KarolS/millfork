package millfork.test.auxilary

import millfork.NonOverlappingIntervals

/**
  * @author Karol Stasiak
  */
object NonOverlappingIntervalsTest {
  def main(args: Array[String]): Unit = {
    NonOverlappingIntervals.apply[(Int, Int)](
      List(0 -> 3, 1 -> 2, 2 -> 3, 0 -> 2, 1 -> 4),
      _._1,
      _._2
    ).map(_.toSeq.sorted).foreach(println)
  }
}
