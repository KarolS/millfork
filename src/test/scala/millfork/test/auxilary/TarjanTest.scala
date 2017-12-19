package millfork.test.auxilary

import millfork.Tarjan

/**
  * @author Karol Stasiak
  */
object TarjanTest {
  def main(s: Array[String]): Unit = {
    println(Tarjan.sort(
      List(1, 2, 3, 4, 5, 6, 7, 8),
      List(
        1 -> 2,
        2 -> 3,
        3 -> 1,
        4 -> 2,
        4 -> 3,
        4 -> 5,
        5 -> 4,
        5 -> 6,
        6 -> 3,
        6 -> 7,
        7 -> 6,
        8 -> 8,
        8 -> 5,
        8 -> 7
      )
    ))
  }
}
