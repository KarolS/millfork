package millfork.output

/**
  * @author Karol Stasiak
  */
case class FormattableLabel(
                             labelName: String,
                             bankName: String,
                             bankNumber: Int,
                             startValue: Int,
                             endValue: Option[Int],
                             category: Char,
                             mesenSymbol: Char
                           )
