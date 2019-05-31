package millfork.test.emu

/**
  * @author Karol Stasiak
  */
object Settings {

  /**
    * Should the Intel 8086 tests be enabled?
    * Intel 8086 tests:
    * - are slow
    * - don't work on headless JRE's
    * – open annoying windows on graphical JRE's
    * – test only the 8080-to-8086 translation
    * – are of low priority
    * so they are disabled by default
    */
  val enableIntel8086Tests: Boolean = false

  /**
    * Should the WDC65816 tests be enabled?
    * WDC65816 tests:
    * - are slow
    * - require Nashorn
    * – are of low priority
    * so they are disabled by default
    */
  val enableWdc85816Tests: Boolean = false

}
