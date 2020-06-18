package millfork.test.emu

/**
  * @author Karol Stasiak
  */
object Settings {

  /**
    * Should the Intel 8086 tests be enabled?
    * Intel 8086 tests:
    * - are slow
    * – leak memory
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

  /**
    * Should the Ricoh tests be enabled?
    * Ricoh tests:
    * - don't differ from 6502 tests a lot
    * – are reasonably unfrequentE
    * – are fast
    * so they are enabled by default
    */
  val enableRicohTests: Boolean = true

  /**
    * Should tests that don't run emulated code be enabled?
    * Unemulated tests:
    * – test only that code generation doesn't crash
    * – usually cover platforms similar to emulated ones
    * – are reasonably fast
    * so they are enabled by default
    */
  val enableUnemulatedTests: Boolean = true

  /**
    * Should the Motorola 6809 tests be enabled?
    * Motorola 6809 emulation is currently under development and more and more tests are ran against it.
    */
  val enableMotorola6809Tests: Boolean = true

  // the following are the core platforms and they are all tested by default

  val enable6502Tests: Boolean = true

  val enable65C02Tests: Boolean = true

  val enableIntel8080Tests: Boolean = true

  val enableZ80Tests: Boolean = true

  val enableGameboyTests: Boolean = true

}
