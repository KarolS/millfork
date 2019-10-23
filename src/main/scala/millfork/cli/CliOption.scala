package millfork.cli

/**
  * @author Karol Stasiak
  */
trait CliOption[T, O <: CliOption[T, O]] {
  this: O =>
  def toStrings(firstTab: Int): List[String] = {
    @inline
    def indent(s: String):String = "".padTo(firstTab, ' ') + s
    val fl = firstLine
    if (_description == "") {
      return List(fl)
    }
    val descriptionLines = _description.split("\n")
    if (fl.length < firstTab) {
      (fl.padTo(firstTab, ' ') + descriptionLines.head) :: descriptionLines.tail.map(indent).toList
    } else {
      fl :: descriptionLines.map(indent).toList
    }
  }

  protected def firstLine: String = names.mkString(" | ")

  def names: Seq[String]

  private[cli] def length: Int

  private[cli] val _shortName: String
  private[cli] var _description: String = ""
  private[cli] var _hidden = false
  private[cli] var _dummy = false
  private[cli] var _maxEncounters = 1
  private[cli] var _minEncounters = 0
  private[cli] var _actualEncounters = 0
  private[cli] var _onTooFew: Option[Int => Unit] = None
  private[cli] var _onTooMany: Option[Int => Unit] = None

  def validate(): Boolean = {
    var ok = true
    if (_actualEncounters < _minEncounters) {
      _onTooFew.fold(throw new IllegalArgumentException(s"Too few ${_shortName} options: required ${_minEncounters}, given ${_actualEncounters}"))(_ (_actualEncounters))
      ok = false
    }
    if (_actualEncounters > _maxEncounters) {
      _onTooMany.fold()(_ (_actualEncounters))
      ok = false
    }
    ok
  }

  def onWrongNumber(action: Int => Unit): Unit = {
    _onTooFew = Some(action)
    _onTooMany = Some(action)
  }

  def onTooFew(action: Int => Unit): Unit = {
    _onTooFew = Some(action)
  }

  def onTooMany(action: Int => Unit): Unit = {
    _onTooMany = Some(action)
  }

  def encounter(): Unit = {
    _actualEncounters += 1
  }

  def description(d: String): O = {
    _description = d
    this
  }

  def hidden(): O = {
    _hidden = true
    this
  }

  def dummy(): O = {
    _dummy = true
    this
  }

  def minCount(count: Int): O = {
    _minEncounters = count
    this
  }

  def maxCount(count: Int): O = {
    _maxEncounters = count
    this
  }

  def required(): O = minCount(1)

  def repeatable(): O = maxCount(Int.MaxValue)
}

class Fluff[T](val text: Seq[String]) extends CliOption[T, Fluff[T]] {
  this.repeatable()

  override def toStrings(firstTab: Int): List[String] = text.toList

  override def length = 0

  override val _shortName = ""

  override def names = Nil
}

class NoMoreOptions[T](val names: Seq[String]) extends CliOption[T, NoMoreOptions[T]] {
  this.repeatable()

  override def length = 1

  override val _shortName = names.head
}

class UnknownParamOption[T] extends CliOption[T, UnknownParamOption[T]] {
  this._hidden = true

  override def length = 0

  val names: Seq[String] = Nil
  private var _action: ((String, T) => T) = (_, x) => x

  def action(a: ((String, T) => T)): UnknownParamOption[T] = {
    _action = a
    this
  }

  def encounter(value: String, t: T): T = {
    encounter()
    _action(value, t)
  }

  override private[cli] val _shortName = ""
}

class FlagOption[T](val names: Seq[String]) extends CliOption[T, FlagOption[T]] {
  override def length = 1

  private var _action: (T => T) = x => x

  def action(a: (T => T)): FlagOption[T] = {
    _action = a
    this
  }

  def encounter(t: T): T = {
    encounter()
    _action(t)
  }

  override val _shortName = names.head
}

class BooleanOption[T](val trueName: String, val falseName: String) extends CliOption[T, BooleanOption[T]] {
  override def length = 1

  private var _action: ((T,Boolean) => T) = (x,_) => x

  def action(a: ((T,Boolean) => T)): BooleanOption[T] = {
    _action = a
    this
  }

  def encounter(asName: String, t: T): T = {
    encounter()
    if (asName == trueName) {
      return _action(t, true)
    }
    if (asName == falseName) {
      return _action(t, false)
    }
    t
  }

  override val _shortName = names.head

  override protected def firstLine: String = trueName + " | " + falseName

  override def names = Seq(trueName, falseName)
}

class ParamOption[T](val names: Seq[String]) extends CliOption[T, ParamOption[T]] {

  override protected def firstLine: String = names.mkString(" | ") + " " + _paramPlaceholder

  override def length = 2

  private var _action: ((String, T) => T) = (_, x) => x
  private var _paramPlaceholder: String = "<x>"

  def placeholder(p: String): ParamOption[T] = {
    _paramPlaceholder = p
    this
  }

  def action(a: ((String, T) => T)): ParamOption[T] = {
    _action = a
    this
  }

  def encounter(value: String, t: T): T = {
    encounter()
    _action(value, t)
  }

  override val _shortName = names.head
}

class ExpansionOption[T](val name: String)(val replacements: List[String]) extends CliOption[T, ExpansionOption[T]] {
  override def names: Seq[String] = Seq(name)

  override private[cli] def length = 1

  override private[cli] val _shortName = name
}