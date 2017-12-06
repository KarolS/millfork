package millfork.cli

import fastparse.core.Parsed.Failure

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
class CliParser[T] {

  private val options = mutable.ArrayBuffer[CliOption[T, _]]()
  private val mapFlags = mutable.Map[String, CliOption[T, _]]()
  private val mapOptions = mutable.Map[String, CliOption[T, _]]()
  private val _default = new UnknownParamOption[T]().action((p, _) => throw new IllegalArgumentException(s"Unknown option $p"))
  private var _status: Option[CliStatus.Value] = None
  options += _default

  private def add[O <: CliOption[T, _]](o: O) = {
    options += o
    o.length match {
      case 1 =>
        o.names.foreach { n => mapFlags(n) = o }
      case 2 =>
        o.names.foreach { n => mapOptions(n) = o }
      case _ => ()
    }
    o
  }

  def parse(context: T, args: List[String]): (CliStatus.Value, T) = {
    val t = parseInner(context, args)
    _status.getOrElse(if (options.forall(_.validate())) CliStatus.Ok else CliStatus.Failed) -> t
  }

  def assumeStatus(s: CliStatus.Value): Unit = {
    _status = Some(s)
  }

  private def parseInner(context: T, args: List[String]): T = {
    args match {
      case k :: v :: xs if mapOptions.contains(k) =>
        mapOptions(k) match {
          case p: ParamOption[T] => parseInner(p.encounter(v, context), xs)
          case _ => ???
        }
      case k :: xs if mapFlags.contains(k) =>
        mapFlags(k) match {
          case p: FlagOption[T] =>
            parseInner(p.encounter(context), xs)
          case p: BooleanOption[T] =>
            parseInner(p.encounter(k, context), xs)
          case p: NoMoreOptions[T] =>
            p.encounter()
            xs.foldLeft(context)((t, x) => _default.encounter(x, t))
          case _ => ???
        }
      case x :: xs =>
        parseInner(_default.encounter(x, context), xs)
      case Nil => context
    }
  }


  def fluff(text: String*): Unit = add(new Fluff[T](text))

  def flag(names: String*): FlagOption[T] = add(new FlagOption[T](names))

  def boolean(trueName: String, falseName: String): BooleanOption[T] = add(new BooleanOption[T](trueName, falseName))

  def endOfFlags(names: String*): NoMoreOptions[T] = add(new NoMoreOptions[T](names))

  def default: UnknownParamOption[T] = _default

  def printHelp(firstTab: Int): List[String] = {
    options.filterNot(_._hidden).toList.flatMap(_.toStrings(firstTab))
  }

  def parameter(names: String*): ParamOption[T] = add(new ParamOption[T](names))

}
