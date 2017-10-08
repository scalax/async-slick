package slick.async.jdbc

import slick.ast.{ Library, SequenceNode, TypedType }
import slick.async.jdbc.config.BasicSqlUtilsComponent
import slick.lifted.FunctionSymbolExtensionMethods._

abstract class SequenceDDLBuilder(seq: Sequence[_]) {
  val sqlUtilsComponent: BasicSqlUtilsComponent

  def buildDDL: DDL = {
    val b = new StringBuilder append "create sequence " append sqlUtilsComponent.quoteIdentifier(seq.name)
    seq._increment.foreach { b append " increment " append _ }
    seq._minValue.foreach { b append " minvalue " append _ }
    seq._maxValue.foreach { b append " maxvalue " append _ }
    seq._start.foreach { b append " start " append _ }
    if (seq._cycle) b append " cycle"
    DDL(b.toString, "drop sequence " + sqlUtilsComponent.quoteIdentifier(seq.name))
  }
}

abstract class Sequence[T](
  val name: String,
  val _minValue: Option[T],
  val _maxValue: Option[T],
  val _increment: Option[T],
  val _start: Option[T],
  val _cycle: Boolean)(implicit val tpe: TypedType[T], val integral: Integral[T]) { self =>
  val sqlUtilsComponent: BasicSqlUtilsComponent

  def min(v: T) = new Sequence[T](name, Some(v), _maxValue, _increment, _start, _cycle) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
  def max(v: T) = new Sequence[T](name, _minValue, Some(v), _increment, _start, _cycle) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
  def inc(v: T) = new Sequence[T](name, _minValue, _maxValue, Some(v), _start, _cycle) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
  def start(v: T) = new Sequence[T](name, _minValue, _maxValue, _increment, Some(v), _cycle) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
  def cycle = new Sequence[T](name, _minValue, _maxValue, _increment, _start, true) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }

  final def next = Library.NextValue.column[T](toNode)
  final def curr = Library.CurrentValue.column[T](toNode)

  def toNode = SequenceNode(name)(_increment.map(integral.toLong).getOrElse(1))

  //def schema: SchemaDescription = buildSequenceSchemaDescription(this)
  //TODO 未做数据库多样性处理
  def schema: DDL = new SequenceDDLBuilder(this) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }.buildDDL
}

object Sequence {
  def apply[T: TypedType: Integral](name: String)(component: BasicSqlUtilsComponent) = new Sequence[T](name, None, None, None, None, false) {
    override val sqlUtilsComponent = component
  }
}