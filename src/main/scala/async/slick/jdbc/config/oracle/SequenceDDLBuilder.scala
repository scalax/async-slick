package slick.async.jdbc.config

import slick.async.jdbc._

abstract class OracleSequenceDDLBuilder[T](seq: Sequence[T]) extends SequenceDDLBuilder(seq) {
  override def buildDDL: DDL = {
    val b = new StringBuilder append "create sequence " append sqlUtilsComponent.quoteIdentifier(seq.name)
    seq._increment.foreach { b append " increment by " append _ }
    seq._minValue.foreach { b append " minvalue " append _ }
    seq._maxValue.foreach { b append " maxvalue " append _ }
    seq._start.foreach { b append " start with " append _ }
    if (seq._cycle) b append " cycle nocache"
    DDL(b.toString, "drop sequence " + sqlUtilsComponent.quoteIdentifier(seq.name))
  }
}