package slick.async.jdbc.config

import slick.async.jdbc._

abstract class DerbySequenceDDLBuilder[T](seq: Sequence[T]) extends SequenceDDLBuilder(seq) {
  override def buildDDL: DDL = {
    import seq.integral._
    val increment = seq._increment.getOrElse(one)
    val desc = increment < zero
    val b = new StringBuilder append "CREATE SEQUENCE " append sqlUtilsComponent.quoteIdentifier(seq.name)
    /* Set the START value explicitly because it defaults to the data type's
     * min/max value instead of the more conventional 1/-1. */
    b append " START WITH " append seq._start.getOrElse(if (desc) -1 else 1)
    seq._increment.foreach { b append " INCREMENT BY " append _ }
    seq._maxValue.foreach { b append " MAXVALUE " append _ }
    seq._minValue.foreach { b append " MINVALUE " append _ }
    /* Cycling is supported but does not conform to SQL:2008 semantics. Derby
     * cycles back to the START value instead of MINVALUE or MAXVALUE. No good
     * workaround available AFAICT. */
    if (seq._cycle) b append " CYCLE"
    DDL(b.toString, "DROP SEQUENCE " + sqlUtilsComponent.quoteIdentifier(seq.name))
  }
}