package slick.async.jdbc.config

import slick.async.jdbc._

abstract class HsqldbSequenceDDLBuilder[T](seq: Sequence[T]) extends SequenceDDLBuilder(seq) {
  override def buildDDL: DDL = {
    import seq.integral._
    val increment = seq._increment.getOrElse(one)
    val desc = increment < zero
    val start = seq._start.getOrElse(if (desc) -1 else 1)
    val b = new StringBuilder append "CREATE SEQUENCE " append sqlUtilsComponent.quoteIdentifier(seq.name)
    seq._increment.foreach { b append " INCREMENT BY " append _ }
    seq._minValue.foreach { b append " MINVALUE " append _ }
    seq._maxValue.foreach { b append " MAXVALUE " append _ }
    /* The START value in Hsqldb defaults to 0 instead of the more
     * conventional 1/-1 so we rewrite it to make 1/-1 the default. */
    if (start != 0) b append " START WITH " append start
    if (seq._cycle) b append " CYCLE"
    DDL(b.toString, "DROP SEQUENCE " + sqlUtilsComponent.quoteIdentifier(seq.name))
  }
}