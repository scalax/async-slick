package slick.async.jdbc.config

import slick.SlickException
import slick.async.jdbc._

abstract class MySQLSequenceDDLBuilder[T](seq: Sequence[T]) extends SequenceDDLBuilder(seq) {
  override def buildDDL: DDL = {
    import seq.integral._
    val sqlType = JdbcTypeHelper.jdbcTypeFor(seq.tpe).sqlTypeName(None)
    val t = sqlType + " not null"
    val increment = seq._increment.getOrElse(one)
    val desc = increment < zero
    val minValue = seq._minValue getOrElse (if (desc) fromInt(java.lang.Integer.MIN_VALUE) else one)
    val maxValue = seq._maxValue getOrElse (if (desc) fromInt(-1) else fromInt(java.lang.Integer.MAX_VALUE))
    val start = seq._start.getOrElse(if (desc) maxValue else minValue)
    val beforeStart = start - increment
    if (!seq._cycle && (seq._minValue.isDefined && desc || seq._maxValue.isDefined && !desc))
      throw new SlickException("Sequences with limited size and without CYCLE are not supported by MySQLProfile's sequence emulation")
    val incExpr = if (seq._cycle) {
      if (desc) "if(id-" + (-increment) + "<" + minValue + "," + maxValue + ",id-" + (-increment) + ")"
      else "if(id+" + increment + ">" + maxValue + "," + minValue + ",id+" + increment + ")"
    } else {
      "id+(" + increment + ")"
    }
    DDL(
      Iterable(
        "create table " + sqlUtilsComponent.quoteIdentifier(seq.name + "_seq") + " (id " + t + ")",
        "insert into " + sqlUtilsComponent.quoteIdentifier(seq.name + "_seq") + " values (" + beforeStart + ")",
        "create function " + sqlUtilsComponent.quoteIdentifier(seq.name + "_nextval") + "() returns " + sqlType + " begin update " +
          sqlUtilsComponent.quoteIdentifier(seq.name + "_seq") + " set id=last_insert_id(" + incExpr + "); return last_insert_id(); end",
        "create function " + sqlUtilsComponent.quoteIdentifier(seq.name + "_currval") + "() returns " + sqlType + " begin " +
          "select max(id) into @v from " + sqlUtilsComponent.quoteIdentifier(seq.name + "_seq") + "; return @v; end"
      ),
      Iterable(
        "drop function " + sqlUtilsComponent.quoteIdentifier(seq.name + "_currval"),
        "drop function " + sqlUtilsComponent.quoteIdentifier(seq.name + "_nextval"),
        "drop table " + sqlUtilsComponent.quoteIdentifier(seq.name + "_seq")
      )
    )
  }
}