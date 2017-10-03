package slick.async.jdbc.config

import slick.ast._
import slick.async.jdbc.{ JdbcTypes, OracleProfile }
import slick.async.relational.RelationalTableComponent

abstract class OracleColumnDDLBuilder(column: FieldSymbol) extends ColumnDDLBuilder(column) {
  var sequenceName: String = _
  var triggerName: String = _

  override def appendColumn(sb: StringBuilder) {
    val qname = sqlUtilsComponent.quoteIdentifier(column.name)
    sb append qname append ' '
    appendType(sb)
    appendOptions(sb)
    if (jdbcType.isInstanceOf[JdbcTypes#BooleanJdbcType]) {
      sb append " check (" append qname append " in (0, 1))"
    }
  }

  override protected def appendOptions(sb: StringBuilder) {
    if (defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
    if (notNull) sb append " NOT NULL"
    if (primaryKey) sb append " PRIMARY KEY"
    if (unique) sb append " UNIQUE"
  }

  override protected def handleColumnOption(o: ColumnOption[_]): Unit = o match {
    case OracleProfile.ColumnOption.AutoIncSequenceName(s) => sequenceName = s
    case OracleProfile.ColumnOption.AutoIncTriggerName(s) => triggerName = s
    case _ => super.handleColumnOption(o)
  }

  def createSequenceAndTrigger(t: RelationalTableComponent#Table[_]): Iterable[String] = if (!autoIncrement) Nil else {
    val tab = sqlUtilsComponent.quoteIdentifier(t.tableName)
    val seq = sqlUtilsComponent.quoteIdentifier(if (sequenceName eq null) t.tableName + "__" + column.name + "_seq" else sequenceName)
    val trg = sqlUtilsComponent.quoteIdentifier(if (triggerName eq null) t.tableName + "__" + column.name + "_trg" else triggerName)
    val col = sqlUtilsComponent.quoteIdentifier(column.name)
    Seq(
      s"create sequence $seq start with 1 increment by 1",
      s"create or replace trigger $trg before insert on $tab referencing new as new for each row" +
        s" when (new.$col is null) begin select $seq.nextval into :new.$col from sys.dual; end;"
    )
  }

  def dropTriggerAndSequence(t: RelationalTableComponent#Table[_]): Iterable[String] = if (!autoIncrement) Nil else {
    val seq = sqlUtilsComponent.quoteIdentifier(if (sequenceName eq null) t.tableName + "__" + column.name + "_seq" else sequenceName)
    val trg = sqlUtilsComponent.quoteIdentifier(if (triggerName eq null) t.tableName + "__" + column.name + "_trg" else triggerName)
    Seq(
      s"drop trigger $trg",
      s"drop sequence $seq"
    )
  }
}