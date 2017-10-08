package async.slick.jdbc.config.postgres

import slick.ast._
import slick.async.jdbc.config.ColumnDDLBuilder

abstract class PostgresColumnDDLBuilder(column: FieldSymbol) extends ColumnDDLBuilder(column) {
  override def appendColumn(sb: StringBuilder) {
    sb append sqlUtilsComponent.quoteIdentifier(column.name) append ' '
    if (autoIncrement && !customSqlType) {
      sb append (if (sqlType.toUpperCase == "BIGINT") "BIGSERIAL" else "SERIAL")
    } else appendType(sb)
    autoIncrement = false
    appendOptions(sb)
  }

  def lobTrigger(tname: String) =
    sqlUtilsComponent.quoteIdentifier(tname + "__" + sqlUtilsComponent.quoteIdentifier(column.name) + "_lob")

  def createLobTrigger(tname: String): Option[String] =
    if (sqlType == "lo") Some(
      "create trigger " + lobTrigger(tname) + " before update or delete on " +
        sqlUtilsComponent.quoteIdentifier(tname) + " for each row execute procedure lo_manage(" + sqlUtilsComponent.quoteIdentifier(column.name) + ")")
    else None

  def dropLobTrigger(tname: String): Option[String] =
    if (sqlType == "lo") Some(
      "drop trigger " + lobTrigger(tname) + " on " + sqlUtilsComponent.quoteIdentifier(tname))
    else None
}