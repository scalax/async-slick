package slick.async.jdbc.config

import slick.ast._
import slick.async.jdbc.JdbcTypes

abstract class DB2ColumnDDLBuilder(column: FieldSymbol) extends ColumnDDLBuilder(column) {
  override def appendColumn(sb: StringBuilder) {
    val qname = sqlUtilsComponent.quoteIdentifier(column.name)
    sb append qname append ' '
    appendType(sb)
    appendOptions(sb)
    if (jdbcType.isInstanceOf[JdbcTypes#BooleanJdbcType]) {
      sb append " constraint " + sqlUtilsComponent.quoteIdentifier(column.name + "__bool") + " check (" append qname append " in (0, 1))"
    }
  }
}