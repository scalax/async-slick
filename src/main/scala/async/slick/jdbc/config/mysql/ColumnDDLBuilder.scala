package slick.async.jdbc.config

import slick.ast._

abstract class MySQLColumnDDLBuilder(column: FieldSymbol) extends ColumnDDLBuilder(column) {
  override protected def appendOptions(sb: StringBuilder) {
    if (defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
    if (notNull) sb append " NOT NULL"
    else if (sqlType.toUpperCase == "TIMESTAMP") sb append " NULL"
    if (autoIncrement) sb append " AUTO_INCREMENT"
    if (primaryKey) sb append " PRIMARY KEY"
    if (unique) sb append " UNIQUE"
  }
}