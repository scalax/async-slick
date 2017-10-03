package slick.async.jdbc.config

import slick.ast._

abstract class SQLiteColumnDDLBuilder(column: FieldSymbol) extends ColumnDDLBuilder(column) {
  override protected def appendOptions(sb: StringBuilder) {
    if (defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
    if (autoIncrement) sb append " PRIMARY KEY AUTOINCREMENT"
    else if (primaryKey) sb append " PRIMARY KEY"
    if (notNull) sb append " NOT NULL"
    if (unique) sb append " UNIQUE"
  }
}