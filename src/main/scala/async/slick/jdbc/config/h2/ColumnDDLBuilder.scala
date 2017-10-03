package slick.async.jdbc.config

import slick.ast._

abstract class H2ColumnDDLBuilder(column: FieldSymbol) extends ColumnDDLBuilder(column) {
  override protected def appendOptions(sb: StringBuilder) {
    if (defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
    if (notNull) sb append " NOT NULL"
    if (primaryKey) sb append " PRIMARY KEY"
    if (autoIncrement) sb append " AUTO_INCREMENT"
    if (unique) sb append " UNIQUE"
  }
}