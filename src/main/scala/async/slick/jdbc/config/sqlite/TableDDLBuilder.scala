package slick.async.jdbc.config

import slick.async.relational.RelationalProfile

abstract class SQLiteTableDDLBuilder(table: RelationalProfile#Table[_]) extends TableDDLBuilder(table) {
  override protected val foreignKeys = Nil // handled directly in addTableOptions
  override protected val primaryKeys = Nil // handled directly in addTableOptions

  override protected def addTableOptions(b: StringBuilder) {
    for (pk <- table.primaryKeys) {
      b append ","
      addPrimaryKey(pk, b)
    }
    for (fk <- table.foreignKeys) {
      b append ","
      addForeignKey(fk, b)
    }
  }

  override def truncateTable = "delete from " + sqlUtilsComponent.quoteTableName(tableNode)
}