package slick.async.jdbc.config

import slick.async.relational.RelationalProfile
import slick.lifted.{ ForeignKey, PrimaryKey }

abstract class MySQLTableDDLBuilder(table: RelationalProfile#Table[_]) extends TableDDLBuilder(table) {
  override protected def dropForeignKey(fk: ForeignKey) = {
    "ALTER TABLE " + sqlUtilsComponent.quoteIdentifier(table.tableName) + " DROP FOREIGN KEY " + sqlUtilsComponent.quoteIdentifier(fk.name)
  }
  override protected def dropPrimaryKey(pk: PrimaryKey): String = {
    "ALTER TABLE " + sqlUtilsComponent.quoteIdentifier(table.tableName) + " DROP PRIMARY KEY"
  }
}