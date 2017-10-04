package slick.async.jdbc.config

import slick.async.relational.RelationalProfile
import slick.lifted.ForeignKey

abstract class SQLServerTableDDLBuilder(table: RelationalProfile#Table[_]) extends TableDDLBuilder(table) {
  override protected def addForeignKey(fk: ForeignKey, sb: StringBuilder) {
    val updateAction = fk.onUpdate.action
    val deleteAction = fk.onDelete.action
    sb append "constraint " append sqlUtilsComponent.quoteIdentifier(fk.name) append " foreign key("
    addForeignKeyColumnList(fk.linearizedSourceColumns, sb, tableNode.tableName)
    sb append ") references " append sqlUtilsComponent.quoteTableName(fk.targetTable) append "("
    addForeignKeyColumnList(fk.linearizedTargetColumnsForOriginalTargetTable, sb, fk.targetTable.tableName)
    // SQLServer has no RESTRICT. Equivalent is NO ACTION. http://technet.microsoft.com/en-us/library/aa902684%28v=sql.80%29.aspx
    sb append ") on update " append (if (updateAction == "RESTRICT") "NO ACTION" else updateAction)
    sb append " on delete " append (if (deleteAction == "RESTRICT") "NO ACTION" else deleteAction)
  }
}