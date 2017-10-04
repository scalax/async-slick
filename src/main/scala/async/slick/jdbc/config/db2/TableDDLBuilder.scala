package slick.async.jdbc.config

import slick.async.relational.RelationalProfile
import slick.lifted.Index

abstract class DB2TableDDLBuilder(table: RelationalProfile#Table[_]) extends TableDDLBuilder(table) {
  override protected def createIndex(idx: Index) = {
    if (idx.unique) {
      /* Create a UNIQUE CONSTRAINT (with an automatically generated backing
       * index) because DB2 does not allow a FOREIGN KEY CONSTRAINT to
       * reference columns which have a UNIQUE INDEX but not a nominal UNIQUE
       * CONSTRAINT. */
      val sb = new StringBuilder append "ALTER TABLE " append sqlUtilsComponent.quoteIdentifier(table.tableName) append " ADD "
      sb append "CONSTRAINT " append sqlUtilsComponent.quoteIdentifier(idx.name) append " UNIQUE("
      addIndexColumnList(idx.on, sb, idx.table.tableName)
      sb append ")"
      sb.toString
    } else super.createIndex(idx)
  }

  //For compatibility with all versions of DB2
  //http://stackoverflow.com/questions/3006999/sql-query-to-truncate-table-in-ibm-db2
  override def truncateTable = s"DELETE FROM ${sqlUtilsComponent.quoteTableName(tableNode)}"
}