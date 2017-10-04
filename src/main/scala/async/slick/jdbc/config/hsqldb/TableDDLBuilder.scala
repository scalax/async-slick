package slick.async.jdbc.config

import slick.async.relational.RelationalProfile
import slick.lifted.{ ForeignKey, Index, PrimaryKey }

abstract class HsqldbTableDDLBuilder(table: RelationalProfile#Table[_]) extends TableDDLBuilder(table) {
  override protected def createIndex(idx: Index) = {
    if (idx.unique) {
      /* Create a UNIQUE CONSTRAINT (with an automatically generated backing
       * index) because Hsqldb does not allow a FOREIGN KEY CONSTRAINT to
       * reference columns which have a UNIQUE INDEX but not a nominal UNIQUE
       * CONSTRAINT. */
      val sb = new StringBuilder append "ALTER TABLE " append sqlUtilsComponent.quoteIdentifier(table.tableName) append " ADD "
      sb append "CONSTRAINT " append sqlUtilsComponent.quoteIdentifier(idx.name) append " UNIQUE("
      addIndexColumnList(idx.on, sb, idx.table.tableName)
      sb append ")"
      sb.toString
    } else super.createIndex(idx)
  }
}