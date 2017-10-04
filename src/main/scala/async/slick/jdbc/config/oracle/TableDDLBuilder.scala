package slick.async.jdbc.config

import slick.async.relational.RelationalProfile
import slick.lifted.{ ForeignKey, Index }
import slick.model.ForeignKeyAction

abstract class OracleTableDDLBuilder(table: RelationalProfile#Table[_]) extends TableDDLBuilder(table) {
  override val createPhase1 = super.createPhase1 ++ createAutoIncSequences
  override val dropPhase2 = dropAutoIncSequences ++ super.dropPhase2

  def createAutoIncSequences = columns.flatMap {
    case cb: OracleColumnDDLBuilder =>
      cb.createSequenceAndTrigger(table)
  }

  def dropAutoIncSequences = columns.flatMap {
    case cb: OracleColumnDDLBuilder =>
      cb.dropTriggerAndSequence(table)
  }

  override protected def addForeignKey(fk: ForeignKey, sb: StringBuilder) {
    sb append "constraint " append sqlUtilsComponent.quoteIdentifier(fk.name) append " foreign key("
    addForeignKeyColumnList(fk.linearizedSourceColumns, sb, table.tableName)
    sb append ") references " append sqlUtilsComponent.quoteIdentifier(fk.targetTable.tableName) append "("
    addForeignKeyColumnList(fk.linearizedTargetColumnsForOriginalTargetTable, sb, fk.targetTable.tableName)
    sb append ')'
    fk.onDelete match {
      case ForeignKeyAction.Cascade => sb append " on delete cascade"
      case ForeignKeyAction.SetNull => sb append " on delete set null"
      case _ => // do nothing
    }
    if (fk.onUpdate == ForeignKeyAction.Cascade) sb append " initially deferred"
  }

  override protected def createIndex(idx: Index) = {
    if (idx.unique) {
      /* Create a UNIQUE CONSTRAINT (with an automatically generated backing
       * index) because Oracle does not allow a FOREIGN KEY CONSTRAINT to
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