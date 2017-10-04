package slick.async.jdbc.config

import slick.SlickException
import slick.ast._
import slick.async.jdbc.DDL
import slick.async.relational.RelationalProfile
import slick.lifted.{ ForeignKey, Index, PrimaryKey }

abstract class TableDDLBuilder(val table: RelationalProfile#Table[_]) { self =>
  val sqlUtilsComponent: BasicSqlUtilsComponent
  def createColumnDDLBuilder(column: FieldSymbol, table: RelationalProfile#Table[_]): ColumnDDLBuilder

  protected val tableNode = table.toNode.asInstanceOf[TableExpansion].table.asInstanceOf[TableNode]
  protected val columns: Iterable[ColumnDDLBuilder] = table.create_*.map(fs => createColumnDDLBuilder(fs, table))
  protected val indexes: Iterable[Index] = table.indexes
  protected val foreignKeys: Iterable[ForeignKey] = table.foreignKeys
  protected val primaryKeys: Iterable[PrimaryKey] = table.primaryKeys

  def buildDDL: DDL = {
    if (primaryKeys.size > 1)
      throw new SlickException("Table " + tableNode.tableName + " defines multiple primary keys ("
        + primaryKeys.map(_.name).mkString(", ") + ")")
    DDL(createPhase1, createPhase2, dropPhase1, dropPhase2, truncatePhase)
  }

  protected def createPhase1 = Iterable(createTable) ++ primaryKeys.map(createPrimaryKey) ++ indexes.map(createIndex)
  protected def createPhase2 = foreignKeys.map(createForeignKey)
  protected def dropPhase1 = foreignKeys.map(dropForeignKey)
  protected def dropPhase2 = primaryKeys.map(dropPrimaryKey) ++ Iterable(dropTable)
  protected def truncatePhase = Iterable(truncateTable)

  protected def createTable: String = {
    val b = new StringBuilder append "create table " append sqlUtilsComponent.quoteTableName(tableNode) append " ("
    var first = true
    for (c <- columns) {
      if (first) first = false else b append ","
      c.appendColumn(b)
    }
    addTableOptions(b)
    b append ")"
    b.toString
  }

  protected def addTableOptions(b: StringBuilder) {}

  protected def dropTable: String = "drop table " + sqlUtilsComponent.quoteTableName(tableNode)

  protected def truncateTable: String = "truncate table " + sqlUtilsComponent.quoteTableName(tableNode)

  protected def createIndex(idx: Index): String = {
    val b = new StringBuilder append "create "
    if (idx.unique) b append "unique "
    b append "index " append sqlUtilsComponent.quoteIdentifier(idx.name) append " on " append sqlUtilsComponent.quoteTableName(tableNode) append " ("
    addIndexColumnList(idx.on, b, idx.table.tableName)
    b append ")"
    b.toString
  }

  protected def createForeignKey(fk: ForeignKey): String = {
    val sb = new StringBuilder append "alter table " append sqlUtilsComponent.quoteTableName(tableNode) append " add "
    addForeignKey(fk, sb)
    sb.toString
  }

  protected def addForeignKey(fk: ForeignKey, sb: StringBuilder) {
    sb append "constraint " append sqlUtilsComponent.quoteIdentifier(fk.name) append " foreign key("
    addForeignKeyColumnList(fk.linearizedSourceColumns, sb, tableNode.tableName)
    sb append ") references " append sqlUtilsComponent.quoteTableName(fk.targetTable) append "("
    addForeignKeyColumnList(fk.linearizedTargetColumnsForOriginalTargetTable, sb, fk.targetTable.tableName)
    sb append ") on update " append fk.onUpdate.action
    sb append " on delete " append fk.onDelete.action
  }

  protected def createPrimaryKey(pk: PrimaryKey): String = {
    val sb = new StringBuilder append "alter table " append sqlUtilsComponent.quoteTableName(tableNode) append " add "
    addPrimaryKey(pk, sb)
    sb.toString
  }

  protected def addPrimaryKey(pk: PrimaryKey, sb: StringBuilder) {
    sb append "constraint " append sqlUtilsComponent.quoteIdentifier(pk.name) append " primary key("
    addPrimaryKeyColumnList(pk.columns, sb, tableNode.tableName)
    sb append ")"
  }

  protected def dropForeignKey(fk: ForeignKey): String =
    "alter table " + sqlUtilsComponent.quoteTableName(tableNode) + " drop constraint " + sqlUtilsComponent.quoteIdentifier(fk.name)

  protected def dropPrimaryKey(pk: PrimaryKey): String =
    "alter table " + sqlUtilsComponent.quoteTableName(tableNode) + " drop constraint " + sqlUtilsComponent.quoteIdentifier(pk.name)

  protected def addIndexColumnList(columns: IndexedSeq[Node], sb: StringBuilder, requiredTableName: String) =
    addColumnList(columns, sb, requiredTableName, "index")

  protected def addForeignKeyColumnList(columns: IndexedSeq[Node], sb: StringBuilder, requiredTableName: String) =
    addColumnList(columns, sb, requiredTableName, "foreign key constraint")

  protected def addPrimaryKeyColumnList(columns: IndexedSeq[Node], sb: StringBuilder, requiredTableName: String) =
    addColumnList(columns, sb, requiredTableName, "foreign key constraint")

  protected def addColumnList(columns: IndexedSeq[Node], sb: StringBuilder, requiredTableName: String, typeInfo: String) {
    var first = true
    for (c <- columns) c match {
      case Select(t: TableNode, field: FieldSymbol) =>
        if (first) first = false
        else sb append ","
        sb append sqlUtilsComponent.quoteIdentifier(field.name)
        if (requiredTableName != t.tableName)
          throw new SlickException("All columns in " + typeInfo + " must belong to table " + requiredTableName)
      case _ => throw new SlickException("Cannot use column " + c + " in " + typeInfo + " (only named columns are allowed)")
    }
  }
}