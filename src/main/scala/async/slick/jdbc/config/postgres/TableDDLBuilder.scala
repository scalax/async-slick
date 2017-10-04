package slick.async.jdbc.config

import async.slick.jdbc.config.postgres.PostgresColumnDDLBuilder
import slick.async.relational.RelationalProfile

abstract class PostgresTableDDLBuilder(table: RelationalProfile#Table[_]) extends TableDDLBuilder(table) {
  override def createPhase1 = super.createPhase1 ++ columns.flatMap {
    case cb: PostgresColumnDDLBuilder => cb.createLobTrigger(table.tableName)
  }
  override def dropPhase1 = {
    val dropLobs = columns.flatMap {
      case cb: PostgresColumnDDLBuilder => cb.dropLobTrigger(table.tableName)
    }
    if (dropLobs.isEmpty) super.dropPhase1
    else Seq("delete from " + sqlUtilsComponent.quoteIdentifier(table.tableName)) ++ dropLobs ++ super.dropPhase1
  }
}