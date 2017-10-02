package slick.async.jdbc.config

import slick.ast._

abstract class SQLServerInsertBuilder(ins: Insert) extends InsertBuilder(ins) {
  override protected def emptyInsert: String = s"insert into $tableName default values"
}

abstract class SQLServerUpsertBuilder(ins: Insert) extends UpsertBuilder(ins) {
  // SQL Server requires MERGE statements to end with a semicolon (unlike all other
  // statements that you can execute via JDBC)
  override protected def buildMergeEnd: String = super.buildMergeEnd + ";"
}