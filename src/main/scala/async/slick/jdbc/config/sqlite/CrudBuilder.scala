package slick.async.jdbc.config

import slick.ast._

abstract class SQLiteUpsertBuilder(ins: Insert) extends InsertBuilder(ins) {
  override protected def buildInsertStart = allNames.mkString(s"insert or replace into $tableName (", ",", ") ")
}

abstract class SQLiteInsertBuilder(ins: Insert) extends InsertBuilder(ins) {
  override protected def emptyInsert: String = s"insert into $tableName default values"
}