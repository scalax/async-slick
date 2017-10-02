package slick.async.jdbc.config

import slick.ast._

/* Extending super.InsertBuilder here instead of super.UpsertBuilder. MERGE is almost identical to INSERT on H2. */
abstract class H2UpsertBuilder(ins: Insert) extends InsertBuilder(ins) {
  override protected def buildInsertStart = allNames.mkString(s"merge into $tableName (", ",", ") ")
}