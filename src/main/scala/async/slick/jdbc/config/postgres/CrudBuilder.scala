package slick.async.jdbc.config

import slick.ast._
import slick.async.jdbc.InsertBuilderResult
import slick.util.ConstArray

abstract class PostgresUpsertBuilder(ins: Insert) extends UpsertBuilder(ins) {
  override def buildInsert: InsertBuilderResult = {
    val update = "update " + tableName + " set " + softNames.map(n => s"$n=?").mkString(",") + " where " + pkNames.map(n => s"$n=?").mkString(" and ")
    val nonAutoIncNames = nonAutoIncSyms.map(fs => sqlUtilsComponent.quoteIdentifier(fs.name)).mkString(",")
    val nonAutoIncVars = nonAutoIncSyms.map(_ => "?").mkString(",")
    val cond = pkNames.map(n => s"$n=?").mkString(" and ")
    val insert = s"insert into $tableName ($nonAutoIncNames) select $nonAutoIncVars where not exists (select 1 from $tableName where $cond)"
    new InsertBuilderResult(table, s"$update; $insert", ConstArray.from(softSyms ++ pkSyms))
  }

  override def transformMapping(n: Node) = reorderColumns(n, softSyms ++ pkSyms ++ nonAutoIncSyms.toSeq ++ pkSyms)
}