package slick.async.jdbc.config

import slick.ast._
import slick.async.jdbc.InsertBuilderResult
import slick.util.ConstArray

abstract class MysqlUpsertBuilder(ins: Insert) extends UpsertBuilder(ins) {
  override def buildInsert: InsertBuilderResult = {
    val start = buildInsertStart
    val update = softNames.map(n => s"$n=VALUES($n)").mkString(", ")
    new InsertBuilderResult(table, s"$start values $allVars on duplicate key update $update", syms)
  }
}