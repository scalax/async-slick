package slick.async.jdbc.config

import slick.ast._
import slick.async.jdbc.OracleProfile
import slick.async.relational.RelationalProfile
import slick.async.sql.SqlProfile

trait RelationalColumnOptions {
  val PrimaryKey = ColumnOption.PrimaryKey
  def Default[T](defaultValue: T) = RelationalProfile.ColumnOption.Default[T](defaultValue)
  val AutoInc = ColumnOption.AutoInc
  val Unique = ColumnOption.Unique
  val Length = RelationalProfile.ColumnOption.Length
}

trait SqlTableColumnOptions extends RelationalColumnOptions {
  def SqlType(typeName: String) = SqlProfile.ColumnOption.SqlType(typeName)
}

trait OracleColumnOptions extends SqlTableColumnOptions {
  def AutoIncSequenceName(name: String) = OracleProfile.ColumnOption.AutoIncSequenceName(name)
  def AutoIncTriggerName(name: String) = OracleProfile.ColumnOption.AutoIncTriggerName(name)
}