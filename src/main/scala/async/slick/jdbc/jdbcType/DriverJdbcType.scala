package slick.async.jdbc

import java.sql.{ PreparedStatement, ResultSet }

import slick.SlickException
import slick.ast.{ FieldSymbol, ScalaBaseType }
import slick.async.relational.RelationalProfile

import scala.reflect.ClassTag

abstract class DriverJdbcType[@specialized T](implicit val classTag: ClassTag[T]) extends JdbcType[T] {
  def scalaType = ScalaBaseType[T]
  def sqlTypeName(sym: Option[FieldSymbol]): String = defaultSqlTypeName(this, sym)
  def valueToSQLLiteral(value: T) =
    if (hasLiteralForm) value.toString
    else throw new SlickException(sqlTypeName(None) + " does not have a literal representation")
  def hasLiteralForm = true
  def wasNull(r: ResultSet, idx: Int) = r.wasNull()
  def setNull(p: PreparedStatement, idx: Int): Unit = p.setNull(idx, sqlType)

  def defaultSqlTypeName(tmd: JdbcType[_], sym: Option[FieldSymbol]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR =>
      val size = sym.flatMap(_.findColumnOption[RelationalProfile.ColumnOption.Length])
      size.fold("VARCHAR(254)")(l => if (l.varying) s"VARCHAR(${l.length})" else s"CHAR(${l.length})")
    case java.sql.Types.DECIMAL => "DECIMAL(21,2)"
    case t => JdbcTypesComponent.typeNames.getOrElse(
      t,
      throw new SlickException("No SQL type name found in java.sql.Types for code " + t)
    )
  }
}