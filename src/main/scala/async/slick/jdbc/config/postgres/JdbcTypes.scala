package slick.async.jdbc

import java.sql.{ PreparedStatement, ResultSet }
import java.util.UUID

import slick.ast._

trait PostgresJdbcTypes extends JdbcTypes {
  override val byteArrayJdbcType = new ByteArrayJdbcType
  override val uuidJdbcType = new UUIDJdbcType

  class ByteArrayJdbcType extends super.ByteArrayJdbcType {
    override val sqlType = java.sql.Types.BINARY
    override def sqlTypeName(sym: Option[FieldSymbol]) = "BYTEA"
  }

  class UUIDJdbcType extends super.UUIDJdbcType {
    override def sqlTypeName(sym: Option[FieldSymbol]) = "UUID"
    override def setValue(v: UUID, p: PreparedStatement, idx: Int) = p.setObject(idx, v, sqlType)
    override def getValue(r: ResultSet, idx: Int) = r.getObject(idx).asInstanceOf[UUID]
    override def updateValue(v: UUID, r: ResultSet, idx: Int) = r.updateObject(idx, v)
    override def valueToSQLLiteral(value: UUID) = "'" + value + "'"
    override def hasLiteralForm = true
  }
}