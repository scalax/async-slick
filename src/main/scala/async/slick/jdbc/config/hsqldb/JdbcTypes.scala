package slick.async.jdbc

import slick.ast._

trait HsqldbJdbcTypes extends JdbcTypes {
  override val byteArrayJdbcType = new ByteArrayJdbcType {
    override def sqlTypeName(sym: Option[FieldSymbol]) = "LONGVARBINARY"
  }
  override val uuidJdbcType = new UUIDJdbcType {
    override def sqlType = java.sql.Types.BINARY
    override def sqlTypeName(sym: Option[FieldSymbol]) = "BINARY(16)"
  }
}