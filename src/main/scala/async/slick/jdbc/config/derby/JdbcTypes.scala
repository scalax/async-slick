package slick.async.jdbc

import slick.ast._

trait DerbyJdbcTypes extends JdbcTypes {
  override val booleanJdbcType = new BooleanJdbcType
  override val uuidJdbcType = new UUIDJdbcType

  /* Derby does not have a proper BOOLEAN type. The suggested workaround is
   * SMALLINT with constants 1 and 0 for TRUE and FALSE. */
  class BooleanJdbcType extends super.BooleanJdbcType {
    override def valueToSQLLiteral(value: Boolean) = if (value) "1" else "0"
  }

  class UUIDJdbcType extends super.UUIDJdbcType {
    override def sqlType = java.sql.Types.BINARY
    override def sqlTypeName(sym: Option[FieldSymbol]) = "CHAR(16) FOR BIT DATA"
  }
}