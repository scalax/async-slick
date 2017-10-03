package slick.async.jdbc

import slick.ast._

trait DB2JdbcTypes extends JdbcTypes {
  override val booleanJdbcType = new BooleanJdbcType
  override val uuidJdbcType = new UUIDJdbcType

  class UUIDJdbcType extends super.UUIDJdbcType {
    override def sqlType = java.sql.Types.CHAR
    override def sqlTypeName(sym: Option[FieldSymbol]) = "CHAR(16) FOR BIT DATA"
  }

  /* DB2 does not have a proper BOOLEAN type. The suggested workaround is
   * a constrained CHAR with constants 1 and 0 for TRUE and FALSE. */
  class BooleanJdbcType extends super.BooleanJdbcType {
    override def sqlTypeName(sym: Option[FieldSymbol]) = "CHAR(1)"
    override def valueToSQLLiteral(value: Boolean) = if (value) "1" else "0"
  }
}