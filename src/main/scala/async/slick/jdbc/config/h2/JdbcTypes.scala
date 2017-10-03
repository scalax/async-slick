package slick.async.jdbc

import java.util.UUID

import slick.ast._

trait H2JdbcTypes extends JdbcTypes {
  override val uuidJdbcType = new UUIDJdbcType {
    override def sqlTypeName(sym: Option[FieldSymbol]) = "UUID"
    override def valueToSQLLiteral(value: UUID) = "'" + value + "'"
    override def hasLiteralForm = true
  }
}