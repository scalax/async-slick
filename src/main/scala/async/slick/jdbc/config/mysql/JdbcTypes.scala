package slick.async.jdbc

import slick.ast._

trait MySQLJdbcTypes extends JdbcTypes {
  override val stringJdbcType = new StringJdbcType {
    override def valueToSQLLiteral(value: String) = if (value eq null) "NULL" else {
      val sb = new StringBuilder
      sb append '\''
      for (c <- value) c match {
        case '\'' => sb append "\\'"
        case '"' => sb append "\\\""
        case 0 => sb append "\\0"
        case 26 => sb append "\\Z"
        case '\b' => sb append "\\b"
        case '\n' => sb append "\\n"
        case '\r' => sb append "\\r"
        case '\t' => sb append "\\t"
        case '\\' => sb append "\\\\"
        case _ => sb append c
      }
      sb append '\''
      sb.toString
    }
  }

  import java.util.UUID

  override val uuidJdbcType = new UUIDJdbcType {
    override def sqlType = java.sql.Types.BINARY
    override def sqlTypeName(sym: Option[FieldSymbol]) = "BINARY(16)"

    override def valueToSQLLiteral(value: UUID): String =
      "x'" + value.toString.replace("-", "") + "'"
  }
}