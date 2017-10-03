package slick.async.jdbc

import java.sql._
import java.util.UUID

import slick.ast._

trait SQLiteJdbcTypes extends JdbcTypes {
  override val booleanJdbcType = new BooleanJdbcType
  override val dateJdbcType = new DateJdbcType
  override val timeJdbcType = new TimeJdbcType
  override val timestampJdbcType = new TimestampJdbcType
  override val uuidJdbcType = new UUIDJdbcType

  /* SQLite does not have a proper BOOLEAN type. The suggested workaround is
   * INTEGER with constants 1 and 0 for TRUE and FALSE. */
  class BooleanJdbcType extends super.BooleanJdbcType {
    override def sqlTypeName(sym: Option[FieldSymbol]) = "INTEGER"
    override def valueToSQLLiteral(value: Boolean) = if (value) "1" else "0"
  }
  /* The SQLite JDBC driver does not support the JDBC escape syntax for
   * date/time/timestamp literals. SQLite expects these values as milliseconds
   * since epoch. */
  class DateJdbcType extends super.DateJdbcType {
    override def valueToSQLLiteral(value: Date) = value.getTime.toString
  }
  class TimeJdbcType extends super.TimeJdbcType {
    override def valueToSQLLiteral(value: Time) = value.getTime.toString
  }
  class TimestampJdbcType extends super.TimestampJdbcType {
    override def valueToSQLLiteral(value: Timestamp) = value.getTime.toString
  }
  class UUIDJdbcType extends super.UUIDJdbcType {
    override def sqlType = java.sql.Types.BLOB
  }
}