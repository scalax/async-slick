package slick.async.jdbc

import java.sql.{ Date, ResultSet, Time, Timestamp }

import slick.ast._

trait SQLServerJdbcTypes extends JdbcTypes {
  override val booleanJdbcType = new BooleanJdbcType
  override val byteJdbcType = new ByteJdbcType
  override val byteArrayJdbcType = new ByteArrayJdbcType
  override val dateJdbcType = new DateJdbcType
  override val timeJdbcType = new TimeJdbcType
  override val timestampJdbcType = new TimestampJdbcType
  override val uuidJdbcType = new UUIDJdbcType {
    override def sqlTypeName(sym: Option[FieldSymbol]) = "UNIQUEIDENTIFIER"
  }
  /* SQL Server does not have a proper BOOLEAN type. The suggested workaround is
   * BIT with constants 1 and 0 for TRUE and FALSE. */
  class BooleanJdbcType extends super.BooleanJdbcType {
    override def valueToSQLLiteral(value: Boolean) = if (value) "1" else "0"
  }
  /* Selecting a straight Date or Timestamp literal fails with a NPE (probably
   * because the type information gets lost along the way), so we cast all Date
   * and Timestamp values to the proper type. This work-around does not seem to
   * be required for Time values. */
  class DateJdbcType extends super.DateJdbcType {
    override def valueToSQLLiteral(value: Date) = "(convert(date, {d '" + value + "'}))"
  }
  class TimeJdbcType extends super.TimeJdbcType {
    override def valueToSQLLiteral(value: Time) = "(convert(time, {t '" + value + "'}))"
    override def getValue(r: ResultSet, idx: Int) = {
      val s = r.getString(idx)
      val sep = s.indexOf('.')
      if (sep == -1) Time.valueOf(s)
      else {
        val t = Time.valueOf(s.substring(0, sep))
        val millis = (("0." + s.substring(sep + 1)).toDouble * 1000.0).toInt
        t.setTime(t.getTime + millis)
        t
      }
    }
  }
  class TimestampJdbcType extends super.TimestampJdbcType {
    /* TIMESTAMP in SQL Server is a data type for sequence numbers. What we
     * want here is DATETIME. */
    override def sqlTypeName(sym: Option[FieldSymbol]) = "DATETIME"
    override def valueToSQLLiteral(value: Timestamp) = "(convert(datetime, {ts '" + value + "'}))"
  }
  /* SQL Server's TINYINT is unsigned, so we use SMALLINT instead to store a signed byte value.
   * The JDBC driver also does not treat signed values correctly when reading bytes from result
   * sets, so we read as Short and then convert to Byte. */
  class ByteJdbcType extends super.ByteJdbcType {
    override def sqlTypeName(sym: Option[FieldSymbol]) = "SMALLINT"
    override def getValue(r: ResultSet, idx: Int) = r.getShort(idx).toByte
  }
  /* SQL Server supports a literal notation for byte arrays */
  private[this] val hexChars = "0123456789ABCDEF".toCharArray()
  class ByteArrayJdbcType extends super.ByteArrayJdbcType {
    override def hasLiteralForm = true
    override def valueToSQLLiteral(value: Array[Byte]) = "0x" + bytesToHex(value)
    private[this] def bytesToHex(bytes: Array[Byte]) = {
      val a = new Array[Char](bytes.length * 2)
      var j = 0
      while (j < bytes.length) {
        val v = bytes(j) & 0xFF
        a(j * 2) = hexChars(v >>> 4)
        a(j * 2 + 1) = hexChars(v & 0x0F)
        j += 1
      }
      new String(a)
    }
  }
}