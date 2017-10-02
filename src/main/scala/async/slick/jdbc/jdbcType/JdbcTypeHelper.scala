package slick.async.jdbc

import java.sql.{ Blob, Clob, Date, PreparedStatement, ResultSet, Time, Timestamp }
import java.util.UUID

import slick.SlickException
import slick.ast._

object JdbcTypeHelper {

  def unapply(t: Type) = Some((JdbcTypeHelper.jdbcTypeFor(t), t.isInstanceOf[OptionType]))

  val columnTypes = new JdbcTypes {}

  def jdbcTypeFor(t: Type): slick.async.jdbc.JdbcType[Any] = ((t.structural match {
    case tmd: slick.async.jdbc.JdbcType[_] => tmd
    case ScalaBaseType.booleanType => columnTypes.booleanJdbcType
    case ScalaBaseType.bigDecimalType => columnTypes.bigDecimalJdbcType
    case ScalaBaseType.byteType => columnTypes.byteJdbcType
    case ScalaBaseType.charType => columnTypes.charJdbcType
    case ScalaBaseType.doubleType => columnTypes.doubleJdbcType
    case ScalaBaseType.floatType => columnTypes.floatJdbcType
    case ScalaBaseType.intType => columnTypes.intJdbcType
    case ScalaBaseType.longType => columnTypes.longJdbcType
    case ScalaBaseType.nullType => columnTypes.nullJdbcType
    case ScalaBaseType.shortType => columnTypes.shortJdbcType
    case ScalaBaseType.stringType => columnTypes.stringJdbcType
    case t: OptionType => jdbcTypeFor(t.elementType)
    case t: ErasedScalaBaseType[_, _] => jdbcTypeFor(t.erasure)
    case t => throw new SlickException("JdbcProfile has no JdbcType for type " + t)
  }): JdbcType[_]).asInstanceOf[JdbcType[Any]]

  def valueToSQLLiteral(v: Any, tpe: Type): String = {
    val JdbcTypeHelper(ti, option) = tpe
    if (option) v.asInstanceOf[Option[Any]].fold("null")(ti.valueToSQLLiteral)
    else ti.valueToSQLLiteral(v)
  }
  //val scalarFrom: Option[String] = None
}

trait JdbcTypes {
  val booleanJdbcType = new BooleanJdbcType
  val blobJdbcType = new BlobJdbcType
  val byteJdbcType = new ByteJdbcType
  val byteArrayJdbcType = new ByteArrayJdbcType
  val charJdbcType = new CharJdbcType
  val clobJdbcType = new ClobJdbcType
  val dateJdbcType = new DateJdbcType
  val doubleJdbcType = new DoubleJdbcType
  val floatJdbcType = new FloatJdbcType
  val intJdbcType = new IntJdbcType
  val longJdbcType = new LongJdbcType
  val shortJdbcType = new ShortJdbcType
  val stringJdbcType = new StringJdbcType
  val timeJdbcType = new TimeJdbcType
  val timestampJdbcType = new TimestampJdbcType
  val uuidJdbcType = new UUIDJdbcType
  val bigDecimalJdbcType = new BigDecimalJdbcType
  val nullJdbcType = new NullJdbcType

  class BooleanJdbcType extends DriverJdbcType[Boolean] {
    override def sqlType = java.sql.Types.BOOLEAN
    override def setValue(v: Boolean, p: PreparedStatement, idx: Int) = p.setBoolean(idx, v)
    override def getValue(r: ResultSet, idx: Int) = r.getBoolean(idx)
    override def updateValue(v: Boolean, r: ResultSet, idx: Int) = r.updateBoolean(idx, v)
  }

  class BlobJdbcType extends DriverJdbcType[Blob] {
    override def sqlType = java.sql.Types.BLOB
    override def setValue(v: Blob, p: PreparedStatement, idx: Int) = p.setBlob(idx, v)
    override def getValue(r: ResultSet, idx: Int) = r.getBlob(idx)
    override def updateValue(v: Blob, r: ResultSet, idx: Int) = r.updateBlob(idx, v)
    override def hasLiteralForm = false
  }

  class ByteJdbcType extends DriverJdbcType[Byte] with NumericTypedType {
    override def sqlType = java.sql.Types.TINYINT
    override def setValue(v: Byte, p: PreparedStatement, idx: Int) = p.setByte(idx, v)
    override def getValue(r: ResultSet, idx: Int) = r.getByte(idx)
    override def updateValue(v: Byte, r: ResultSet, idx: Int) = r.updateByte(idx, v)
  }

  class ByteArrayJdbcType extends DriverJdbcType[Array[Byte]] {
    override def sqlType = java.sql.Types.BLOB
    override def setValue(v: Array[Byte], p: PreparedStatement, idx: Int) = p.setBytes(idx, v)
    override def getValue(r: ResultSet, idx: Int) = r.getBytes(idx)
    override def updateValue(v: Array[Byte], r: ResultSet, idx: Int) = r.updateBytes(idx, v)
    override def hasLiteralForm = false
  }

  class ClobJdbcType extends DriverJdbcType[Clob] {
    override def sqlType = java.sql.Types.CLOB
    override def setValue(v: Clob, p: PreparedStatement, idx: Int) = p.setClob(idx, v)
    override def getValue(r: ResultSet, idx: Int) = r.getClob(idx)
    override def updateValue(v: Clob, r: ResultSet, idx: Int) = r.updateClob(idx, v)
    override def hasLiteralForm = false
  }

  class CharJdbcType extends DriverJdbcType[Char] {
    override def sqlType = java.sql.Types.CHAR
    override def sqlTypeName(sym: Option[FieldSymbol]) = "CHAR(1)"
    override def setValue(v: Char, p: PreparedStatement, idx: Int) = stringJdbcType.setValue(String.valueOf(v), p, idx)
    override def getValue(r: ResultSet, idx: Int) = {
      val s = stringJdbcType.getValue(r, idx)
      if (s == null || s.isEmpty) ' ' else s.charAt(0)
    }
    override def updateValue(v: Char, r: ResultSet, idx: Int) = stringJdbcType.updateValue(String.valueOf(v), r, idx)
    override def valueToSQLLiteral(v: Char) = stringJdbcType.valueToSQLLiteral(String.valueOf(v))
  }

  class DateJdbcType extends DriverJdbcType[Date] {
    override def sqlType = java.sql.Types.DATE
    override def setValue(v: Date, p: PreparedStatement, idx: Int) = p.setDate(idx, v)
    override def getValue(r: ResultSet, idx: Int) = r.getDate(idx)
    override def updateValue(v: Date, r: ResultSet, idx: Int) = r.updateDate(idx, v)
    override def valueToSQLLiteral(value: Date) = "{d '" + value.toString + "'}"
  }

  class DoubleJdbcType extends DriverJdbcType[Double] with NumericTypedType {
    override def sqlType = java.sql.Types.DOUBLE
    override def setValue(v: Double, p: PreparedStatement, idx: Int) = p.setDouble(idx, v)
    override def getValue(r: ResultSet, idx: Int) = r.getDouble(idx)
    override def updateValue(v: Double, r: ResultSet, idx: Int) = r.updateDouble(idx, v)
  }

  class FloatJdbcType extends DriverJdbcType[Float] with NumericTypedType {
    override def sqlType = java.sql.Types.REAL // see http://docs.oracle.com/javase/1.5.0/docs/guide/jdbc/getstart/mapping.html#1055162
    override def setValue(v: Float, p: PreparedStatement, idx: Int) = p.setFloat(idx, v)
    override def getValue(r: ResultSet, idx: Int) = r.getFloat(idx)
    override def updateValue(v: Float, r: ResultSet, idx: Int) = r.updateFloat(idx, v)
  }

  class IntJdbcType extends DriverJdbcType[Int] with NumericTypedType {
    override def sqlType = java.sql.Types.INTEGER
    override def setValue(v: Int, p: PreparedStatement, idx: Int) = p.setInt(idx, v)
    override def getValue(r: ResultSet, idx: Int) = r.getInt(idx)
    override def updateValue(v: Int, r: ResultSet, idx: Int) = r.updateInt(idx, v)
  }

  class LongJdbcType extends DriverJdbcType[Long] with NumericTypedType {
    override def sqlType = java.sql.Types.BIGINT
    override def setValue(v: Long, p: PreparedStatement, idx: Int) = p.setLong(idx, v)
    override def getValue(r: ResultSet, idx: Int) = r.getLong(idx)
    override def updateValue(v: Long, r: ResultSet, idx: Int) = r.updateLong(idx, v)
  }

  class ShortJdbcType extends DriverJdbcType[Short] with NumericTypedType {
    override def sqlType = java.sql.Types.SMALLINT
    override def setValue(v: Short, p: PreparedStatement, idx: Int) = p.setShort(idx, v)
    override def getValue(r: ResultSet, idx: Int) = r.getShort(idx)
    override def updateValue(v: Short, r: ResultSet, idx: Int) = r.updateShort(idx, v)
  }

  class StringJdbcType extends DriverJdbcType[String] {
    override def sqlType = java.sql.Types.VARCHAR
    override def setValue(v: String, p: PreparedStatement, idx: Int) = p.setString(idx, v)
    override def getValue(r: ResultSet, idx: Int) = r.getString(idx)
    override def updateValue(v: String, r: ResultSet, idx: Int) = r.updateString(idx, v)
    override def valueToSQLLiteral(value: String) = if (value eq null) "NULL" else {
      val sb = new StringBuilder
      sb append '\''
      for (c <- value) c match {
        case '\'' => sb append "''"
        case _ => sb append c
      }
      sb append '\''
      sb.toString
    }
  }

  class TimeJdbcType extends DriverJdbcType[Time] {
    override def sqlType = java.sql.Types.TIME
    override def setValue(v: Time, p: PreparedStatement, idx: Int) = p.setTime(idx, v)
    override def getValue(r: ResultSet, idx: Int) = r.getTime(idx)
    override def updateValue(v: Time, r: ResultSet, idx: Int) = r.updateTime(idx, v)
    override def valueToSQLLiteral(value: Time) = "{t '" + value.toString + "'}"
  }

  class TimestampJdbcType extends DriverJdbcType[Timestamp] {
    override def sqlType = java.sql.Types.TIMESTAMP
    override def setValue(v: Timestamp, p: PreparedStatement, idx: Int) = p.setTimestamp(idx, v)
    override def getValue(r: ResultSet, idx: Int) = r.getTimestamp(idx)
    override def updateValue(v: Timestamp, r: ResultSet, idx: Int) = r.updateTimestamp(idx, v)
    override def valueToSQLLiteral(value: Timestamp) = "{ts '" + value.toString + "'}"
  }

  class UUIDJdbcType extends DriverJdbcType[UUID] {
    override def sqlType = java.sql.Types.OTHER
    override def setValue(v: UUID, p: PreparedStatement, idx: Int) = p.setBytes(idx, toBytes(v))
    override def getValue(r: ResultSet, idx: Int) = fromBytes(r.getBytes(idx))
    override def updateValue(v: UUID, r: ResultSet, idx: Int) = r.updateBytes(idx, toBytes(v))
    override def hasLiteralForm = false
    def toBytes(uuid: UUID) = if (uuid eq null) null else {
      val msb = uuid.getMostSignificantBits
      val lsb = uuid.getLeastSignificantBits
      val buff = new Array[Byte](16)
      for (i <- 0 until 8) {
        buff(i) = ((msb >> (8 * (7 - i))) & 255).toByte
        buff(8 + i) = ((lsb >> (8 * (7 - i))) & 255).toByte
      }
      buff
    }
    def fromBytes(data: Array[Byte]) = if (data eq null) null else {
      var msb = 0L
      var lsb = 0L
      for (i <- 0 until 8) {
        msb = (msb << 8) | (data(i) & 0xff)
      }
      for (i <- 8 until 16) {
        lsb = (lsb << 8) | (data(i) & 0xff)
      }
      new UUID(msb, lsb)
    }
  }

  class BigDecimalJdbcType extends DriverJdbcType[BigDecimal] with NumericTypedType {
    override def sqlType = java.sql.Types.DECIMAL
    override def setValue(v: BigDecimal, p: PreparedStatement, idx: Int) = p.setBigDecimal(idx, v.bigDecimal)
    override def getValue(r: ResultSet, idx: Int) = {
      val v = r.getBigDecimal(idx)
      if (v eq null) null else BigDecimal(v)
    }
    override def updateValue(v: BigDecimal, r: ResultSet, idx: Int) = r.updateBigDecimal(idx, v.bigDecimal)
  }

  class NullJdbcType extends DriverJdbcType[Null] {
    override def sqlType = java.sql.Types.NULL
    override def setValue(v: Null, p: PreparedStatement, idx: Int) = p.setString(idx, null)
    override def setNull(p: PreparedStatement, idx: Int) = p.setString(idx, null)
    override def getValue(r: ResultSet, idx: Int) = null
    override def updateValue(v: Null, r: ResultSet, idx: Int) = r.updateNull(idx)
    override def valueToSQLLiteral(value: Null) = "NULL"
  }
}