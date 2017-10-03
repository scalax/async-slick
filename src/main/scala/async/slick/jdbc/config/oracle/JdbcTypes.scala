package slick.async.jdbc

import java.sql.{ Array, Blob, PreparedStatement, ResultSet, Time, Timestamp }
import java.util.UUID

import slick.SlickException
import slick.ast._

trait OracleJdbcTypes extends JdbcTypes {
  val blobBufferSize: Int

  override val booleanJdbcType = new BooleanJdbcType
  override val blobJdbcType = new BlobJdbcType
  override val byteArrayJdbcType = new ByteArrayJdbcType
  override val stringJdbcType = new StringJdbcType
  override val timeJdbcType = new TimeJdbcType
  override val uuidJdbcType = new UUIDJdbcType

  /* Oracle does not have a proper BOOLEAN type. The suggested workaround is
   * a constrained CHAR with constants 1 and 0 for TRUE and FALSE. */
  class BooleanJdbcType extends super.BooleanJdbcType {
    override def sqlType = java.sql.Types.CHAR
    override def sqlTypeName(sym: Option[FieldSymbol]) = "CHAR(1)"
    override def valueToSQLLiteral(value: Boolean) = if (value) "1" else "0"
  }

  class BlobJdbcType extends super.BlobJdbcType {
    override def setValue(v: Blob, p: PreparedStatement, idx: Int) = {
      val ob = p.getConnection.createBlob()
      var added = false
      try {
        val out = ob.setBinaryStream(0L)
        try {
          val in = v.getBinaryStream
          try {
            val buf = new scala.Array[Byte](blobBufferSize)
            var cont = true
            while (cont) {
              val len = in.read(buf)
              if (len < 0) cont = false
              else out.write(buf, 0, len)
            }
            p.setBlob(idx, ob)
            added = true
          } finally in.close()
        } finally out.close()
      } finally if (!added) ob.free()
    }
    override def updateValue(v: Blob, r: ResultSet, idx: Int) =
      throw new SlickException("OracleProfile does not support updating Blob values")
  }

  class ByteArrayJdbcType extends super.ByteArrayJdbcType {
    override def updateValue(v: scala.Array[Byte], r: ResultSet, idx: Int) =
      throw new SlickException("OracleProfile does not support updating Blob values")
  }

  class StringJdbcType extends super.StringJdbcType {
    /* Oracle treats an empty String as NULL, so we need to convert it back
     * when reading a null String value from a ResultSet. There is no way
     * to distinguish that from a proper NULL. */
    override def getValue(r: ResultSet, idx: Int) = {
      val v = super.getValue(r, idx)
      if (v eq null) "" else v
    }
  }

  class TimeJdbcType extends super.TimeJdbcType {
    override def sqlType = java.sql.Types.TIMESTAMP
    override def setValue(v: Time, p: PreparedStatement, idx: Int) = p.setTimestamp(idx, new Timestamp(v.getTime))
    override def getValue(r: ResultSet, idx: Int) = {
      val v = r.getTimestamp(idx)
      if (v eq null) null else new Time(v.getTime)
    }
    override def updateValue(v: Time, r: ResultSet, idx: Int) = r.updateTimestamp(idx, new Timestamp(v.getTime))
    override def valueToSQLLiteral(value: Time) = "{ts '" + (new Timestamp(value.getTime).toString) + "'}"
  }

  class UUIDJdbcType extends super.UUIDJdbcType {
    override def sqlTypeName(sym: Option[FieldSymbol]) = "RAW(32)"
    override def valueToSQLLiteral(value: UUID) = {
      val hex = value.toString.replace("-", "").toUpperCase
      s"hextoraw('$hex')"
    }
    override def hasLiteralForm = true
  }
}