package slick.async.jdbc

import slick.async.jdbc.meta.{ MColumn, MPrimaryKey, MTable }
import scala.concurrent.ExecutionContext

class SQLiteModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext) extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {
  override def createColumnBuilder(tableBuilder: TableBuilder, meta: MColumn): ColumnBuilder = new ColumnBuilder(tableBuilder, meta) {
    /** Regex matcher to extract name and length out of a db type name with length ascription */
    final val TypePattern = "^([A-Z]+)(\\(([0-9]+)\\))?$".r
    private val (_dbType, _size) = meta.typeName match {
      case TypePattern(d, _, s) => (d, Option(s).map(_.toInt))
      case "" => ("TEXT", None)
    }
    override def dbType = Some(_dbType)
    override def length = _size
    override def varying = dbType == Some("VARCHAR")
    override def default = meta.columnDef.map((_, tpe)).collect {
      case ("null", _) => Some(None) // 3.7.15-M1
      case (v, "java.sql.Timestamp") => {
        import scala.util.{ Try, Success }
        val convertors = Seq(
          (s: String) => new java.sql.Timestamp(s.toLong),
          (s: String) => java.sql.Timestamp.valueOf(s),
          (s: String) => new java.sql.Timestamp(javax.xml.bind.DatatypeConverter.parseDateTime(s).getTime.getTime),
          (s: String) => new java.sql.Timestamp(javax.xml.bind.DatatypeConverter.parseDateTime(s.replaceAll(" ", "T")).getTime.getTime),
          (s: String) => {
            if (s == "now")
              "new java.sql.Timestamp(java.util.Calendar.getInstance().getTime().getTime())"
            else
              throw new Exception(s"Failed to parse timestamp - $s")
          }
        )
        val v2 = v.replaceAll("\"", "")
        convertors.collectFirst(fn => Try(fn(v2)) match {
          case Success(v) => Some(v)
        })
      }
    }.getOrElse { super.default }
    override def tpe = dbType match {
      case Some("DOUBLE") => "Double"
      case Some("DATE") => "java.sql.Date"
      case Some("TIME") => "java.sql.Time"
      case Some("TIMESTAMP") => "java.sql.Timestamp"
      case Some("BLOB") => "java.sql.Blob"
      case _ => super.tpe
    }
  }
  override def createPrimaryKeyBuilder(tableBuilder: TableBuilder, meta: Seq[MPrimaryKey]): PrimaryKeyBuilder = new PrimaryKeyBuilder(tableBuilder, meta) {
    // in 3.7.15-M1:
    override def columns = super.columns.map(_.stripPrefix("\"").stripSuffix("\""))
  }
  override def readIndices(t: MTable) = super.readIndices(t).map(
    _.filterNot(
      _.exists(_.indexName.exists(_.startsWith("sqlite_autoindex_")))
    )
  )
}