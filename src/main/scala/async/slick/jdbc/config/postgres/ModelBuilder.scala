package slick.async.jdbc

import slick.async.jdbc.meta.{ MColumn, MIndexInfo, MTable }
import scala.concurrent.ExecutionContext

class PostgresModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext) extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {
  override def createTableNamer(mTable: MTable): TableNamer = new TableNamer(mTable) {
    override def schema = super.schema.filter(_ != "public") // remove default schema
  }
  override def createColumnBuilder(tableBuilder: TableBuilder, meta: MColumn): ColumnBuilder = new ColumnBuilder(tableBuilder, meta) {
    /*
    The default value for numeric type behave different with postgres version
    PG9.5 - PG9.6:
     positive default value in int boundary: 1
     negative default value in int boundary: '-1'::integer
     positive default value between int boundary and long boundary: '123123214232131312'::bitint
     negative default value between int boundary and long boundary: '-123123214232131312'::bitint
     positive default value beyond long boundary: '111111111111111111111111111'::numeric
     negative default value beyond long boundary: '-111111111111111111111111111'::numeric
     positive floating: '1.1'::numeric
     negative floating: '-.1.1'::numeric

    PGX.X to PG9.4:
     positive default value in int boundary: 1
     negative default value in int boundary: (-1)
     positive default value between int boundary and long boundary: 123123214232131312::bitint
     negative default value between int boundary and long boundary: (-123123214232131312)::bitint
     positive default value beyond long boundary: 111111111111111111111111111::numeric
     negative default value beyond long boundary: (-111111111111111111111111111)::numeric
     positive floating: 1.1
     negative floating: (-.1.1)


     */
    val NumericPattern = "^['(]?(-?[0-9]+\\.?[0-9]*)[')]?(?:::(?:numeric|bigint|integer))?".r
    val TextPattern = "^'(.*)'::(?:bpchar|character varying|text)".r
    val UUIDPattern = "^'(.*)'::uuid".r
    override def default = meta.columnDef.map((_, tpe)).collect {
      case ("true", "Boolean") => Some(Some(true))
      case ("false", "Boolean") => Some(Some(false))
      case (TextPattern(str), "String") => Some(Some(str))
      case ("NULL::bpchar", "String") => Some(None)
      case (TextPattern(str), "Char") => str.length match {
        case 0 => Some(Some(' ')) // Default to one space, as the char will be space padded anyway
        case 1 => Some(Some(str.head))
        case _ => None // This is invalid, so let's not supply any default
      }
      case ("NULL::bpchar", "Char") => Some(None)
      case (NumericPattern(v), "Short") => Some(Some(v.toShort))
      case (NumericPattern(v), "Int") => Some(Some(v.toInt))
      case (NumericPattern(v), "Long") => Some(Some(v.toLong))
      case (NumericPattern(v), "Float") => Some(Some(v.toFloat))
      case (NumericPattern(v), "Double") => Some(Some(v.toDouble))
      case (NumericPattern(v), "scala.math.BigDecimal") => Some(Some(BigDecimal(s"$v")))
      case (UUIDPattern(v), "java.util.UUID") => Some(Some(java.util.UUID.fromString(v)))
      case (_, "java.util.UUID") => None // The UUID is generated through a function - treat it as if there was no default.
    }.getOrElse {
      val d = super.default
      if (meta.nullable == Some(true) && d == None) {
        Some(None)
      } else d
    }
    override def length: Option[Int] = {
      val l = super.length
      if (tpe == "String" && varying && l == Some(2147483647)) None
      else l
    }
    override def tpe = meta.typeName match {
      case "bytea" => "Array[Byte]"
      case "lo" if meta.sqlType == java.sql.Types.DISTINCT => "java.sql.Blob"
      case "uuid" => "java.util.UUID"
      case _ => super.tpe
    }
  }
  override def createIndexBuilder(tableBuilder: TableBuilder, meta: Seq[MIndexInfo]): IndexBuilder = new IndexBuilder(tableBuilder, meta) {
    // FIXME: this needs a test
    override def columns = super.columns.map(_.stripPrefix("\"").stripSuffix("\""))
  }
}