package slick.async.jdbc

import slick.async.jdbc.meta.{ MColumn, MTable }

import scala.concurrent.ExecutionContext
import scala.reflect.{ ClassTag, classTag }

class SQLServerModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext) extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {
  override def createColumnBuilder(tableBuilder: TableBuilder, meta: MColumn): ColumnBuilder = new ColumnBuilder(tableBuilder, meta) {
    override def tpe = dbType match {
      case Some("date") => "java.sql.Date"
      case Some("time") => "java.sql.Time"
      case _ => super.tpe
    }
    override def rawDefault = super.rawDefault.map(_.stripPrefix("(") // jtds
      .stripPrefix("(")
      .stripSuffix(")")
      .stripSuffix(")"))
    override def default = rawDefault.map((_, tpe)).collect {
      case ("0", "Boolean") => Some(false)
      case ("1", "Boolean") => Some(true)
    }.map(d => Some(d)).getOrElse { super.default }
  }
  override def jdbcTypeToScala(jdbcType: Int, typeName: String = ""): ClassTag[_] = {
    //SQL Server's TINYINT type is unsigned while Scala's Byte is signed
    if (jdbcType == java.sql.Types.TINYINT)
      classTag[Short]
    else
      super.jdbcTypeToScala(jdbcType, typeName)
  }
}