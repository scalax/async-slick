package slick.async.jdbc

import slick.async.jdbc.meta.{ MColumn, MTable }
import scala.concurrent.ExecutionContext

class H2ModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext) extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {
  override def createTableNamer(mTable: MTable): TableNamer = new TableNamer(mTable) {
    override def schema = super.schema.filter(_ != "PUBLIC") // remove default schema
  }
  override def createColumnBuilder(tableBuilder: TableBuilder, meta: MColumn): ColumnBuilder = new ColumnBuilder(tableBuilder, meta) {
    override def length = super.length.filter(_ != Int.MaxValue) // H2 sometimes show this value, but doesn't accept it back in the DBType
    override def default = rawDefault.map((_, tpe)).collect {
      case (v, "java.util.UUID") =>
        if (v.matches("^['\"].*['\"]$"))
          Some(Some(java.util.UUID.fromString(v.replaceAll("[\'\"]", "")))) //strip quotes
        else
          None // The UUID is generated through a function - treat it as if there was no default.
    }.getOrElse { super.default }
    override def tpe = dbType match {
      case Some("UUID") => "java.util.UUID"
      case _ => super.tpe
    }
  }
}