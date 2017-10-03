package slick.async.jdbc

import slick.async.jdbc.meta.{ MColumn, MTable }

import scala.concurrent.ExecutionContext

class OracleModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext) extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {
  override def createColumnBuilder(tableBuilder: TableBuilder, meta: MColumn): ColumnBuilder = new ColumnBuilder(tableBuilder, meta) {
    override def tpe = meta.sqlType match {
      case 101 => "Double"
      case _ => super.tpe
    }
    override def rawDefault = super.rawDefault.map(_.stripSuffix(" ")).map {
      case "null" => "NULL"
      case v => v
    }
  }
}