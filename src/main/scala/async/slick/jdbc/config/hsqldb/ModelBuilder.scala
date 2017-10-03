package slick.async.jdbc

import slick.async.jdbc.meta.MTable
import scala.concurrent.ExecutionContext

class HsqldbModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext) extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {
  override def createTableNamer(mTable: MTable): TableNamer = new TableNamer(mTable) {
    override def schema = super.schema.filter(_ != "PUBLIC") // remove default schema
    override def catalog = super.catalog.filter(_ != "PUBLIC") // remove default catalog
  }
}