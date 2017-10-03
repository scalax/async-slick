package slick.async.jdbc

import slick.async.jdbc.meta.MTable
import scala.concurrent.ExecutionContext

class DerbyModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext) extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {
  override def createTableNamer(mTable: MTable): TableNamer = new TableNamer(mTable) {
    override def schema = super.schema.filter(_ != "APP") // remove default schema
  }
}