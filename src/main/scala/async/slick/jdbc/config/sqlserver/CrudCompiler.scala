package slick.async.jdbc.config

import slick.ast._
import slick.async.jdbc.{ QueryBuilder, SQLServerQueryBuilder }
import slick.compiler.CompilerState

trait SQLServerCrudCompiler extends CrudCompiler { self =>
  override def createInsertBuilder(node: Insert): InsertBuilder = new SQLServerInsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
  override def createUpsertBuilder(node: Insert): InsertBuilder = new SQLServerUpsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
    override val scalarFrom = self.scalarFrom
  }
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new SQLServerQueryBuilder(n, state) {
    override lazy val commonCapabilities = self.capabilitiesContent
    override lazy val sqlUtilsComponent = self.sqlUtilsComponent
    override val scalarFrom = self.scalarFrom
  }
}