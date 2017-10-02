package slick.async.jdbc.config

import slick.ast._

trait SQLServerCrudCompiler extends CrudCompiler { self =>
  override def createInsertBuilder(node: Insert): InsertBuilder = new SQLServerInsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
  override def createUpsertBuilder(node: Insert): InsertBuilder = new SQLServerUpsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
}