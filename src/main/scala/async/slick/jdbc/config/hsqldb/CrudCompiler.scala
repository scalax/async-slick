package slick.async.jdbc.config

import slick.ast._

trait HsqldbCrudCompiler extends CrudCompiler { self =>
  override def createInsertBuilder(node: Insert): InsertBuilder = new InsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
  override def createUpsertBuilder(node: Insert): InsertBuilder = new MysqlUpsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
}