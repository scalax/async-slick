package slick.async.jdbc.config

import slick.ast._

trait SQLiteCrudCompiler extends CrudCompiler { self =>
  override def createInsertBuilder(node: Insert): InsertBuilder = new SQLiteInsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
  override def createUpsertBuilder(node: Insert): InsertBuilder = new SQLiteUpsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
}