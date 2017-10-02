package slick.async.jdbc.config

import slick.ast._
import slick.async.jdbc.{ QueryBuilder, SQLiteQueryBuilder }
import slick.compiler.CompilerState

trait SQLiteCrudCompiler extends CrudCompiler { self =>
  override def createInsertBuilder(node: Insert): InsertBuilder = new SQLiteInsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
  override def createUpsertBuilder(node: Insert): InsertBuilder = new SQLiteUpsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new SQLiteQueryBuilder(n, state) {
    override lazy val commonCapabilities = self.capabilitiesContent
    override lazy val sqlUtilsComponent = self.sqlUtilsComponent
    override val scalarFrom = self.scalarFrom
  }
}