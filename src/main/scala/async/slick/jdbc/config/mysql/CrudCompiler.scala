package slick.async.jdbc.config

import slick.ast._
import slick.async.jdbc.{ MysqlQueryBuilder, QueryBuilder }
import slick.compiler.CompilerState

trait MysqlCrudCompiler extends CrudCompiler { self =>
  override def createInsertBuilder(node: Insert): InsertBuilder = new InsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
  override def createUpsertBuilder(node: Insert): InsertBuilder = new MysqlUpsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
    override val scalarFrom = self.scalarFrom
  }
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new MysqlQueryBuilder(n, state) {
    override lazy val commonCapabilities = self.capabilitiesContent
    override lazy val sqlUtilsComponent = self.sqlUtilsComponent
    override val scalarFrom = self.scalarFrom
  }
}