package slick.async.jdbc.config

import slick.ast._
import slick.async.jdbc.DB2QueryBuilder
import slick.compiler.CompilerState

trait DB2CrudCompiler extends CrudCompiler { self =>
  override def createInsertBuilder(node: Insert): InsertBuilder = new InsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
  override def createUpsertBuilder(node: Insert): InsertBuilder = new UpsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
    override val scalarFrom = self.scalarFrom
  }
  override def createQueryBuilder(n: Node, state: CompilerState): slick.async.jdbc.QueryBuilder = new DB2QueryBuilder(n, state) {
    override lazy val commonCapabilities = self.capabilitiesContent
    override lazy val sqlUtilsComponent = self.sqlUtilsComponent
    override val scalarFrom = self.scalarFrom
  }
}