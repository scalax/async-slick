package slick.async.jdbc.config

import slick.ast._
import slick.async.jdbc.{ H2QueryBuilder, QueryBuilder }
import slick.compiler.CompilerState

trait H2CrudCompiler extends CrudCompiler { self =>
  override def createInsertBuilder(node: Insert): InsertBuilder = new InsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
  override def createUpsertBuilder(node: Insert): InsertBuilder = new H2UpsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new H2QueryBuilder(n, state) {
    override lazy val commonCapabilities = self.capabilitiesContent
    override lazy val sqlUtilsComponent = self.sqlUtilsComponent
    override val scalarFrom = self.scalarFrom
  }
}