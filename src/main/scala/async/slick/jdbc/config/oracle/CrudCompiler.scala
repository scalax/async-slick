package slick.async.jdbc.config

import slick.ast._
import slick.async.jdbc.{ OracleQueryBuilder, QueryBuilder }
import slick.compiler.CompilerState

trait OracleCrudCompiler extends CrudCompiler { self =>
  override lazy val mappingCompiler: MappingCompiler = new OracleMappingCompiler

  override def createInsertBuilder(node: Insert): InsertBuilder = new InsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
  override def createUpsertBuilder(node: Insert): InsertBuilder = new UpsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
    override val scalarFrom = self.scalarFrom
  }
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new OracleQueryBuilder(n, state) {
    override lazy val commonCapabilities = self.capabilitiesContent
    override lazy val sqlUtilsComponent = self.sqlUtilsComponent
    override val scalarFrom = self.scalarFrom
  }
}