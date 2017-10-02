package slick.async.jdbc.config

import slick.ast._
import slick.async.jdbc.{ InsertBuilderResult, QueryBuilder }
import slick.compiler._
import slick.util.{ ConstArray, SQLBuilder }

trait CrudCompiler { self =>

  val capabilitiesContent: BasicCapabilities
  val compilerContent: RelationalQueryCompiler
  val sqlUtilsComponent: BasicSqlUtilsComponent
  val scalarFrom: Option[String]

  lazy val mappingCompiler: MappingCompiler = new MappingCompiler

  def createInsertBuilder(node: Insert): InsertBuilder = new InsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
  def createUpsertBuilder(node: Insert): InsertBuilder = new UpsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
    override val scalarFrom = self.scalarFrom
  }

  def createCheckInsertBuilder(node: Insert): InsertBuilder = new CheckInsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
    override val scalarFrom = self.scalarFrom
  }
  def createUpdateInsertBuilder(node: Insert): InsertBuilder = new UpdateInsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
    override val scalarFrom = self.scalarFrom
  }

  def createQueryBuilder(n: Node, state: CompilerState): slick.async.jdbc.QueryBuilder

  lazy val queryCompiler = compilerContent.computeQueryCompiler + new JdbcCodeGen(_.buildSelect) {
    override lazy val crudCompiler = self
    override lazy val mappingCompiler = self.mappingCompiler
  }
  lazy val updateCompiler = compilerContent.computeQueryCompiler + new JdbcCodeGen(_.buildUpdate) {
    override lazy val crudCompiler = self
    override lazy val mappingCompiler = self.mappingCompiler
  }
  lazy val deleteCompiler = compilerContent.computeQueryCompiler + new JdbcCodeGen(_.buildDelete) {
    override lazy val crudCompiler = self
    override lazy val mappingCompiler = self.mappingCompiler
  }
  lazy val insertCompiler: QueryCompiler = QueryCompiler(
    Phase.assignUniqueSymbols,
    Phase.inferTypes,
    new InsertCompiler(InsertCompiler.NonAutoInc),
    new JdbcInsertCodeGen(createInsertBuilder) {
      override lazy val mappingCompiler = self.mappingCompiler
    }
  )
  lazy val forceInsertCompiler: QueryCompiler = QueryCompiler(
    Phase.assignUniqueSymbols,
    Phase.inferTypes,
    new InsertCompiler(InsertCompiler.AllColumns),
    new JdbcInsertCodeGen(createInsertBuilder) {
      override lazy val mappingCompiler = self.mappingCompiler
    }
  )
  lazy val upsertCompiler: QueryCompiler = QueryCompiler(
    Phase.assignUniqueSymbols,
    Phase.inferTypes,
    new InsertCompiler(InsertCompiler.AllColumns),
    new JdbcInsertCodeGen(createUpsertBuilder) {
      override lazy val mappingCompiler = self.mappingCompiler
    }
  )
  lazy val checkInsertCompiler = QueryCompiler(
    Phase.assignUniqueSymbols,
    Phase.inferTypes,
    new InsertCompiler(InsertCompiler.PrimaryKeys),
    new JdbcInsertCodeGen(createCheckInsertBuilder) {
      override lazy val mappingCompiler = self.mappingCompiler
    }
  )
  lazy val updateInsertCompiler = QueryCompiler(
    Phase.assignUniqueSymbols,
    Phase.inferTypes,
    new InsertCompiler(InsertCompiler.AllColumns),
    new JdbcInsertCodeGen(createUpdateInsertBuilder) {
      override lazy val mappingCompiler = self.mappingCompiler
    }
  )
  //def compileInsert(tree: Node) = new JdbcCompiledInsert(tree)

}