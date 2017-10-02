package slick.async.jdbc.config

import slick.ast._
import slick.async.jdbc.{ InsertBuilderResult, JdbcTypeHelper, OracleProfile, QueryBuilder }
import slick.compiler._
import slick.util.{ ConstArray, SQLBuilder }
import slick.ast.Util.nodeToNodeOps
import slick.async.relational.RelationalProfile
import slick.async.sql.SqlProfile
import slick.lifted.{ AbstractTable, RefTag, Rep, Tag }

trait CrudCompiler { self =>

  val compilerContent: RelationalQueryCompiler
  val sqlUtilsComponent: BasicSqlUtilsComponent

  def createInsertBuilder(node: Insert): InsertBuilder = new InsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
  def createUpsertBuilder(node: Insert): InsertBuilder = new UpsertBuilder(node) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }

  /*lazy val queryCompiler = compilerContent.computeQueryCompiler + new JdbcCodeGen(_.buildSelect)
  lazy val updateCompiler = compilerContent.computeQueryCompiler + new JdbcCodeGen(_.buildUpdate)
  lazy val deleteCompiler = compilerContent.computeQueryCompiler + new JdbcCodeGen(_.buildDelete)*/
  //lazy val insertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.NonAutoInc), new JdbcInsertCodeGen(createInsertBuilder))
  //lazy val forceInsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.AllColumns), new JdbcInsertCodeGen(createInsertBuilder))
  //lazy val upsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.AllColumns), new JdbcInsertCodeGen(createUpsertBuilder))
  /*lazy val checkInsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.PrimaryKeys), new JdbcInsertCodeGen(createCheckInsertBuilder))
  lazy val updateInsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.AllColumns), new JdbcInsertCodeGen(createUpdateInsertBuilder))
  def compileInsert(tree: Node) = new JdbcCompiledInsert(tree)*/

}

/** Code generator phase for queries on JdbcProfile. */
/*
class JdbcCodeGen(f: QueryBuilder => SQLBuilder.Result) extends CodeGen {
  def compileServerSideAndMapping(serverSide: Node, mapping: Option[Node], state: CompilerState) = {
    val (tree, tpe) = treeAndType(serverSide)
    val sbr = f(self.createQueryBuilder(tree, state))
    (CompiledStatement(sbr.sql, sbr, tpe).infer(), mapping.map(mappingCompiler.compileMapping))
  }
}

//TODO 准备删除
/** Code generator phase for inserts on JdbcProfile. */
class JdbcInsertCodeGen(f: Insert => InsertBuilder) extends CodeGen {
  def compileServerSideAndMapping(serverSide: Node, mapping: Option[Node], state: CompilerState) = {
    val ib = f(serverSide.asInstanceOf[Insert])
    val ibr = ib.buildInsert
    (CompiledStatement(ibr.sql, ibr, serverSide.nodeType).infer(), mapping.map(n => mappingCompiler.compileMapping(ib.transformMapping(n))))
  }
}*/
