package slick.async.jdbc.config

import slick.SlickException
import slick.ast._
import slick.async.jdbc.{ InsertBuilderResult, JdbcResultConverterDomain }
import slick.compiler.QueryCompiler
import slick.async.jdbc.JdbcCapabilities
import slick.relational.{ CompiledMapping, ResultConverter }
import slick.util.ConstArray
import scala.language.existentials

abstract class JdbcCompiledInsert(source: Node) {
  val crudCompiler: CrudCompiler
  val capabilitiesContent: BasicCapabilities

  class Artifacts(val compiled: Node, val converter: ResultConverter[JdbcResultConverterDomain, Any], val ibr: InsertBuilderResult) {
    def table: TableNode = ibr.table
    def sql: String = ibr.sql
    def fields: ConstArray[FieldSymbol] = ibr.fields
  }

  protected[this] def compile(compiler: QueryCompiler): Artifacts = {
    val compiled = compiler.run(source).tree
    val ResultSetMapping(_, CompiledStatement(sql, ibr: InsertBuilderResult, _), CompiledMapping(conv, _)) = compiled
    new Artifacts(compiled, conv.asInstanceOf[ResultConverter[JdbcResultConverterDomain, Any]], ibr)
  }

  /** The compiled artifacts for standard insert statements. */
  lazy val standardInsert = compile(crudCompiler.insertCompiler)

  /** The compiled artifacts for forced insert statements. */
  lazy val forceInsert = compile(crudCompiler.forceInsertCompiler)

  /** The compiled artifacts for upsert statements. */
  lazy val upsert = compile(crudCompiler.upsertCompiler)

  /** The compiled artifacts for 'check insert' statements. */
  lazy val checkInsert = compile(crudCompiler.checkInsertCompiler)

  /** The compiled artifacts for 'update insert' statements. */
  lazy val updateInsert = compile(crudCompiler.updateInsertCompiler)

  /** Build a list of columns and a matching `ResultConverter` for retrieving keys of inserted rows. */
  def buildReturnColumns(node: Node): (ConstArray[String], ResultConverter[JdbcResultConverterDomain, _], Boolean) = {
    if (!capabilitiesContent.capabilities.contains(JdbcCapabilities.returnInsertKey))
      throw new SlickException("This DBMS does not allow returning columns from INSERT statements")
    val ResultSetMapping(_, CompiledStatement(_, ibr: InsertBuilderResult, _), CompiledMapping(rconv, _)) =
      crudCompiler.forceInsertCompiler.run(node).tree
    if (ibr.table.baseIdentity != standardInsert.table.baseIdentity)
      throw new SlickException("Returned key columns must be from same table as inserted columns (" +
        ibr.table.baseIdentity + " != " + standardInsert.table.baseIdentity + ")")
    val returnOther = ibr.fields.length > 1 || !ibr.fields.head.options.contains(ColumnOption.AutoInc)
    if (!capabilitiesContent.capabilities.contains(JdbcCapabilities.returnInsertOther) && returnOther)
      throw new SlickException("This DBMS allows only a single column to be returned from an INSERT, and that column must be an AutoInc column.")
    (ibr.fields.map(_.name), rconv.asInstanceOf[ResultConverter[JdbcResultConverterDomain, _]], returnOther)
  }
}