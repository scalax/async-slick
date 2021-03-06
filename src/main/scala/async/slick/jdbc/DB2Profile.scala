package slick.async.jdbc

import scala.concurrent.ExecutionContext
import slick.ast._
import slick.async.dbio._
import slick.async.jdbc.config._
import slick.async.jdbc.meta.MTable
import slick.async.relational.RelationalProfile

/**
 * Slick profile for IBM DB2 UDB.
 *
 * This profile implements [[slick.jdbc.JdbcProfile]]
 * ''without'' the following capabilities:
 *
 * <ul>
 *   <li>[[slick.relational.RelationalCapabilities.reverse]]:
 *     This String function is not available in DB2.</li>
 *   <li>[[slick.jdbc.JdbcCapabilities.insertOrUpdate]]:
 *     InsertOrUpdate operations are emulated on the client side if generated
 *     keys should be returned. Otherwise the operation is performmed
 *     natively on the server side.</li>
 *   <li>[[slick.jdbc.JdbcCapabilities.booleanMetaData]]:
 *     DB2 doesn't have booleans, so Slick maps to SMALLINT instead.
 *     Other JDBC drivers like MySQL map TINYINT(1) back to a Scala
 *     Boolean. DB2 maps SMALLINT to an Integer and that's how it shows
 *     up in the JDBC meta data, thus the original type is lost.</li>
 *   <li>[[slick.jdbc.JdbcCapabilities.supportsByte]]:
 *     DB2 does not have a BYTE type.</li>
 * </ul>
 *
 * Note: The DB2 JDBC driver has problems with quoted identifiers. Columns
 * which are returned from inserts must not require quoted names (in
 * particular, they must not contain lower-case characters or be equal to a
 * reserved word), otherwise a bug in the DB2 JDBC driver triggers a SQL
 * Error -206 (SQLState 42703).
 */
trait DB2Profile extends JdbcProfile { self =>

  /*override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - RelationalCapabilities.reverse
    - JdbcCapabilities.insertOrUpdate
    - JdbcCapabilities.supportsByte
    - JdbcCapabilities.booleanMetaData)*/

  override protected lazy val useServerSideUpsert = true
  override protected lazy val useServerSideUpsertReturning = false
  //override protected val invokerMutateType: ResultSetType = ResultSetType.ScrollSensitive
  override val jdbcInvokerComponent: JdbcInvokerComponent = new DB2InvokerComponent {}

  override lazy val capabilitiesContent: BasicCapabilities = new DB2Capabilities {}

  override protected def computeQueryCompiler = new DB2QueryCompiler {}
  /*(super.computeQueryCompiler.addAfter(Phase.removeTakeDrop, Phase.expandSums)
      + Phase.rewriteBooleans)*/
  //override val columnTypes = new DB2JdbcTypes {}
  /*override def createQueryBuilder(n: Node, state: CompilerState): slick.async.jdbc.QueryBuilder = new DB2QueryBuilder(n, state) {
    override lazy val commonCapabilities = self.capabilitiesContent
    override lazy val sqlUtilsComponent = self.sqlUtilsComponent
  }*/
  override lazy val crudCompiler = new DB2CrudCompiler {
    override val sqlUtilsComponent = self.sqlUtilsComponent
    override val compilerContent = self.computeQueryCompiler
    override lazy val capabilitiesContent = self.capabilitiesContent
    override val scalarFrom = self.scalarFrom
  }
  override def createTableDDLBuilder(table: RelationalProfile#Table[_]): TableDDLBuilder = new DB2TableDDLBuilder(table) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
    override def createColumnDDLBuilder(column: FieldSymbol, table: RelationalProfile#Table[_]) = {
      self.createColumnDDLBuilder(column, table)
    }
  }
  override def createColumnDDLBuilder(column: FieldSymbol, table: RelationalProfile#Table[_]): ColumnDDLBuilder = new DB2ColumnDDLBuilder(column) {
    override val sqlUtilsComponent = self.sqlUtilsComponent
  }
  //TODO schema 方法未做数据库多样性处理
  override def createSequenceDDLBuilder(seq: Sequence[_]): DB2SequenceDDLBuilder[_] = {
    def create1[T](seq1: Sequence[T]): DB2SequenceDDLBuilder[T] = new DB2SequenceDDLBuilder(seq1) {
      override val sqlUtilsComponent = self.sqlUtilsComponent
    }
    create1(seq)
  }
  override val api: API with DB2JdbcTypes = new API with DB2JdbcTypes {}

  override def defaultTables(implicit ec: ExecutionContext): DBIO[Seq[MTable]] =
    MTable.getTables(None, None, None, Some(Seq("TABLE"))).map(_.filter(_.name.schema.filter(_ == "SYSTOOLS").isEmpty))

  override def defaultSqlTypeName(tmd: JdbcType[_], sym: Option[FieldSymbol]): String = tmd.sqlType match {
    case java.sql.Types.TINYINT => "SMALLINT" // DB2 has no smaller binary integer type
    case _ => super.defaultSqlTypeName(tmd, sym)
  }

  override def createModelBuilder(tables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext): JdbcModelBuilder =
    new JdbcModelBuilder(tables, ignoreInvalidDefaults)

  override val scalarFrom = Some("sysibm.sysdummy1")

  /*class QueryBuilder(tree: Node, state: CompilerState) extends super.QueryBuilder(tree, state) {

    override protected val hasPiFunction = false
    override protected val hasRadDegConversion = false
    override protected val pi = "decfloat(3.1415926535897932384626433832)"

    override def expr(c: Node, skipParens: Boolean = false): Unit = c match {
      case RowNumber(by) =>
        b += "row_number() over("
        if (!by.isEmpty) buildOrderByClause(by)
        b += ")"
      case Library.IfNull(l, r) =>
        /* DB2 does not support IFNULL so we use COALESCE instead */
        b += "coalesce("; expr(l, true); b += ","; expr(r, true); b += ")"
      case Library.NextValue(SequenceNode(name)) => b += "(next value for " += quoteIdentifier(name) += ")"
      case Library.CurrentValue(SequenceNode(name)) => b += "(prevval for " += quoteIdentifier(name) += ")"
      case Library.User() => b += "current user"
      case Library.Database() => b += "current server"
      case Library.CountAll(LiteralNode(1)) => b"count(*)"
      case _ => super.expr(c, skipParens)
    }

    override protected def buildOrdering(n: Node, o: Ordering) {
      /* DB2 does not have explicit NULLS FIST/LAST clauses. Nulls are
       * sorted after non-null values by default. */
      if (o.nulls.first && !o.direction.desc) {
        b += "case when ("
        expr(n)
        b += ") is null then 0 else 1 end,"
      } else if (o.nulls.last && o.direction.desc) {
        b += "case when ("
        expr(n)
        b += ") is null then 1 else 0 end,"
      }
      expr(n)
      if (o.direction.desc) b += " desc"
    }

    override protected def buildForUpdateClause(forUpdate: Boolean) = {
      super.buildForUpdateClause(forUpdate)
      if (forUpdate) {
        b" with RS "
      }
    }
  }*/
  /*class TableDDLBuilder(table: RelationalProfile#Table[_]) extends super.TableDDLBuilder(table) {
    override protected def createIndex(idx: Index) = {
      if (idx.unique) {
        /* Create a UNIQUE CONSTRAINT (with an automatically generated backing
         * index) because DB2 does not allow a FOREIGN KEY CONSTRAINT to
         * reference columns which have a UNIQUE INDEX but not a nominal UNIQUE
         * CONSTRAINT. */
        val sb = new StringBuilder append "ALTER TABLE " append sqlUtilsComponent.quoteIdentifier(table.tableName) append " ADD "
        sb append "CONSTRAINT " append sqlUtilsComponent.quoteIdentifier(idx.name) append " UNIQUE("
        addIndexColumnList(idx.on, sb, idx.table.tableName)
        sb append ")"
        sb.toString
      } else super.createIndex(idx)
    }

    //For compatibility with all versions of DB2
    //http://stackoverflow.com/questions/3006999/sql-query-to-truncate-table-in-ibm-db2
    override def truncateTable = s"DELETE FROM ${sqlUtilsComponent.quoteTableName(tableNode)}"
  }*/
  /*class ColumnDDLBuilder(column: FieldSymbol) extends super.ColumnDDLBuilder(column) {
    override def appendColumn(sb: StringBuilder) {
      val qname = sqlUtilsComponent.quoteIdentifier(column.name)
      sb append qname append ' '
      appendType(sb)
      appendOptions(sb)
      if (jdbcType.isInstanceOf[JdbcTypes#BooleanJdbcType]) {
        sb append " constraint " + sqlUtilsComponent.quoteIdentifier(column.name + "__bool") + " check (" append qname append " in (0, 1))"
      }
    }
  }*/
  /*class SequenceDDLBuilder[T](seq: Sequence[T]) extends super.SequenceDDLBuilder(seq) {
    override def buildDDL: DDL = {
      val b = new StringBuilder append "create sequence " append sqlUtilsComponent.quoteIdentifier(seq.name)
      b append " as " append JdbcTypeHelper.jdbcTypeFor(seq.tpe).sqlTypeName(None)
      seq._start.foreach { b append " start with " append _ }
      seq._increment.foreach { b append " increment by " append _ }
      seq._minValue.foreach { b append " minvalue " append _ }
      seq._maxValue.foreach { b append " maxvalue " append _ }
      if (seq._cycle) b append " cycle"
      DDL(b.toString, "drop sequence " + sqlUtilsComponent.quoteIdentifier(seq.name))
    }
  }*/
  /*class JdbcTypes extends super.JdbcTypes {
    override val booleanJdbcType = new BooleanJdbcType
    override val uuidJdbcType = new UUIDJdbcType

    class UUIDJdbcType extends super.UUIDJdbcType {
      override def sqlType = java.sql.Types.CHAR
      override def sqlTypeName(sym: Option[FieldSymbol]) = "CHAR(16) FOR BIT DATA"
    }

    /* DB2 does not have a proper BOOLEAN type. The suggested workaround is
     * a constrained CHAR with constants 1 and 0 for TRUE and FALSE. */
    class BooleanJdbcType extends super.BooleanJdbcType {
      override def sqlTypeName(sym: Option[FieldSymbol]) = "CHAR(1)"
      override def valueToSQLLiteral(value: Boolean) = if (value) "1" else "0"
    }
  }*/
}

object DB2Profile extends DB2Profile
