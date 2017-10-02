package slick.async.jdbc.config

import slick.ast._
import slick.async.jdbc.{ InsertBuilderResult, JdbcTypeHelper, OracleProfile }
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
  lazy val deleteCompiler = compilerContent.computeQueryCompiler + new JdbcCodeGen(_.buildDelete)
  lazy val insertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.NonAutoInc), new JdbcInsertCodeGen(createInsertBuilder))
  lazy val forceInsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.AllColumns), new JdbcInsertCodeGen(createInsertBuilder))
  lazy val upsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.AllColumns), new JdbcInsertCodeGen(createUpsertBuilder))
  lazy val checkInsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.PrimaryKeys), new JdbcInsertCodeGen(createCheckInsertBuilder))
  lazy val updateInsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.AllColumns), new JdbcInsertCodeGen(createUpdateInsertBuilder))
  def compileInsert(tree: Node) = new JdbcCompiledInsert(tree)*/

}

/** Builder for INSERT statements. */
abstract class InsertBuilder(val ins: Insert) {
  val sqlUtilsComponent: BasicSqlUtilsComponent

  protected val Insert(_, table: TableNode, ProductNode(rawColumns), allFields) = ins
  protected val syms: ConstArray[FieldSymbol] = rawColumns.map { case Select(_, fs: FieldSymbol) => fs }
  protected lazy val allNames = syms.map(fs => sqlUtilsComponent.quoteIdentifier(fs.name))
  protected lazy val allVars = syms.iterator.map(_ => "?").mkString("(", ",", ")")
  protected lazy val tableName = sqlUtilsComponent.quoteTableName(table)

  def buildInsert: InsertBuilderResult = {
    val start = buildInsertStart
    if (syms.isEmpty) new InsertBuilderResult(table, emptyInsert, syms)
    else new InsertBuilderResult(table, s"$start values $allVars", syms) {
      override def buildInsert(compiledQuery: Node) = {
        val (_, sbr: SQLBuilder.Result) = CodeGen.findResult(compiledQuery)
        SQLBuilder.Result(start + sbr.sql, sbr.setter)
      }
    }
  }

  def transformMapping(n: Node) = n

  protected def buildInsertStart: String = allNames.iterator.mkString(s"insert into $tableName (", ",", ") ")

  protected def emptyInsert: String =
    if (allFields.isEmpty) s"insert into $tableName default values"
    else s"insert into $tableName (${sqlUtilsComponent.quoteIdentifier(allFields.head.name)}) values (default)"

  /**
   * Reorder InsertColumn indices in a mapping Node in the order of the given
   * sequence of FieldSymbols (which may contain duplicates).
   */
  protected def reorderColumns(n: Node, order: IndexedSeq[FieldSymbol]): Node = {
    val newIndices = order.zipWithIndex.groupBy(_._1)
    lazy val reordering: ConstArray[IndexedSeq[Int]] = syms.map(fs => newIndices(fs).map(_._2 + 1))
    n.replace({
      case InsertColumn(ConstArray(Select(ref, ElementSymbol(idx))), fs, tpe) =>
        val newPaths = reordering(idx - 1).map(i => Select(ref, ElementSymbol(i)))
        InsertColumn(ConstArray.from(newPaths), fs, tpe) :@ tpe
    }, keepType = true)
  }
}

abstract class UpsertBuilder(ins: Insert) extends InsertBuilder(ins) {
  val sqlUtilsComponent: BasicSqlUtilsComponent

  /* NOTE: pk defined by using method `primaryKey` and pk defined with `PrimaryKey` can only have one,
           here we let table ddl to help us ensure this. */
  private lazy val funcDefinedPKs = table.profileTable.asInstanceOf[ProfileTable[_]].primaryKeys
  protected lazy val (pkSyms, softSyms) = syms.toSeq.partition { sym =>
    sym.options.contains(ColumnOption.PrimaryKey) || funcDefinedPKs.exists(pk => pk.columns.collect {
      case Select(_, f: FieldSymbol) => f
    }.exists(_.name == sym.name))
  }
  protected lazy val pkNames = pkSyms.map { fs => sqlUtilsComponent.quoteIdentifier(fs.name) }
  protected lazy val softNames = softSyms.map { fs => sqlUtilsComponent.quoteIdentifier(fs.name) }
  protected lazy val nonAutoIncSyms = syms.filter(s => !(s.options contains ColumnOption.AutoInc))
  protected lazy val nonAutoIncNames = nonAutoIncSyms.map(fs => sqlUtilsComponent.quoteIdentifier(fs.name))

  override def buildInsert: InsertBuilderResult = {
    val start = buildMergeStart
    val end = buildMergeEnd
    val paramSel = "select " + allNames.map(n => "? as " + n).iterator.mkString(",") + JdbcTypeHelper.scalarFrom.map(n => " from " + n).getOrElse("")
    // We'd need a way to alias the column names at the top level in order to support merges from a source Query
    new InsertBuilderResult(table, start + paramSel + end, syms)
  }

  protected def buildMergeStart: String = s"merge into $tableName t using ("

  protected def buildMergeEnd: String = {
    val updateCols = softNames.map(n => s"t.$n=s.$n").mkString(", ")
    val insertCols = nonAutoIncNames /*.map(n => s"t.$n")*/ .mkString(", ")
    val insertVals = nonAutoIncNames.map(n => s"s.$n").mkString(", ")
    val cond = pkNames.map(n => s"t.$n=s.$n").mkString(" and ")
    s") s on ($cond) when matched then update set $updateCols when not matched then insert ($insertCols) values ($insertVals)"
  }
}

abstract class ProfileTable[T](_tableTag: Tag, _schemaName: Option[String], _tableName: String) extends AbstractTable[T](_tableTag, _schemaName, _tableName) { table =>
  final type TableElementType = T

  def this(_tableTag: Tag, _tableName: String) = this(_tableTag, None, _tableName)

  def tableProvider: RelationalProfile

  def tableIdentitySymbol: TableIdentitySymbol = SimpleTableIdentitySymbol(tableProvider, schemaName.getOrElse("_"), tableName)

  val O: RelationalColumnOptions //: self.columnOptions.type = columnOptions

  /**
   * Note that Slick uses VARCHAR or VARCHAR(254) in DDL for String
   * columns if neither ColumnOption DBType nor Length are given.
   */
  def column[C](n: String, options: ColumnOption[C]*)(implicit tt: TypedType[C]): Rep[C] = {
    if (tt == null) throw new NullPointerException(
      "implicit TypedType[C] for column[C] is null. " +
        "This may be an initialization order problem. " +
        "When using a MappedColumnType, you may want to change it from a val to a lazy val or def."
    )
    new Rep.TypedRep[C] {
      override def toNode =
        Select((tableTag match {
          case r: RefTag => r.path
          case _ => tableNode
        }), FieldSymbol(n)(options, tt)) :@ tt
      override def toString = (tableTag match {
        case r: RefTag => "(" + _tableName + " " + r.path + ")"
        case _ => _tableName
      }) + "." + n
    }
  }
}

trait RelationalColumnOptions {
  val PrimaryKey = ColumnOption.PrimaryKey
  def Default[T](defaultValue: T) = RelationalProfile.ColumnOption.Default[T](defaultValue)
  val AutoInc = ColumnOption.AutoInc
  val Unique = ColumnOption.Unique
  val Length = RelationalProfile.ColumnOption.Length
}

trait SqlTableColumnOptions extends RelationalColumnOptions {
  def SqlType(typeName: String) = SqlProfile.ColumnOption.SqlType(typeName)
}

trait OracleColumnOptions extends SqlTableColumnOptions {
  def AutoIncSequenceName(name: String) = OracleProfile.ColumnOption.AutoIncSequenceName(name)
  def AutoIncTriggerName(name: String) = OracleProfile.ColumnOption.AutoIncTriggerName(name)
}