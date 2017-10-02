package slick.async.jdbc.config

import slick.ast._
import slick.async.jdbc.{ InsertBuilderResult, JdbcTypeHelper }
import slick.compiler._
import slick.util.{ ConstArray, SQLBuilder }
import slick.ast.Util.nodeToNodeOps

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