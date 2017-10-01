package slick.async.jdbc

import slick.ast.TypeUtil.:@
import slick.ast.{ Library, Node, Ordering, SequenceNode, Union }
import slick.async.jdbc.MySQLProfile.{ RowNum, RowNumGen }
import slick.async.jdbc.config.MysqlCapabilities
import slick.compiler.CompilerState
import slick.util.MacroSupport.macroSupportInterpolation

class MysqlQueryBuilder(tree: Node, state: CompilerState) extends QueryBuilder(tree, state)(new MysqlCapabilities {}) {

  val columnTypes = new MySQLJdbcTypes {}

  override protected val supportsCast = false
  override protected val parenthesizeNestedRHSJoin = true
  override protected val quotedJdbcFns = Some(Nil)

  override def expr(n: Node, skipParens: Boolean = false): Unit = n match {
    case Library.Cast(ch) :@ JdbcTypeHelper(ti, _) =>
      val tn = if (ti == columnTypes.stringJdbcType) "VARCHAR" else if (ti == columnTypes.bigDecimalJdbcType) "DECIMAL" else ti.sqlTypeName(None)
      b"\({fn convert(!${ch},$tn)}\)"
    case Library.NextValue(SequenceNode(name)) => b"`${name + "_nextval"}()"
    case Library.CurrentValue(SequenceNode(name)) => b"`${name + "_currval"}()"
    case RowNum(sym, true) => b"(@`$sym := @`$sym + 1)"
    case RowNum(sym, false) => b"@`$sym"
    case RowNumGen(sym, init) => b"@`$sym := $init"
    case Union(left, right, all) =>
      b"\{"
      buildFrom(left, None, false)
      if (all) b"\nunion all " else b"\nunion "
      buildFrom(right, None, false)
      b"\}"
    case _ => super.expr(n, skipParens)
  }

  override protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = (fetch, offset) match {
    case (Some(t), Some(d)) => b"\nlimit $d,$t"
    case (Some(t), None) => b"\nlimit $t"
    case (None, Some(d)) => b"\nlimit $d,18446744073709551615"
    case _ =>
  }

  override protected def buildOrdering(n: Node, o: Ordering) {
    if (o.nulls.last && !o.direction.desc)
      b"isnull($n),"
    else if (o.nulls.first && o.direction.desc)
      b"isnull($n) desc,"
    expr(n)
    if (o.direction.desc) b" desc"
  }

  // Override default DELETE FROM syntax in order to produce a more efficient
  // DELETE query for MySQL.
  //
  // Slick cannot directly handle multi-table DELETEs, i.e., using JOIN or
  // USING, but it can handle subqueries in the WHERE clause of a DELETE.
  // This is good except for the fact that MySQL doesn't know how to
  // optimize such semi-join subqueries to joins in single-table DELETE
  // queries. However, if the DELETE query is a multi-table DELETE, even if
  // on a single table, then something in MySQL kicks in and optimizes the
  // subquery to a more efficient JOIN. Further reading:
  //
  // - http://mysqlserverteam.com/multi-table-trick
  // - https://mariadb.com/kb/en/mariadb/semi-join-subquery-optimizations
  //
  override protected def buildDeleteFrom(tableName: String): Unit = {
    b"delete $tableName from $tableName"
  }
}