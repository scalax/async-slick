package slick.async.jdbc

import slick.SlickException
import slick.ast.{ Library, LiteralNode, Node, Ordering, QueryParameter, RowNumber }
import slick.compiler.CompilerState
import slick.util.MacroSupport.macroSupportInterpolation
import slick.async.jdbc.config.{ DB2Capabilities, SQLiteCapabilities }

class SQLiteQueryBuilder(tree: Node, state: CompilerState) extends QueryBuilder(tree, state)(new SQLiteCapabilities {}) {

  override protected val supportsTuples = false
  override protected val concatOperator = Some("||")
  override protected val parenthesizeNestedRHSJoin = true
  override protected val alwaysAliasSubqueries = false
  override protected val quotedJdbcFns = Some(Nil)

  override protected def buildOrdering(n: Node, o: Ordering) {
    if (o.nulls.last && !o.direction.desc)
      b"($n) is null,"
    else if (o.nulls.first && o.direction.desc)
      b"($n) is null desc,"
    expr(n)
    if (o.direction.desc) b" desc"
  }

  override protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = (fetch, offset) match {
    case (Some(t), Some(d)) => b"\nlimit $d,$t"
    case (Some(t), None) => b"\nlimit $t"
    case (None, Some(d)) => b"\nlimit $d,-1"
    case _ =>
  }

  override def expr(c: Node, skipParens: Boolean = false): Unit = c match {
    case Library.UCase(ch) => b"upper(!$ch)"
    case Library.LCase(ch) => b"lower(!$ch)"
    case Library.Substring(n, start, end) =>
      b"substr($n, ${QueryParameter.constOp[Int]("+")(_ + _)(start, LiteralNode(1).infer())}, ${QueryParameter.constOp[Int]("-")(_ - _)(end, start)})"
    case Library.Substring(n, start) =>
      b"substr($n, ${QueryParameter.constOp[Int]("+")(_ + _)(start, LiteralNode(1).infer())})\)"
    case Library.IndexOf(n, str) => b"\(charindex($str, $n) - 1\)"
    case Library.%(l, r) => b"\($l%$r\)"
    case Library.Ceiling(ch) => b"round($ch+0.5)"
    case Library.Floor(ch) => b"round($ch-0.5)"
    case Library.User() => b"''"
    case Library.Database() => b"''"
    case RowNumber(_) => throw new SlickException("SQLite does not support row numbers")
    // https://github.com/jOOQ/jOOQ/issues/1595
    case Library.Repeat(n, times) => b"replace(substr(quote(zeroblob(($times + 1) / 2)), 3, $times), '0', $n)"
    case _ => super.expr(c, skipParens)
  }
}