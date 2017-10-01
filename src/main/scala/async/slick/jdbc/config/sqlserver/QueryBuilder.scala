package slick.async.jdbc

import slick.ast.{ Comprehension, Library, LiteralNode, Node, Ordering, QueryParameter, TermSymbol }
import slick.async.jdbc.config.SQLServerCapabilities
import slick.compiler.CompilerState
import slick.util.MacroSupport.macroSupportInterpolation

class SQLServerQueryBuilder(tree: Node, state: CompilerState) extends QueryBuilder(tree, state)(new SQLServerCapabilities {}) {
  val columnTypes = new SQLServerJdbcTypes {}

  override protected val supportsTuples = false
  override protected val concatOperator = Some("+")

  override protected def buildSelectModifiers(c: Comprehension) {
    super.buildSelectModifiers(c)
    (c.fetch, c.offset) match {
      case (Some(t), Some(d)) => b"top (${QueryParameter.constOp[Long]("+")(_ + _)(t, d)}) "
      case (Some(t), None) => b"top ($t) "
      case (None, _) => if (!c.orderBy.isEmpty) b"top 100 percent "
    }
  }

  override protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = ()

  override protected def buildOrdering(n: Node, o: Ordering) {
    if (o.nulls.last && !o.direction.desc)
      b"case when ($n) is null then 1 else 0 end,"
    else if (o.nulls.first && o.direction.desc)
      b"case when ($n) is null then 0 else 1 end,"
    expr(n)
    if (o.direction.desc) b" desc"
  }

  override protected def buildFromClause(from: Seq[(TermSymbol, Node)]) = {
    super.buildFromClause(from)
    tree match {
      // SQL Server "select for update" syntax
      case c: Comprehension => if (c.forUpdate) b" with (updlock,rowlock) "
      case _ =>
    }
  }

  override protected def buildForUpdateClause(forUpdate: Boolean) = {
    // SQLSever doesn't have "select for update" syntax, so use with (updlock,rowlock) in from clause
  }

  override def expr(n: Node, skipParens: Boolean = false): Unit = n match {
    // Cast bind variables of type TIME to TIME (otherwise they're treated as TIMESTAMP)
    case c @ LiteralNode(v) if c.volatileHint && JdbcTypeHelper.jdbcTypeFor(c.nodeType) == columnTypes.timeJdbcType =>
      b"cast("
      super.expr(n, skipParens)
      b" as ${columnTypes.timeJdbcType.sqlTypeName(None)})"
    case QueryParameter(extractor, tpe, _) if JdbcTypeHelper.jdbcTypeFor(tpe) == columnTypes.timeJdbcType =>
      b"cast("
      super.expr(n, skipParens)
      b" as ${columnTypes.timeJdbcType.sqlTypeName(None)})"
    case Library.Substring(n, start) =>
      b"\({fn substring($n, ${QueryParameter.constOp[Int]("+")(_ + _)(start, LiteralNode(1).infer())}, ${Int.MaxValue})}\)"
    case Library.Repeat(str, count) =>
      b"replicate($str, $count)"
    case n => super.expr(n, skipParens)
  }
}