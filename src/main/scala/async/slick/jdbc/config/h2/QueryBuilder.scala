package slick.async.jdbc

import slick.ast.{ Library, Node, RowNumber, SequenceNode }
import slick.async.jdbc.config.H2Capabilities
import slick.compiler.CompilerState
import slick.util.MacroSupport.macroSupportInterpolation

abstract class H2QueryBuilder(tree: Node, state: CompilerState) extends QueryBuilder(tree, state) /*(new H2Capabilities {})*/ {
  override protected val concatOperator = Some("||")
  override protected val alwaysAliasSubqueries = false
  override protected val supportsLiteralGroupBy = true
  override protected val quotedJdbcFns = Some(Nil)

  override def expr(n: Node, skipParens: Boolean = false) = n match {
    case Library.NextValue(SequenceNode(name)) => b"nextval(schema(), '$name')"
    case Library.CurrentValue(SequenceNode(name)) => b"currval(schema(), '$name')"
    case RowNumber(_) => b"rownum"
    case _ => super.expr(n, skipParens)
  }

  override protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = (fetch, offset) match {
    case (Some(t), Some(d)) => b"\nlimit $t offset $d"
    case (Some(t), None) => b"\nlimit $t"
    case (None, Some(d)) => b"\nlimit -1 offset $d"
    case _ =>
  }
}