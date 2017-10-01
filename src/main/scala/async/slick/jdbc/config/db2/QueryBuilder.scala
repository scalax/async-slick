package slick.async.jdbc

import slick.ast.{ Library, LiteralNode, Node, Ordering, RowNumber, SequenceNode }
import slick.compiler.CompilerState
import slick.util.MacroSupport.macroSupportInterpolation
import slick.async.jdbc.config.DB2Capabilities

class DB2QueryBuilder(tree: Node, state: CompilerState) extends QueryBuilder(tree, state)(new DB2Capabilities {}) {

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
}