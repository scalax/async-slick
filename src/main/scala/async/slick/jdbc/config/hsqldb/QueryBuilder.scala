package slick.async.jdbc

import java.sql.Types

import slick.SlickException
import slick.ast._
import slick.async.jdbc.config.HsqldbCapabilities
import slick.compiler.CompilerState
import slick.util.MacroSupport.macroSupportInterpolation
import slick.util.ConstArray

class HsqldbQueryBuilder(tree: Node, state: CompilerState) extends QueryBuilder(tree, state)(new HsqldbCapabilities {}) {
  override protected val concatOperator = Some("||")
  override protected val alwaysAliasSubqueries = false
  override protected val supportsLiteralGroupBy = true
  override protected val quotedJdbcFns = Some(Nil)

  override def expr(c: Node, skipParens: Boolean = false): Unit = c match {
    case l @ LiteralNode(v: String) if (v ne null) && JdbcTypeHelper.jdbcTypeFor(l.nodeType).sqlType != Types.CHAR =>
      /* Hsqldb treats string literals as type CHARACTER and pads them with
       * spaces in some expressions, so we cast all string literals to
       * VARCHAR. The length is only 16M instead of 2^31-1 in order to leave
       * enough room for concatenating strings (which extends the size even if
       * it is not needed). */
      b"cast("
      super.expr(c)
      b" as varchar(16777216))"
    /* Hsqldb uses the SQL:2008 syntax for NEXTVAL */
    case Library.NextValue(SequenceNode(name)) => b"(next value for `$name)"
    case Library.CurrentValue(_*) => throw new SlickException("Hsqldb does not support CURRVAL")
    case RowNumber(_) => b"rownum()" // Hsqldb uses Oracle ROWNUM semantics but needs parens
    case _ => super.expr(c, skipParens)
  }

  override protected def buildJoin(j: Join): Unit = {
    /* Re-balance inner joins to the left because Hsqldb does not supported RHS nesting. Paths
     * into joined views have already been mapped to unique identifiers at this point, so we can
     * safely rearrange views. */
    j match {
      case Join(ls, rs, l, Join(ls2, rs2, l2, r2, JoinType.Inner, on2), JoinType.Inner, on) =>
        val on3 = (on, on2) match {
          case (a, LiteralNode(true)) => a
          case (LiteralNode(true), b) => b
          case (a, b) => Apply(Library.And, ConstArray(a, b))(UnassignedType)
        }
        buildJoin(Join(rs, rs2, Join(ls, ls2, l, l2, JoinType.Inner, LiteralNode(true)), r2, JoinType.Inner, on3))
      case j => super.buildJoin(j)
    }
  }

  override protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = (fetch, offset) match {
    case (Some(t), Some(d)) => b"\nlimit $t offset $d"
    case (Some(t), None) => b"\nlimit $t"
    case (None, Some(d)) => b"\noffset $d"
    case _ =>
  }
}