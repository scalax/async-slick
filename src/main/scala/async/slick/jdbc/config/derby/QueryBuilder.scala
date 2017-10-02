package slick.async.jdbc

import slick.SlickException
import slick.ast.TypeUtil.:@
import slick.ast._
import slick.compiler.CompilerState
import slick.util.MacroSupport.macroSupportInterpolation
import slick.async.jdbc.config.{ DB2Capabilities, DerbyCapabilities }

abstract class DerbyQueryBuilder(tree: Node, state: CompilerState) extends QueryBuilder(tree, state) /*(new DerbyCapabilities {})*/ {
  //TODO 看有没有可能省略
  val columnTypes = new DerbyJdbcTypes {}

  override protected val concatOperator = Some("||")
  override protected val supportsTuples = false
  override protected val supportsLiteralGroupBy = true
  override protected val quotedJdbcFns = Some(Vector(Library.User))

  override protected def buildForUpdateClause(forUpdate: Boolean) = {
    super.buildForUpdateClause(forUpdate)
    if (forUpdate) {
      b" with RS "
    }
  }

  override def expr(c: Node, skipParens: Boolean = false): Unit = {
    import sqlUtilsComponent.quoteIdentifier
    c match {
      case Library.Cast(ch @ _*) =>
        /* Work around DERBY-2072 by casting numeric values first to CHAR and
         * then to VARCHAR. */
        val (toVarchar, tn) = {
          val tn =
            (if (ch.length == 2) ch(1).asInstanceOf[LiteralNode].value.asInstanceOf[String]
            else JdbcTypeHelper.jdbcTypeFor(c.nodeType).sqlTypeName(None)).toLowerCase
          if (tn == "varchar") (true, columnTypes.stringJdbcType.sqlTypeName(None))
          else if (tn.startsWith("varchar")) (true, tn)
          else (false, tn)
        }
        if (toVarchar && JdbcTypeHelper.jdbcTypeFor(ch(0).nodeType).isInstanceOf[NumericTypedType])
          b"trim(cast(cast(${ch(0)} as char(30)) as $tn))"
        else b"cast(${ch(0)} as $tn)"
      case Library.IfNull(l, r) =>
        /* Derby does not support IFNULL so we use COALESCE instead,
         * and it requires NULLs to be casted to a suitable type */
        b"coalesce(cast($l as ${JdbcTypeHelper.jdbcTypeFor(c.nodeType).sqlTypeName(None)}),!$r)"
      case Library.SilentCast(LiteralNode(None)) :@ JdbcTypeHelper(ti, _) if currentPart == SelectPart =>
        // Cast NULL to the correct type
        b"cast(null as ${ti.sqlTypeName(None)})"
      case LiteralNode(None) :@ JdbcTypeHelper(ti, _) if currentPart == SelectPart =>
        // Cast NULL to the correct type
        b"cast(null as ${ti.sqlTypeName(None)})"
      case (c @ LiteralNode(v)) :@ JdbcTypeHelper(ti, option) if currentPart == SelectPart =>
        /* The Derby embedded driver has a bug (DERBY-4671) which results in a
         * NullPointerException when using bind variables in a SELECT clause.
         * This should be fixed in Derby 10.6.1.1. The workaround is to add an
         * explicit type annotation (in the form of a CAST expression). */
        if (c.volatileHint || !ti.hasLiteralForm) {
          b"cast("
          b +?= { (p, idx, param) => if (option) ti.setOption(v.asInstanceOf[Option[Any]], p, idx) else ti.setValue(v, p, idx) }
          b" as ${ti.sqlTypeName(None)})"
        } else super.expr(c, skipParens)
      case Library.NextValue(SequenceNode(name)) => b"(next value for `$name)"
      case Library.CurrentValue(_*) => throw new SlickException("Derby does not support CURRVAL")
      case _ => super.expr(c, skipParens)
    }
  }
}