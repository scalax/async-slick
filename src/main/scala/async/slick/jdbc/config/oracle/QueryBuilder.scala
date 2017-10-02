package slick.async.jdbc

import java.sql.Types

import slick.ast._
import slick.async.jdbc.config.OracleCapabilities
import slick.compiler.CompilerState
import slick.util.MacroSupport.macroSupportInterpolation

abstract class OracleQueryBuilder(tree: Node, state: CompilerState) extends QueryBuilder(tree, state) /*(new OracleCapabilities {})*/ {
  override protected val supportsTuples = false
  override protected val concatOperator = Some("||")
  override protected val hasPiFunction = false
  /* Oracle officially supports {fn degrees} and {fn radians} but
   * Statement.execute throws a NullPointerException when you try to use
   * these functions. */
  override protected val hasRadDegConversion = false

  override def expr(c: Node, skipParens: Boolean = false): Unit = c match {
    case RowNumber(_) => b"rownum"
    case Library.NextValue(SequenceNode(name)) => b += sqlUtilsComponent.quoteIdentifier(name) += ".nextval"
    case Library.CurrentValue(SequenceNode(name)) => b += sqlUtilsComponent.quoteIdentifier(name) += ".currval"
    case Library.Database() => b += "ORA_DATABASE_NAME"
    case Library.Repeat(s, n) => b"RPAD($s, LENGTH($s)*$n, $s)"
    case Library.==(left: ProductNode, right: ProductNode) => //TODO
      b"\("
      val cols = (left.children zip right.children).force
      b.sep(cols, " and ") { case (l, r) => expr(Library.==.typed[Boolean](l, r)) }
      b"\)"
    case Library.==(l, r) if (l.nodeType != UnassignedType) && JdbcTypeHelper.jdbcTypeFor(l.nodeType).sqlType == Types.BLOB =>
      b"\(dbms_lob.compare($l, $r) = 0\)"
    case _ => super.expr(c, skipParens)
  }
}