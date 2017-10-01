package slick.async.jdbc

import slick.ast._
import slick.async.jdbc.config.PostgresCapabilities
import slick.compiler.CompilerState
import slick.util.MacroSupport.macroSupportInterpolation
import slick.util.ConstArray

class PostgresQueryBuilder(tree: Node, state: CompilerState) extends QueryBuilder(tree, state)(new PostgresCapabilities {}) {
  override protected val concatOperator = Some("||")
  override protected val quotedJdbcFns = Some(Vector(Library.Database, Library.User))

  override protected def buildSelectModifiers(c: Comprehension): Unit = (c.distinct, c.select) match {
    case (Some(ProductNode(onNodes)), Pure(ProductNode(selNodes), _)) if onNodes.nonEmpty =>
      def eligible(a: ConstArray[Node]) = a.forall {
        case _: PathElement => true
        case _: LiteralNode => true
        case _: QueryParameter => true
        case _ => false
      }
      if (eligible(onNodes) && eligible(selNodes) &&
        onNodes.iterator.collect[List[TermSymbol]] { case FwdPath(ss) => ss }.toSet ==
        selNodes.iterator.collect[List[TermSymbol]] { case FwdPath(ss) => ss }.toSet) b"distinct " else super.buildSelectModifiers(c)
    case _ => super.buildSelectModifiers(c)
  }

  override protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = (fetch, offset) match {
    case (Some(t), Some(d)) => b"\nlimit $t offset $d"
    case (Some(t), None) => b"\nlimit $t"
    case (None, Some(d)) => b"\noffset $d"
    case _ =>
  }

  override def expr(n: Node, skipParens: Boolean = false) = n match {
    case Library.UCase(ch) => b"upper($ch)"
    case Library.LCase(ch) => b"lower($ch)"
    case Library.IfNull(ch, d) => b"coalesce($ch, $d)"
    case Library.NextValue(SequenceNode(name)) => b"nextval('$name')"
    case Library.CurrentValue(SequenceNode(name)) => b"currval('$name')"
    case Library.CurrentDate() => b"current_date"
    case Library.CurrentTime() => b"current_time"
    case _ => super.expr(n, skipParens)
  }
}