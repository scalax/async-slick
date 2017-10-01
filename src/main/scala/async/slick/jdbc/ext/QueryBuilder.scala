package slick.async.jdbc

import slick.SlickException
import slick.ast.TypeUtil.:@
import slick.ast.{ Apply, Comprehension, ElementSymbol, IfThenElse, Join, JoinType, Library, LiteralNode, Node, OptionApply, Ordering, Path, ProductNode, Pure, QueryParameter, Ref, RowNumber, ScalaBaseType, Select, StructNode, TableNode, TermSymbol, Type, Union }
import slick.async.sql.SqlUtilsComponent
import slick.compiler.{ CompilerState, RewriteBooleans }
import slick.lifted.{ SimpleBinaryOperator, SimpleFunction, SimpleLiteral }
import slick.relational.RelationalCapabilities
import slick.util._
import slick.util.MacroSupport.macroSupportInterpolation
import slick.ast.Util.nodeToNodeOps
import slick.async.jdbc.config.CommonCapabilities
//import slick.basic.Capability

import scala.collection.mutable.HashMap

abstract class StatementPart
case object SelectPart extends StatementPart
case object FromPart extends StatementPart
case object WherePart extends StatementPart
case object HavingPart extends StatementPart
case object OtherPart extends StatementPart

class QueryBuilder(val tree: Node, val state: CompilerState)(implicit commonCapabilities: CommonCapabilities) extends SqlUtilsComponent { queryBuilder =>

  // Immutable config options (to be overridden by subclasses)
  protected val supportsTuples = true
  protected val supportsCast = true
  protected val concatOperator: Option[String] = None
  protected val hasPiFunction = true
  protected val hasRadDegConversion = true
  protected val parenthesizeNestedRHSJoin = false
  protected val pi = "3.1415926535897932384626433832795"
  protected val alwaysAliasSubqueries = true
  protected val supportsLiteralGroupBy = false
  protected val quotedJdbcFns: Option[Seq[Library.JdbcFunction]] = None // quote all by default

  // Mutable state accessible to subclasses
  protected val b = new SQLBuilder
  protected var currentPart: StatementPart = OtherPart
  protected val symbolName = new QuotingSymbolNamer(Some(state.symbolNamer))
  protected val joins = new HashMap[TermSymbol, Join]
  protected var currentUniqueFrom: Option[TermSymbol] = None

  //TODO jdbc 的实现还没搬过来
  //val capabilities: Set[Capability] = Set.empty

  def sqlBuilder = b

  final def buildSelect(): SQLBuilder.Result = {
    expr(tree, true)
    b.build
  }

  @inline protected final def building(p: StatementPart)(f: => Unit): Unit = {
    val oldPart = currentPart
    currentPart = p
    f
    currentPart = oldPart
  }

  protected def buildComprehension(c: Comprehension): Unit = {
    val limit0 = c.fetch match {
      case Some(LiteralNode(0L)) => true
      case _ => false
    }
    scanJoins(ConstArray((c.sym, c.from)))
    val (from, on) = flattenJoins(c.sym, c.from)
    val oldUniqueFrom = currentUniqueFrom
    def containsSymbolInSubquery(s: TermSymbol) =
      c.children.iterator.drop(1).flatMap(_.collect { case c: Comprehension => c }.toSeq.flatMap(_.findNode(_ == Ref(s)))).nonEmpty
    currentUniqueFrom = from match {
      case Seq((s, _: TableNode)) if !containsSymbolInSubquery(s) => Some(s)
      case Seq((s, _)) if !alwaysAliasSubqueries && !containsSymbolInSubquery(s) => Some(s)
      case _ => None
    }
    buildSelectClause(c)
    buildFromClause(from)
    if (limit0) b"\nwhere 1=0"
    else buildWhereClause(and(c.where.toSeq ++ on))
    buildGroupByClause(c.groupBy)
    buildHavingClause(c.having)
    buildOrderByClause(c.orderBy)
    if (!limit0) buildFetchOffsetClause(c.fetch, c.offset)
    buildForUpdateClause(c.forUpdate)
    currentUniqueFrom = oldUniqueFrom
  }

  private[this] def and(ns: Seq[Node]): Option[Node] =
    if (ns.isEmpty) None else Some(ns.reduceLeft((p1, p2) => Library.And.typed[Boolean](p1, p2)))

  protected def flattenJoins(s: TermSymbol, n: Node): (Seq[(TermSymbol, Node)], Seq[Node]) = {
    def f(s: TermSymbol, n: Node): Option[(Seq[(TermSymbol, Node)], Seq[Node])] = n match {
      case Join(ls, rs, l, r, JoinType.Inner, on) =>
        for {
          (defs1, on1) <- f(ls, l)
          (defs2, on2) <- f(rs, r)
        } yield (defs1 ++ defs2, on match {
          case LiteralNode(true) => on1 ++ on2
          case on => on1 ++ on2 :+ on
        })
      case _: Join => None
      case n => Some((Seq((s, n)), Nil))
    }
    f(s, n).getOrElse((Seq((s, n)), Nil))
  }

  protected def buildSelectClause(c: Comprehension) = building(SelectPart) {
    b"select "
    buildSelectModifiers(c)
    c.select match {
      case Pure(StructNode(ch), _) =>
        b.sep(ch, ", ") {
          case (sym, n) =>
            buildSelectPart(n)
            b" as `$sym"
        }
        if (ch.isEmpty) b"1"
      case Pure(ProductNode(ch), _) =>
        b.sep(ch, ", ")(buildSelectPart)
        if (ch.isEmpty) b"1"
      case Pure(n, _) => buildSelectPart(n)
    }
  }

  protected def buildSelectModifiers(c: Comprehension): Unit = {
    c.distinct.foreach {
      case ProductNode(ch) if ch.isEmpty => b"distinct "
      case n => b"distinct on (!$n) "
    }
  }

  protected def scanJoins(from: ConstArray[(TermSymbol, Node)]) {
    for ((sym, j: Join) <- from) {
      joins += sym -> j
      scanJoins(j.generators)
    }
  }

  protected def buildFromClause(from: Seq[(TermSymbol, Node)]) = building(FromPart) {
    from match {
      case Nil | Seq((_, Pure(ProductNode(ConstArray()), _))) => JdbcTypeHelper.scalarFrom.foreach { s => b"\nfrom $s" }
      case from =>
        b"\nfrom "
        b.sep(from, ", ") { case (sym, n) => buildFrom(n, if (Some(sym) == currentUniqueFrom) None else Some(sym)) }
    }
  }

  protected def buildWhereClause(where: Option[Node]) =
    building(WherePart)(where.foreach(p => b"\nwhere !$p"))

  protected def buildGroupByClause(groupBy: Option[Node]) = building(OtherPart)(groupBy.foreach { n =>
    b"\ngroup by "
    n match {
      case ProductNode(es) => b.sep(es, ", ")(buildGroupByColumn)
      case e => buildGroupByColumn(e)
    }
  })

  protected def buildGroupByColumn(by: Node) = by match {
    // Some database systems assign special meaning to literal values in GROUP BY, so we replace
    // them by a constant non-literal expression unless it is known to be safe.
    case LiteralNode(_) if !supportsLiteralGroupBy => b"0+0"
    case e => b"!$e"
  }

  protected def buildHavingClause(having: Option[Node]) =
    building(HavingPart)(having.foreach(p => b"\nhaving !$p"))

  protected def buildOrderByClause(order: ConstArray[(Node, Ordering)]) = building(OtherPart) {
    if (!order.isEmpty) {
      b"\norder by "
      b.sep(order, ", ") { case (n, o) => buildOrdering(n, o) }
    }
  }

  protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = building(OtherPart) {
    (fetch, offset) match {
      /* SQL:2008 syntax */
      case (Some(t), Some(d)) => b"\noffset $d row fetch next $t row only"
      case (Some(t), None) => b"\nfetch next $t row only"
      case (None, Some(d)) => b"\noffset $d row"
      case _ =>
    }
  }

  protected def buildForUpdateClause(forUpdate: Boolean) = building(OtherPart) {
    if (forUpdate) {
      b"\nfor update "
    }
  }

  protected def buildSelectPart(n: Node): Unit = n match {
    case c: Comprehension =>
      b"\["
      buildComprehension(c)
      b"\]"
    case n =>
      expr(n, true)
  }

  protected def buildFrom(n: Node, alias: Option[TermSymbol], skipParens: Boolean = false): Unit = building(FromPart) {
    def addAlias = alias foreach { s => b += ' ' += symbolName(s) }
    n match {
      case t: TableNode =>
        b += quoteTableName(t)
        addAlias
      case j: Join =>
        buildJoin(j)
      case n =>
        expr(n, skipParens)
        addAlias
    }
  }

  protected def buildJoin(j: Join): Unit = {
    buildFrom(j.left, Some(j.leftGen))
    val op = j.on match {
      case LiteralNode(true) if j.jt == JoinType.Inner => "cross"
      case _ => j.jt.sqlName
    }
    b"\n$op join "
    if (j.right.isInstanceOf[Join] && parenthesizeNestedRHSJoin) {
      b"\["
      buildFrom(j.right, Some(j.rightGen))
      b"\]"
    } else buildFrom(j.right, Some(j.rightGen))
    if (op != "cross") j.on match {
      case LiteralNode(true) => b"\non 1=1"
      case on => b"\non !$on"
    }
  }

  def expr(n: Node, skipParens: Boolean = false): Unit = n match {
    case p @ Path(path) =>
      val (base, rest) = path.foldRight[(Option[TermSymbol], List[TermSymbol])]((None, Nil)) {
        case (ElementSymbol(idx), (Some(b), Nil)) => (Some(joins(b).generators(idx - 1)._1), Nil)
        case (s, (None, Nil)) => (Some(s), Nil)
        case (s, (b, r)) => (b, s :: r)
      }
      if (base != currentUniqueFrom) b += symbolName(base.get) += '.'
      rest match {
        case Nil => b += '*'
        case field :: Nil => b += symbolName(field)
        case _ => throw new SlickException("Cannot resolve " + p + " as field or view")
      }
    case (n @ LiteralNode(v)) :@ JdbcTypeHelper(ti, option) =>
      if (n.volatileHint || !ti.hasLiteralForm) b +?= { (p, idx, param) =>
        if (option) ti.setOption(v.asInstanceOf[Option[Any]], p, idx)
        else ti.setValue(v, p, idx)
      }
      else b += JdbcTypeHelper.valueToSQLLiteral(v, n.nodeType)
    case ProductNode(ch) =>
      b"\("
      b.sep(ch, ", ")(expr(_))
      b"\)"
    case n: Apply => n match {
      case Library.Not(Library.==(l, LiteralNode(null))) =>
        b"\($l is not null\)"
      case Library.==(l, LiteralNode(null)) =>
        b"\($l is null\)"
      case Library.==(left: ProductNode, right: ProductNode) =>
        b"\("
        if (supportsTuples) b"$left = $right"
        else {
          val cols = left.children.zip(right.children).force
          b.sep(cols, " and ") { case (l, r) => expr(l); b += "="; expr(r) }
        }
        b"\)"
      case RewriteBooleans.ToFakeBoolean(ch) =>
        expr(IfThenElse(ConstArray(ch, LiteralNode(1).infer(), LiteralNode(0).infer())), skipParens)
      case RewriteBooleans.ToRealBoolean(ch) =>
        expr(Library.==.typed[Boolean](ch, LiteralNode(true).infer()), skipParens)
      case Library.Exists(c: Comprehension) =>
        /* If tuples are not supported, selecting multiple individial columns
           * in exists(select ...) is probably not supported, either, so we rewrite
           * such sub-queries to "select 1". */
        b"exists\[!${(if (supportsTuples) c else c.copy(select = Pure(LiteralNode(1))).infer()): Node}\]"
      case Library.Concat(l, r) if concatOperator.isDefined =>
        b"\($l${concatOperator.get}$r\)"
      case Library.User() if !commonCapabilities.capabilities.contains(RelationalCapabilities.functionUser) =>
        b += "''"
      case Library.Database() if !commonCapabilities.capabilities.contains(RelationalCapabilities.functionDatabase) =>
        b += "''"
      case Library.Pi() if !hasPiFunction => b += pi
      case Library.Degrees(ch) if !hasRadDegConversion => b"(180.0/!${Library.Pi.typed(JdbcTypeHelper.columnTypes.bigDecimalJdbcType)}*$ch)"
      case Library.Radians(ch) if !hasRadDegConversion => b"(!${Library.Pi.typed(JdbcTypeHelper.columnTypes.bigDecimalJdbcType)}/180.0*$ch)"
      case Library.Between(left, start, end) => b"$left between $start and $end"
      case Library.CountDistinct(e) => b"count(distinct $e)"
      case Library.CountAll(e) => b"count($e)"
      case Library.Like(l, r) => b"\($l like $r\)"
      case Library.Like(l, r, LiteralNode(esc: Char)) =>
        if (esc == '\'' || esc == '%' || esc == '_') throw new SlickException("Illegal escape character '" + esc + "' for LIKE expression")
        // JDBC defines an {escape } syntax but the unescaped version is understood by more DBs/drivers
        b"\($l like $r escape '$esc'\)"
      case Library.StartsWith(n, LiteralNode(s: String)) =>
        b"\($n like ${JdbcTypeHelper.valueToSQLLiteral(likeEncode(s) + '%', ScalaBaseType.stringType)} escape '^'\)"
      case Library.EndsWith(n, LiteralNode(s: String)) =>
        b"\($n like ${JdbcTypeHelper.valueToSQLLiteral("%" + likeEncode(s), ScalaBaseType.stringType)} escape '^'\)"
      case Library.Trim(n) =>
        expr(Library.LTrim.typed[String](Library.RTrim.typed[String](n)), skipParens)
      case Library.Substring(n, start, end) =>
        b"\({fn substring($n, ${QueryParameter.constOp[Int]("+")(_ + _)(start, LiteralNode(1).infer())}, ${QueryParameter.constOp[Int]("-")(_ - _)(end, start)})}\)"
      case Library.Substring(n, start) =>
        b"\({fn substring($n, ${QueryParameter.constOp[Int]("+")(_ + _)(start, LiteralNode(1).infer())})}\)"
      case Library.IndexOf(n, str) => b"\({fn locate($str, $n)} - 1\)"
      case Library.Cast(ch @ _*) =>
        val tn =
          if (ch.length == 2) ch(1).asInstanceOf[LiteralNode].value.asInstanceOf[String]
          else JdbcTypeHelper.jdbcTypeFor(n.nodeType).sqlTypeName(None)
        if (supportsCast) b"cast(${ch(0)} as $tn)"
        else b"{fn convert(!${ch(0)},$tn)}"
      case Library.SilentCast(ch) => b"$ch"
      case Apply(sym: Library.SqlOperator, ch) =>
        b"\("
        if (ch.length == 1) {
          b"${sym.name} ${ch.head}"
        } else b.sep(ch, " " + sym.name + " ")(expr(_))
        b"\)"
      case Apply(sym: Library.JdbcFunction, ch) =>
        val quote = quotedJdbcFns.map(_.contains(sym)).getOrElse(true)
        if (quote) b"{fn "
        b"${sym.name}("
        b.sep(ch, ",")(expr(_, true))
        b")"
        if (quote) b"}"
      case Apply(sym: Library.SqlFunction, ch) =>
        b"${sym.name}("
        b.sep(ch, ",")(expr(_, true))
        b")"
      case n => throw new SlickException("Unexpected function call " + n + " -- SQL prefix: " + b.build.sql)
    }
    case c: IfThenElse =>
      b"(case"
      c.ifThenClauses.foreach { case (l, r) => b" when $l then $r" }
      c.elseClause match {
        case LiteralNode(null) =>
        case n => b" else $n"
      }
      b" end)"
    case OptionApply(ch) => expr(ch, skipParens)
    case QueryParameter(extractor, JdbcTypeHelper(ti, option), _) =>
      b +?= { (p, idx, param) =>
        if (option) ti.setOption(extractor(param).asInstanceOf[Option[Any]], p, idx)
        else ti.setValue(extractor(param), p, idx)
      }
    case s: SimpleFunction =>
      if (s.scalar) b"{fn "
      b"${s.name}("
      b.sep(s.children, ",")(expr(_, true))
      b")"
      if (s.scalar) b += '}'
    case RowNumber(by) =>
      b"row_number() over(order by "
      if (by.isEmpty) b"(select 1)"
      else b.sep(by, ", ") { case (n, o) => buildOrdering(n, o) }
      b")"
    case c: Comprehension =>
      b"\{"
      buildComprehension(c)
      b"\}"
    case Union(left, right, all) =>
      b"\{"
      buildFrom(left, None, true)
      if (all) b"\nunion all " else b"\nunion "
      buildFrom(right, None, true)
      b"\}"
    case SimpleLiteral(w) => b += w
    case s: slick.async.lifted.SimpleExpression => s.toSQL(this)
    case s: SimpleBinaryOperator => b"\(${s.left} ${s.name} ${s.right}\)"
    case n => throw new SlickException("Unexpected node " + n + " -- SQL prefix: " + b.build.sql)
  }

  protected def buildOrdering(n: Node, o: Ordering) {
    expr(n)
    if (o.direction.desc) b" desc"
    if (o.nulls.first) b" nulls first"
    else if (o.nulls.last) b" nulls last"
  }

  def buildUpdate: SQLBuilder.Result = {
    val (gen, from, where, select) = tree match {
      case Comprehension(sym, from: TableNode, Pure(select, _), where, None, _, None, None, None, None, false) => select match {
        case f @ Select(Ref(struct), _) if struct == sym => (sym, from, where, ConstArray(f.field))
        case ProductNode(ch) if ch.forall { case Select(Ref(struct), _) if struct == sym => true; case _ => false } =>
          (sym, from, where, ch.map { case Select(Ref(_), field) => field })
        case _ => throw new SlickException("A query for an UPDATE statement must select table columns only -- Unsupported shape: " + select)
      }
      case o => throw new SlickException("A query for an UPDATE statement must resolve to a comprehension with a single table -- Unsupported shape: " + o)
    }

    val qtn = quoteTableName(from)
    symbolName(gen) = qtn // Alias table to itself because UPDATE does not support aliases
    b"update $qtn set "
    b.sep(select, ", ")(field => b += symbolName(field) += " = ?")
    if (!where.isEmpty) {
      b" where "
      expr(where.reduceLeft((a, b) => Library.And.typed[Boolean](a, b)), true)
    }
    b.build
  }

  protected def buildDeleteFrom(tableName: String): Unit = {
    b"delete from $tableName"
  }

  def buildDelete: SQLBuilder.Result = {
    def fail(msg: String) =
      throw new SlickException("Invalid query for DELETE statement: " + msg)
    val (gen, from, where) = tree match {
      case Comprehension(sym, from, Pure(select, _), where, _, _, None, distinct, fetch, offset, forUpdate) =>
        if (fetch.isDefined || offset.isDefined || distinct.isDefined || forUpdate)
          fail(".take, .drop .forUpdate and .distinct are not supported for deleting")
        from match {
          case from: TableNode => (sym, from, where)
          case from => fail("A single source table is required, found: " + from)
        }
      case o => fail("Unsupported shape: " + o + " -- A single SQL comprehension is required")
    }
    val qtn = quoteTableName(from)
    symbolName(gen) = qtn // Alias table to itself because DELETE does not support aliases
    buildDeleteFrom(qtn)
    if (!where.isEmpty) {
      b" where "
      expr(where.reduceLeft((a, b) => Library.And.typed[Boolean](a, b)), true)
    }
    b.build
  }

}