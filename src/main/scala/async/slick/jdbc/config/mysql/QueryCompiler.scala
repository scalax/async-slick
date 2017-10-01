package slick.async.jdbc.config

import slick.ast._
import slick.async.jdbc.MySQLProfile.{ RowNum, RowNumGen }
import slick.compiler.{ Phase, QueryCompiler, ResolveZipJoins }
import slick.util.ConstArray
import slick.ast.Util._

trait MysqlQueryCompiler extends SqlQueryCompiler {

  override def capabilities: CommonCapabilities = new MysqlCapabilities {}

  override def computeQueryCompiler: QueryCompiler = {
    super.computeQueryCompiler.replace(new MySQLResolveZipJoins) - Phase.fixRowNumberOrdering
  }

}

class MySQLResolveZipJoins extends ResolveZipJoins {
  // MySQL does not support ROW_NUMBER() but you can manually increment a variable in the SELECT
  // clause to emulate it. See http://stackoverflow.com/a/1895127/458687 for an example.
  // According to http://dev.mysql.com/doc/refman/5.0/en/user-variables.html this should not be
  // relied on but it is the generally accepted solution and there is no better way.
  override def transformZipWithIndex(s1: TermSymbol, ls: TermSymbol, from: Node,
    defs: ConstArray[(TermSymbol, Node)], offset: Long, p: Node): Node = {
    val countSym = new AnonSymbol
    val j = Join(new AnonSymbol, new AnonSymbol,
      Bind(ls, from, Pure(StructNode(defs))),
      Bind(new AnonSymbol, Pure(StructNode(ConstArray.empty)),
        Pure(StructNode(ConstArray(new AnonSymbol -> RowNumGen(countSym, offset - 1))))),
      JoinType.Inner, LiteralNode(true))
    var first = true
    Subquery(Bind(s1, j, p.replace {
      case Select(Ref(s), ElementSymbol(2)) if s == s1 =>
        val r = RowNum(countSym, first)
        first = false
        r
      case r @ Ref(s) if s == s1 => r.untyped
    }), Subquery.Default).infer()
  }
}