package slick.async.jdbc.config

import slick.ast.{ Symbol, SymbolNamer, TableNode }

trait BasicSqlUtilsComponent {

  /** quotes identifiers to avoid collisions with SQL keywords and other syntax issues */
  def quoteIdentifier(id: String): String = {
    val s = new StringBuilder(id.length + 4) append '"'
    for (c <- id) if (c == '"') s append "\"\"" else s append c
    (s append '"').toString
  }

  def quoteTableName(t: TableNode): String = t.schemaName match {
    case Some(s) => quoteIdentifier(s) + "." + quoteIdentifier(t.tableName)
    case None => quoteIdentifier(t.tableName)
  }

  def likeEncode(s: String) = {
    val b = new StringBuilder
    for (c <- s) c match {
      case '%' | '_' | '^' => b append '^' append c
      case _ => b append c
    }
    b.toString
  }

  class QuotingSymbolNamer(parent: Option[SymbolNamer]) extends SymbolNamer("x", "y", parent) {
    override def namedSymbolName(s: Symbol) = quoteIdentifier(s.name)
  }
}

trait MysqlUtilsComponent extends BasicSqlUtilsComponent {

  override def quoteIdentifier(id: String) = '`' + id + '`'

}