package slick.async.lifted

import slick.ast.Node
import slick.async.jdbc.JdbcStatementBuilderComponent

/** A SimpleExpression allows arbitrary SQL code to be generated. */
trait SimpleExpression extends Node {
  def toSQL(qb: JdbcStatementBuilderComponent#QueryBuilder): Unit
}