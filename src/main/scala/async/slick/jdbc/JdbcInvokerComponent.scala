package slick.async.jdbc

import scala.language.existentials
import java.sql.PreparedStatement
import slick.ast.{ CompiledStatement, ResultSetMapping, Node, ParameterSwitch }
import slick.util.SQLBuilder
import slick.relational.{ ResultConverter, CompiledMapping }

trait JdbcInvokerComponent { self =>

  def createQueryInvoker[R](tree: Node, param: Any, sql: String): QueryInvokerImpl[R] = new QueryInvokerImpl[R](tree, param, sql)

  // Parameters for invokers -- can be overridden by profiles as needed
  val invokerMutateConcurrency: ResultSetConcurrency = ResultSetConcurrency.Updatable
  val invokerMutateType: ResultSetType = ResultSetType.Auto
  val invokerPreviousAfterDelete = false

}

/** An Invoker for queries. */
trait QueryInvoker[R] extends StatementInvoker[R] {
  def invoker: this.type = this
}

class QueryInvokerImpl[R](tree: Node, param: Any, overrideSql: String) extends QueryInvoker[R] {
  protected[this] val ResultSetMapping(_, compiled, CompiledMapping(_converter, _)) = tree
  protected[this] val converter = _converter.asInstanceOf[ResultConverter[JdbcResultConverterDomain, R]]
  protected[this] val CompiledStatement(_, sres: SQLBuilder.Result, _) = findCompiledStatement(compiled)

  protected[this] def findCompiledStatement(n: Node): CompiledStatement = n match {
    case c: CompiledStatement => c
    case ParameterSwitch(cases, default) =>
      findCompiledStatement(cases.find { case (f, n) => f(param) }.map(_._2).getOrElse(default))
  }

  protected def getStatement = if (overrideSql ne null) overrideSql else sres.sql
  protected def setParam(st: PreparedStatement): Unit = sres.setter(st, 1, param)
  def extractValue(pr: PositionedResult): R = converter.read(pr.rs)
  def updateRowValues(pr: PositionedResult, value: R) = converter.update(value, pr.rs)
}

trait DB2InvokerComponent extends JdbcInvokerComponent { self =>

  override val invokerMutateType: ResultSetType = ResultSetType.ScrollSensitive

}