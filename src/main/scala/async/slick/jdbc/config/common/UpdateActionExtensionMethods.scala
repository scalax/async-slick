package slick.async.jdbc.config

import slick.ast._
import slick.async.dbio.{ Effect, NoStream }
import slick.async.jdbc.{ JdbcBackend, JdbcResultConverterDomain }
import slick.async.sql.FixedSqlAction
import slick.relational.{ CompiledMapping, ResultConverter }
import slick.util.SQLBuilder
import scala.language.existentials

object UpdateActionExtensionContent {
  def createUpdateActionExtensionMethods[T](tree: Node, param: Any): UpdateActionExtensionMethodsImpl[T] =
    new UpdateActionExtensionMethodsImpl[T](tree, param)
}

class UpdateActionExtensionMethodsImpl[T](tree: Node, param: Any) {
  protected[this] val ResultSetMapping(_,
    CompiledStatement(_, sres: SQLBuilder.Result, _),
    CompiledMapping(_converter, _)) = tree
  protected[this] val converter = _converter.asInstanceOf[ResultConverter[JdbcResultConverterDomain, T]]

  /** An Action that updates the data selected by this query. */
  def update(value: T): FixedSqlAction[Int, NoStream, Effect.Write] = {
    new SimpleJdbcProfileAction[Int]("update", Vector(sres.sql)) {
      def run(ctx: JdbcBackend#Context, sql: Vector[String]): Int = ctx.session.withPreparedStatement(sql.head) { st =>
        st.clearParameters
        converter.set(value, st)
        sres.setter(st, converter.width + 1, param)
        st.executeUpdate
      }
    }
  }
  /** Get the statement usd by `update` */
  def updateStatement: String = sres.sql
}