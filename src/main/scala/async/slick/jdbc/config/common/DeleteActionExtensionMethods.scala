package slick.async.jdbc.config

import slick.ast._
import slick.async.dbio.{ Effect, NoStream, SynchronousDatabaseAction }
import slick.async.jdbc.JdbcBackend
import slick.async.sql.FixedSqlAction
import slick.util.SQLBuilder

object DeleteActionExtensionContent {
  def createDeleteActionExtensionMethods(tree: Node, param: Any): DeleteActionExtensionMethodsImpl =
    new DeleteActionExtensionMethodsImpl(tree, param)
}

class DeleteActionExtensionMethodsImpl(tree: Node, param: Any) {
  /** An Action that deletes the data selected by this query. */
  def delete: FixedSqlAction[Int, NoStream, Effect.Write] = {
    val ResultSetMapping(_, CompiledStatement(_, sres: SQLBuilder.Result, _), _) = tree
    new SimpleJdbcProfileAction[Int]("delete", Vector(sres.sql)) {
      def run(ctx: JdbcBackend#Context, sql: Vector[String]): Int = ctx.session.withPreparedStatement(sql.head) { st =>
        sres.setter(st, 1, param)
        st.executeUpdate
      }
    }
  }
}

abstract class SimpleJdbcProfileAction[+R](_name: String, val statements: Vector[String]) extends SynchronousDatabaseAction[R, NoStream, JdbcBackend, Effect] with FixedSqlAction[R, NoStream, Effect] { self =>
  def run(ctx: JdbcBackend#Context, sql: Vector[String]): R
  final override def getDumpInfo = super.getDumpInfo.copy(name = _name)
  final def run(ctx: JdbcBackend#Context): R = run(ctx, statements)
  final def overrideStatements(_statements: Iterable[String]): FixedSqlAction[R, NoStream, Effect] = new SimpleJdbcProfileAction[R](_name, _statements.toVector) {
    def run(ctx: JdbcBackend#Context, sql: Vector[String]): R = self.run(ctx, statements)
  }
}