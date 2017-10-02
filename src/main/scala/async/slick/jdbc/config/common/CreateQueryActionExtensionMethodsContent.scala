package slick.async.jdbc.config

import slick.ast.TypeUtil.:@
import slick.ast._
import slick.async.basic.BasicStreamingAction
import slick.async.dbio.{ Effect, NoStream, Streaming, SynchronousDatabaseAction }
import slick.async.jdbc.{ JdbcBackend, JdbcInvokerComponent, ResultSetMutator, StreamingInvokerAction }
import slick.async.sql.{ FixedSqlAction, FixedSqlStreamingAction }
import slick.relational.CompiledMapping
import slick.util.SQLBuilder
import slick.ast.Util._

import scala.collection.mutable.Builder

trait CreateQueryActionExtensionMethodsContent { self =>
  val jdbcInvokerComponent: JdbcInvokerComponent

  def createQueryActionExtensionMethods[R, S <: NoStream](tree: Node, param: Any): QueryActionExtensionMethodsImpl[R, S] =
    new QueryActionExtensionMethodsImpl[R, S](tree, param) {
      override val jdbcInvokerComponent = self.jdbcInvokerComponent
    }
}

trait BasicQueryActionExtensionMethods[R, S <: NoStream] {
  /** An Action that runs this query. */
  def result: FixedSqlAction[R, S, Effect.Read]
}

abstract class QueryActionExtensionMethodsImpl[R, S <: NoStream](tree: Node, param: Any) extends BasicQueryActionExtensionMethods[R, S] { self =>
  val jdbcInvokerComponent: JdbcInvokerComponent

  def result: FixedSqlAction[R, S, Effect.Read] = {
    def findSql(n: Node): String = n match {
      case c: CompiledStatement => c.extra.asInstanceOf[SQLBuilder.Result].sql
      case ParameterSwitch(cases, default) =>
        findSql(cases.find { case (f, n) => f(param) }.map(_._2).getOrElse(default))
    }
    (tree match {
      case (rsm @ ResultSetMapping(_, compiled, CompiledMapping(_, elemType))) :@ (ct: CollectionType) =>
        val sql = findSql(compiled)
        new StreamingInvokerAction[R, Any, Effect] { streamingAction =>
          protected[this] def createInvoker(sql: Iterable[String]) = jdbcInvokerComponent.createQueryInvoker(rsm, param, sql.head)
          protected[this] def createBuilder = ct.cons.createBuilder(ct.elementType.classTag).asInstanceOf[Builder[Any, R]]
          def statements = List(sql)
          override def getDumpInfo = super.getDumpInfo.copy(name = "result")
        }
      case First(rsm @ ResultSetMapping(_, compiled, _)) =>
        val sql = findSql(compiled)
        new SimpleJdbcProfileAction[R]("result", Vector(sql)) {
          def run(ctx: JdbcBackend#Context, sql: Vector[String]): R =
            jdbcInvokerComponent.createQueryInvoker[R](rsm, param, sql.head).first(ctx.session)
        }
    }).asInstanceOf[FixedSqlAction[R, S, Effect.Read]]
  }
}

trait BasicStreamingQueryActionExtensionMethodsImpl[R, T] extends BasicQueryActionExtensionMethods[R, Streaming[T]] {
  override def result: BasicStreamingAction[R, T, Effect.Read] with FixedSqlAction[R, Streaming[T], Effect.Read]
}

abstract class StreamingQueryActionExtensionMethodsImpl[R, T](tree: Node, param: Any) extends QueryActionExtensionMethodsImpl[R, Streaming[T]](tree, param) with BasicStreamingQueryActionExtensionMethodsImpl[R, T] { self =>
  override def result: FixedSqlStreamingAction[R, T, Effect.Read] = super.result.asInstanceOf[FixedSqlStreamingAction[R, T, Effect.Read]]

  /** Same as `mutate(sendEndMarker = false)`. */
  def mutate: FixedSqlAction[Nothing, Streaming[ResultSetMutator[T]], Effect.Read with Effect.Write] = mutate(false)

  /**
   * Create an Action that can be streamed in order to modify a mutable result set. All stream
   * elements will be the same [[slick.jdbc.ResultSetMutator]] object but it is in a different state each
   * time. Thre resulting stream is always non-buffered and events can be processed either
   * synchronously or asynchronously (but all processing must happen in sequence).
   *
   * @param sendEndMarker If set to true, an extra event is sent after the end of the result
   *                      set, poviding you with a chance to insert additional rows after
   *                      seeing all results. Only `end` (to check for this special event) and
   *                      `insert` may be called in the ResultSetMutator in this case.
   */
  def mutate(sendEndMarker: Boolean = false): FixedSqlAction[Nothing, Streaming[ResultSetMutator[T]], Effect.Read with Effect.Write] = {
    val sql = tree.findNode(_.isInstanceOf[CompiledStatement]).get
      .asInstanceOf[CompiledStatement].extra.asInstanceOf[SQLBuilder.Result].sql
    val (rsm @ ResultSetMapping(_, _, CompiledMapping(_, elemType))) :@ (ct: CollectionType) = tree
    new MutatingResultAction[T](rsm, elemType, ct, sql, param, sendEndMarker) {
      override val jdbcInvokerComponent = self.jdbcInvokerComponent
    }
  }
}