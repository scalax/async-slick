package slick.async.jdbc.config

import slick.SlickException
import slick.ast._
import slick.async.dbio.{ Effect, NoStream, Streaming, SynchronousDatabaseAction }
import slick.async.jdbc._
import slick.async.sql.FixedSqlAction
import slick.relational.{ CompiledMapping, ResultConverter }
import slick.util.{ SQLBuilder, ignoreFollowOnError }

import scala.language.existentials
import scala.util.control.NonFatal

abstract class MutatingResultAction[T](rsm: ResultSetMapping, elemType: Type, collectionType: CollectionType, sql: String, param: Any, sendEndMarker: Boolean) extends SynchronousDatabaseAction[Nothing, Streaming[ResultSetMutator[T]], JdbcBackend, Effect] with FixedSqlAction[Nothing, Streaming[ResultSetMutator[T]], Effect] { streamingAction =>
  val jdbcInvokerComponent: JdbcInvokerComponent

  class Mutator(val prit: PositionedResultIterator[T], val bufferNext: Boolean, val inv: QueryInvokerImpl[T]) extends ResultSetMutator[T] {
    val pr = prit.pr
    val rs = pr.rs
    var current: T = _
    /** The state of the stream. 0 = in result set, 1 = before end marker, 2 = after end marker. */
    var state = 0
    def row = if (state > 0) throw new SlickException("After end of result set") else current
    def row_=(value: T): Unit = {
      if (state > 0) throw new SlickException("After end of result set")
      pr.restart
      inv.updateRowValues(pr, value)
      rs.updateRow()
    }
    def +=(value: T): Unit = {
      rs.moveToInsertRow()
      pr.restart
      inv.updateRowValues(pr, value)
      rs.insertRow()
      if (state == 0) rs.moveToCurrentRow()
    }
    def delete: Unit = {
      if (state > 0) throw new SlickException("After end of result set")
      rs.deleteRow()
      if (jdbcInvokerComponent.invokerPreviousAfterDelete) rs.previous()
    }
    def emitStream(ctx: JdbcBackend#StreamingContext, limit: Long): this.type = {
      var count = 0L
      try {
        while (count < limit && state == 0) {
          if (!pr.nextRow) state = if (sendEndMarker) 1 else 2
          if (state == 0) {
            current = inv.extractValue(pr)
            count += 1
            ctx.emit(this)
          }
        }
        if (count < limit && state == 1) {
          ctx.emit(this)
          state = 2
        }
      } catch {
        case NonFatal(ex) =>
          try prit.close() catch ignoreFollowOnError
          throw ex
      }
      if (state < 2) this else null
    }
    def end = if (state > 1) throw new SlickException("After end of result set") else state > 0
    override def toString = s"Mutator(state = $state, current = $current)"
  }
  type StreamState = Mutator
  def statements = List(sql)
  def run(ctx: JdbcBackend#Context) =
    throw new SlickException("The result of .mutate can only be used in a streaming way")
  override def emitStream(ctx: JdbcBackend#StreamingContext, limit: Long, state: StreamState): StreamState = {
    val mu = if (state ne null) state else {
      val inv = jdbcInvokerComponent.createQueryInvoker[T](rsm, param, sql)
      new Mutator(
        inv.results(0, defaultConcurrency = jdbcInvokerComponent.invokerMutateConcurrency, defaultType = jdbcInvokerComponent.invokerMutateType)(ctx.session).right.get,
        ctx.bufferNext,
        inv
      )
    }
    mu.emitStream(ctx, limit)
  }
  override def cancelStream(ctx: JdbcBackend#StreamingContext, state: StreamState): Unit = state.prit.close()
  override def getDumpInfo = super.getDumpInfo.copy(name = "mutate")
  def overrideStatements(_statements: Iterable[String]): MutatingResultAction[T] =
    new MutatingResultAction[T](rsm, elemType, collectionType, _statements.head, param, sendEndMarker) {
      override val jdbcInvokerComponent = streamingAction.jdbcInvokerComponent
    }
}