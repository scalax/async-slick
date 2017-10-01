package slick.async.dbio

import slick.util
import slick.async.dbio
import slick.lifted.AbstractTable

import scala.language.higherKinds

/**
 * Aliases for lifted embedding features. This trait can be mixed into aliasing
 * objects which simplify the use of the lifted embedding.
 */
trait DBIOAliases {

  type DBIO[+R] = dbio.DBIO[R]
  type StreamingDBIO[+R, +T] = dbio.StreamingDBIO[R, T]
  type DBIOAction[+R, +S <: dbio.NoStream, -E <: dbio.Effect] = dbio.DBIOAction[R, S, E]
  val DBIO = dbio.DBIO
  type Effect = dbio.Effect
  val Effect = dbio.Effect
  type NoStream = dbio.NoStream
  type Streaming[+T] = dbio.Streaming[T]
  type AsyncExecutor = util.AsyncExecutor
  val AsyncExecutor = util.AsyncExecutor

}