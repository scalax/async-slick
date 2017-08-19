package net.scalax.slick.async

import slick.dbio._
import slick.jdbc.JdbcBackend
import slick.lifted.Query

import scala.language.higherKinds
import scala.language.implicitConversions

trait QueryFunction[E, F <: NoStream] {
  val key: String
}

trait SlcikQueryResult[G, C[_]] extends QueryFunction[C[G], Streaming[G]] {
  val query: Query[_, G, C]
}

trait SlickQueryUpdate extends QueryFunction[Int, NoStream] {
  type Rep
  type Data
  type Coll[_]
  val query: Query[Rep, Data, Coll]
  val data: Data
}

trait QueryExtra[DBAction[_, _ <: NoStream]] {
  def apply[F, G <: NoStream](queryWrap: QueryFunction[F, G]): DBAction[F, G]
}

trait ActionFunction[DBAction[_, _ <: NoStream]] {

  val queryConverts: Map[String, QueryExtra[DBAction]]

  def actionToSetOnly[S, T <: NoStream](action: DBAction[S, T]): DBAction[S, NoStream]

}

trait SessionConn[T, _ <: NoStream] {
  def withSession(implicit session: JdbcBackend#Session): T
}