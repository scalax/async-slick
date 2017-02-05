package net.scalax.slick.async

import slick.basic.BasicProfile
import slick.dbio.DBIO
import slick.jdbc.{JdbcBackend, JdbcProfile}
import slick.lifted.Query

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.concurrent.ExecutionContext

trait ActionFunction[DBAction[_]] {

  def byQuery[E, T](query: Query[E, T, Seq]): DBAction[Seq[T]]

  def point[T](m: T): DBAction[T]

  def map[T, S](m: DBAction[T], f: T => S)(implicit executor: ExecutionContext): DBAction[S]

  def flatMap[T, S](m: DBAction[T], f: T => DBAction[S])(implicit executor: ExecutionContext): DBAction[S]

}

object ActionFunctionHelper {

  implicit def DBIOActionFunction(
                                   implicit
                                   retrieveCv: Query[Any, Any, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Any], Any]
                                 ): ActionFunction[DBIO] = new ActionFunction[DBIO] {
    self =>

    override def byQuery[E, T](query: Query[E, T, Seq]): DBIO[Seq[T]] = {
      retrieveCv
        .asInstanceOf[Query[E, T, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[T], T]]
        .apply(query)
        .result
    }

    override def point[T](m: T): DBIO[T] = {
      DBIO.successful(m)
    }

    override def map[T, S](m: DBIO[T], f: T => S)(implicit executor: ExecutionContext): DBIO[S] = {
      m.map(f)(executor)
    }

    override def flatMap[T, S](m: DBIO[T], f: T => DBIO[S])(implicit executor: ExecutionContext): DBIO[S] = {
      m.flatMap(f)(executor)
    }

  }

  trait SessionConn[T] {
    def withSession(implicit session: JdbcBackend#Session): T
  }

  implicit def syncActionFunction(
                                 jdbcProfile: JdbcProfile
                                 ): ActionFunction[SessionConn] = new ActionFunction[SessionConn] {
    self =>

    override def byQuery[E, T](query: Query[E, T, Seq]): SessionConn[Seq[T]] = {
      val invoker = new jdbcProfile.QueryInvokerImpl[T](jdbcProfile.queryCompiler.run(query.toNode).tree, null, null)
      new SessionConn[Seq[T]] {
        override def withSession(implicit session: JdbcBackend#Session): Seq[T] = {
          invoker.results(0)(session).right.get.toList
        }
      }
    }

    override def point[T](m: T): SessionConn[T] = {
      new SessionConn[T] {
        override def withSession(implicit session: JdbcBackend#Session): T = {
          m
        }
      }
    }

    override def map[T, S](m: SessionConn[T], f: T => S)(implicit executor: ExecutionContext): SessionConn[S] = {
      new SessionConn[S] {
        override def withSession(implicit session: JdbcBackend#Session): S = {
          f(m.withSession(session))
        }
      }
    }

    override def flatMap[T, S](m: SessionConn[T], f: T => SessionConn[S])(implicit executor: ExecutionContext): SessionConn[S] = {
      new SessionConn[S] {
        override def withSession(implicit session: JdbcBackend#Session): S = {
          f(m.withSession(session)).withSession(session)
        }
      }
    }

  }

  implicit class QueryConvert[E, U](query1: Query[E, U, Seq]) {
    def toAction: ActionImpl[Seq[U]] = {
      new ActionImpl[Seq[U]] {
        def result[DBAction[_]](implicit actionFunction: ActionFunction[DBAction]): DBAction[Seq[U]] = {
          actionFunction.byQuery(query1)
        }
      }
    }
  }

}