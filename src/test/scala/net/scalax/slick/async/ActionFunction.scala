package net.scalax.slick.async

import java.sql.Connection

import slick.basic.BasicProfile
import slick.dbio.{DBIO, Effect, NoStream, SynchronousDatabaseAction}
import slick.jdbc.{JdbcActionComponent, JdbcBackend, JdbcProfile}
import slick.lifted.Query

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.concurrent.ExecutionContext

trait QueryFunction[E]

trait SlcikQueryResult[E] extends QueryFunction[Seq[E]] {
  type Rep
  val query: Query[Rep, E, Seq]
}

trait SlickQueryUpdate extends QueryFunction[Int] {
  type Rep
  type Data
  val query: Query[Rep, Data, Seq]
  val data: Data
}

trait QueryExtra[E, T <: QueryFunction[E], DBAction[_]] {
  def apply(queryWrap: T): DBAction[E]
}

trait ActionFunction[DBAction[_]] {
  //def byQuery[E, T](query: Query[E, T, Seq]): DBAction[Seq[T]]
  def point[T](m: T): DBAction[T]

  def map[T, S](m: DBAction[T], f: T => S)(implicit executor: ExecutionContext): DBAction[S]

  def flatMap[T, S](m: DBAction[T], f: T => DBAction[S])(implicit executor: ExecutionContext): DBAction[S]

}

object ActionFunctionHelper {

  trait InitActionImpl[T, E <: QueryFunction[T]] extends ActionImpl[T, E, T] {
    override def convert[DBAction[_]](actionFunction: ActionFunction[DBAction], qExtra: QueryExtra[T, E, DBAction], source: DBAction[T]): DBAction[T] = source
  }

  def result[E, U](query1: Query[E, U, Seq]): InitActionImpl[Seq[U], SlcikQueryResult[U]] = {
    new InitActionImpl[Seq[U], SlcikQueryResult[U]] {
      override val qFunction = new SlcikQueryResult[U] {
        override type Rep = E
        override val query = query1
      }
    }
  }

  def update[E, U](query1: Query[E, U, Seq], data1: U): InitActionImpl[Int, SlickQueryUpdate] = {
    new InitActionImpl[Int, SlickQueryUpdate] {
      override val qFunction = new SlickQueryUpdate {
        override type Rep = E
        override type Data = U
        override val query = query1
        override val data = data1
      }
    }
  }

  implicit def resultConvert[D](
                              implicit
                              retrieveCv: Query[_, D, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[D], D]
                            ): QueryExtra[Seq[D], SlcikQueryResult[D], DBIO] = new QueryExtra[Seq[D], SlcikQueryResult[D], DBIO] {
    def apply(queryWrap: SlcikQueryResult[D]): DBIO[Seq[D]] = {
      retrieveCv(queryWrap.query).result
    }
  }

  implicit def updateConvert(
                            implicit
                            updateConV: Query[_, Any, Seq] => JdbcActionComponent#UpdateActionExtensionMethods[Any]
                            ): QueryExtra[Int, SlickQueryUpdate, DBIO] = new QueryExtra[Int, SlickQueryUpdate, DBIO] {
    def apply(queryWrap: SlickQueryUpdate): DBIO[Int] = {
      updateConV.asInstanceOf[Query[_, queryWrap.Data, Seq] => JdbcActionComponent#UpdateActionExtensionMethods[queryWrap.Data]](queryWrap.query).update(queryWrap.data)
    }
  }

  implicit def syncConvert[D](jdbcProfile: JdbcProfile): QueryExtra[Seq[D], SlcikQueryResult[D], SessionConn] = new QueryExtra[Seq[D], SlcikQueryResult[D], SessionConn] {
    override def apply(queryWrap: SlcikQueryResult[D]): SessionConn[Seq[D]] = {
      val invoker = new jdbcProfile.QueryInvokerImpl[D](jdbcProfile.queryCompiler.run(queryWrap.query.toNode).tree, null, null)
      new SessionConn[Seq[D]] {
        override def withSession(implicit session: JdbcBackend#Session): Seq[D] = {
          invoker.results(0)(session).right.get.toList
        }
      }
    }
  }

  implicit def syncUpdate(jdbcProfile: JdbcProfile): QueryExtra[Int, SlickQueryUpdate, SessionConn] =
    new QueryExtra[Int, SlickQueryUpdate, SessionConn] {
      override def apply(queryWrap: SlickQueryUpdate): SessionConn[Int] = {
        val tree = jdbcProfile.updateCompiler.run(queryWrap.query.toNode).tree

        class BlockingJdbcActionContext(s: JdbcBackend#Session) extends jdbcProfile.backend.JdbcActionContext {
          val useSameThread = true
          override def session = s.asInstanceOf[jdbcProfile.backend.Session]
          override def connection = s.conn
        }

        new SessionConn[Int] {
          override def withSession(implicit session: JdbcBackend#Session): Int = {
            jdbcProfile.createUpdateActionExtensionMethods(tree, null).update(queryWrap.data)
              .asInstanceOf[SynchronousDatabaseAction[Int, NoStream, JdbcBackend, Effect]].run(new BlockingJdbcActionContext(session))
          }
        }
      }
    }

  implicit def DBIOActionFunction(
                                   implicit
                                   retrieveCv: Query[Any, Any, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Any], Any]
                                 ): ActionFunction[DBIO] = new ActionFunction[DBIO] {
    self =>
    /*override def byQuery[E, T](query: Query[E, T, Seq]): DBIO[Seq[T]] = {
      retrieveCv
        .asInstanceOf[Query[E, T, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[T], T]]
        .apply(query)
        .result
    }*/
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

  implicit val syncActionFunction: ActionFunction[SessionConn] = new ActionFunction[SessionConn] {
    self =>
    /*override def byQuery[E, T](query: Query[E, T, Seq]): SessionConn[Seq[T]] = {
      val invoker = new jdbcProfile.QueryInvokerImpl[T](jdbcProfile.queryCompiler.run(query.toNode).tree, null, null)
      new SessionConn[Seq[T]] {
        override def withSession(implicit session: JdbcBackend#Session): Seq[T] = {
          invoker.results(0)(session).right.get.toList
        }
      }
    }*/
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
  /*implicit class QueryConvert[E, U](query1: Query[E, U, Seq]) {
    def toAction: ActionImpl[Seq[U]] = {
      new ActionImpl[Seq[U]] {
        def result[DBAction[_]](implicit actionFunction: ActionFunction[DBAction]): DBAction[Seq[U]] = {
          actionFunction.byQuery(query1)
        }
      }
    }
  }*/
}