package net.scalax.slick.async

import slick.basic.BasicProfile
import slick.dbio.{ DBIO, Effect, NoStream, SynchronousDatabaseAction }
import slick.jdbc.{ JdbcActionComponent, JdbcBackend, JdbcProfile }
import slick.lifted.Query

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.concurrent.ExecutionContext

trait QueryFunction[E] {
  val key: String
}

trait SlcikQueryResult[E, F <: Seq[E]] extends QueryFunction[F] {
  type Rep = E
  val query: Query[_, Rep, Seq]
}

trait SlickQueryUpdate extends QueryFunction[Int] {
  type Rep
  type Data
  val query: Query[Rep, Data, Seq]
  val data: Data
}

trait QueryExtra[DBAction[_]] {
  def apply[F](queryWrap: QueryFunction[F]): DBAction[F]
}

trait ActionFunction[DBAction[_]] {

  val queryConverts: Map[String, QueryExtra[DBAction]]

  def point[T](m: T): DBAction[T]

  def map[T, S](m: DBAction[T], f: T => S)(implicit executor: ExecutionContext): DBAction[S]

  def flatMap[T, S](m: DBAction[T], f: T => DBAction[S])(implicit executor: ExecutionContext): DBAction[S]

}

object ActionFunctionHelper {

  def result[E](query1: Query[_, E, Seq]): SlcikQueryResult[E, Seq[E]] = {
    new SlcikQueryResult[E, Seq[E]] {
      override val key = "slick.query.result"
      override type Rep = E
      override val query = query1
    }
  }

  def update[E, U](query1: Query[E, U, Seq], data1: U): SlickQueryUpdate = {
    new SlickQueryUpdate {
      override val key = "slick.query.update"
      override type Rep = E
      override type Data = U
      override val query = query1
      override val data = data1
    }
  }

  implicit class toActionConvert[E](funAction: QueryFunction[E]) {
    def toAction = {
      new ActionImpl[E] {
        self =>
        def result[DBAction[_]](implicit actionFunction: ActionFunction[DBAction]): DBAction[E] = {
          actionFunction.queryConverts(funAction.key).apply(funAction)
        }
      }
    }
  }

  implicit def DBIOActionFunction(
    implicit
    retrieveCv: Query[Any, Any, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[Any], Any],
    updateConV: Query[_, Any, Seq] => JdbcActionComponent#UpdateActionExtensionMethods[Any]
  ): ActionFunction[DBIO] = new ActionFunction[DBIO] {
    self =>

    override val queryConverts = Map(
      "slick.query.result" -> new QueryExtra[DBIO] {
        def apply[F](queryWrap: QueryFunction[F]): DBIO[F] = {
          val wrap = queryWrap.asInstanceOf[SlcikQueryResult[_, _]]
          retrieveCv.asInstanceOf[Query[_, wrap.Rep, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[wrap.Rep], wrap.Rep]]
            .apply(wrap.query).result.asInstanceOf[DBIO[F]]
        }
      },
      "slick.query.update" -> new QueryExtra[DBIO] {
        def apply[F](queryWrap: QueryFunction[F]): DBIO[F] = {
          val wrap = queryWrap.asInstanceOf[SlickQueryUpdate]
          updateConV.asInstanceOf[Query[_, wrap.Data, Seq] => JdbcActionComponent#UpdateActionExtensionMethods[wrap.Data]]
            .apply(wrap.query).update(wrap.data).asInstanceOf[DBIO[F]]
        }
      }
    )

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

  implicit def syncActionFunction(jdbcProfile: JdbcProfile): ActionFunction[SessionConn] = new ActionFunction[SessionConn] {
    self =>

    override val queryConverts = Map(
      "slick.query.result" -> new QueryExtra[SessionConn] {
        def apply[F](queryWrap: QueryFunction[F]): SessionConn[F] = {
          val wrap = queryWrap.asInstanceOf[SlcikQueryResult[_, _]]
          val invoker = new jdbcProfile.QueryInvokerImpl[wrap.Rep](jdbcProfile.queryCompiler.run(wrap.query.toNode).tree, null, null)
          new SessionConn[F] {
            override def withSession(implicit session: JdbcBackend#Session): F = {
              invoker.results(0)(session).right.get.toList.asInstanceOf[F]
            }
          }
        }
      },
      "slick.query.update" -> new QueryExtra[SessionConn] {
        def apply[F](queryWrap: QueryFunction[F]): SessionConn[F] = {
          val wrap = queryWrap.asInstanceOf[SlickQueryUpdate]
          val tree = jdbcProfile.updateCompiler.run(wrap.query.toNode).tree

          class BlockingJdbcActionContext(s: JdbcBackend#Session) extends jdbcProfile.backend.JdbcActionContext {
            val useSameThread = true
            override def session = s.asInstanceOf[jdbcProfile.backend.Session]
            override def connection = s.conn
          }

          new SessionConn[F] {
            override def withSession(implicit session: JdbcBackend#Session): F = {
              jdbcProfile.createUpdateActionExtensionMethods(tree, null).update(wrap.data)
                .asInstanceOf[SynchronousDatabaseAction[Int, NoStream, JdbcBackend, Effect]].run(new BlockingJdbcActionContext(session))
                .asInstanceOf[F]
            }
          }
        }
      }
    )

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

}