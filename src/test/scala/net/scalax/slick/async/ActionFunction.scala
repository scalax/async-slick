package net.scalax.slick.async

import slick.dbio._
import slick.jdbc.{ JdbcBackend, JdbcProfile }
import slick.lifted.Query

import scala.language.higherKinds

object ActionFunctionHelper {

  implicit class QueryExtendsionMethod[E, U, C[_]](query: Query[E, U, C]) {
    def asyncResult: SlcikQueryResult[U, C] = {
      val query1 = query
      new SlcikQueryResult[U, C] {
        override val key = "slick.query.result"
        override val query = query1
      }
    }

    def asyncUpdate(data: U): SlickQueryUpdate = {
      val query1 = query
      val data1 = data
      new SlickQueryUpdate {
        override val key = "slick.query.update"
        override type Rep = E
        override type Data = U
        override type Coll[T] = C[T]
        override val query = query1
        override val data = data1
      }
    }
  }

  implicit class toActionConvert[E, F <: NoStream](funAction: QueryFunction[E, F]) {
    def set[DBAction[_, _ <: NoStream]](implicit actionFunction: ActionFunction[DBAction]): DBAction[E, NoStream] = {
      actionFunction.actionToSetOnly(withStream)
    }
    def withStream[DBAction[_, _ <: NoStream]](implicit actionFunction: ActionFunction[DBAction]): DBAction[E, F] = {
      actionFunction.queryConverts.get(funAction.key).getOrElse(throw new IllegalArgumentException("Query function instance not found.")).apply(funAction)
    }
  }

  implicit def DBIOActionFunction(
    implicit
    profile: JdbcProfile
  ): ActionFunction[StreamingDBIO] = new ActionFunction[StreamingDBIO] {
    self =>

    val jdbcProfile = profile
    import jdbcProfile.api._

    override val queryConverts = Map(
      "slick.query.result" -> new QueryExtra[StreamingDBIO] {
        def apply[F, G <: NoStream](queryWrap: QueryFunction[F, G]): StreamingDBIO[F, G] = {
          //Set can be any other type. Will not effect the final result.
          val wrap = queryWrap.asInstanceOf[SlcikQueryResult[_, Set]]
          wrap.query.result.asInstanceOf[StreamingDBIO[F, G]]
        }
      },
      "slick.query.update" -> new QueryExtra[StreamingDBIO] {
        def apply[F, G <: NoStream](queryWrap: QueryFunction[F, G]): StreamingDBIO[F, G] = {
          val wrap = queryWrap.asInstanceOf[SlickQueryUpdate]
          wrap.query.update(wrap.data).asInstanceOf[StreamingDBIO[F, G]]
        }
      }
    )

    override def actionToSetOnly[S, T <: NoStream](action: StreamingDBIO[S, T]): StreamingDBIO[S, NoStream] = {
      action
    }

  }

  implicit def syncActionFunction(
    implicit
    profile: JdbcProfile
  ): ActionFunction[SessionConn] = new ActionFunction[SessionConn] {
    self =>

    val jdbcProfile = profile

    override val queryConverts = Map(
      "slick.query.result" -> new QueryExtra[SessionConn] {
        def apply[F, G <: NoStream](queryWrap: QueryFunction[F, G]): SessionConn[F, G] = {
          val wrap = queryWrap.asInstanceOf[SlcikQueryResult[_, Nothing]]
          val invoker = new jdbcProfile.QueryInvokerImpl(jdbcProfile.queryCompiler.run(wrap.query.toNode).tree, null, null)
          new SessionConn[F, G] {
            override def withSession(implicit session: JdbcBackend#Session): F = {
              invoker.results(0)(session).right.get.toList.asInstanceOf[F]
            }
          }
        }
      },
      "slick.query.update" -> new QueryExtra[SessionConn] {
        def apply[F, G <: NoStream](queryWrap: QueryFunction[F, G]): SessionConn[F, G] = {
          val wrap = queryWrap.asInstanceOf[SlickQueryUpdate]
          val tree = jdbcProfile.updateCompiler.run(wrap.query.toNode).tree

          class BlockingJdbcActionContext(s: JdbcBackend#Session) extends jdbcProfile.backend.JdbcActionContext {
            val useSameThread = true
            override def session = s.asInstanceOf[jdbcProfile.backend.Session]
            override def connection = s.conn
          }

          new SessionConn[F, NoStream] {
            override def withSession(implicit session: JdbcBackend#Session): F = {
              jdbcProfile.createUpdateActionExtensionMethods(tree, null).update(wrap.data)
                .asInstanceOf[SynchronousDatabaseAction[Int, NoStream, JdbcBackend, Effect]].run(new BlockingJdbcActionContext(session))
                .asInstanceOf[F]
            }
          }.asInstanceOf[SessionConn[F, G]]
        }
      }
    )

    override def actionToSetOnly[S, T <: NoStream](action: SessionConn[S, T]): SessionConn[S, NoStream] = {
      new SessionConn[S, NoStream] {
        def withSession(implicit session: JdbcBackend#Session): S = {
          action.withSession(session)
        }
      }
    }

  }

}