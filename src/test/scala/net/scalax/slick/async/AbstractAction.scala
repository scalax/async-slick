package net.scalax.slick.async

import slick.dbio.DBIO
import slick.jdbc.H2Profile

import scala.concurrent.ExecutionContext
import scala.language.higherKinds

trait ActionImpl[T] { self =>

  def result[DBAction[_]](implicit actionFunction: ActionFunction[DBAction]): DBAction[T]

  def map[S](f: T => S)(implicit executor: ExecutionContext): ActionImpl[S] = {
    new ActionImpl[S] {
      override def result[DBAction[_]](implicit actionFunction: ActionFunction[DBAction]): DBAction[S] = {
        actionFunction.map(self.result(actionFunction), f)
      }
    }
  }

  def flatMap[S](f: T => ActionImpl[S])(implicit executor: ExecutionContext): ActionImpl[S] = {
    new ActionImpl[S] {
      override def result[DBAction[_]](implicit actionFunction: ActionFunction[DBAction]): DBAction[S] = {
        actionFunction.flatMap(self.result(actionFunction), { s: T => f(s).result(actionFunction) })
      }
    }
  }

}

object ActionImpl {
  /*def successfully[T](m: T): ActionImpl[T, E] = {
    new ActionImpl[T, E] {
      override def result[DBAction[_]](implicit actionFunction: ActionFunction[DBAction]): DBAction[T] = {
        actionFunction.point(m)
      }
    }
  }*/
}

import slick.jdbc.H2Profile.api._

import org.h2.jdbcx.JdbcDataSource
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case class Friends(
  id: Option[Long] = None,
  name: String,
  nick: String
)

class FriendTable(tag: slick.lifted.Tag) extends Table[Friends](tag, "firend") {
  def id = column[Long]("id", O.AutoInc)
  def name = column[String]("name")
  def nick = column[String]("nick")

  def * = (id.?, name, nick).mapTo[Friends]
}

class CreateTest extends FlatSpec
    with Matchers
    with EitherValues
    with ScalaFutures
    with BeforeAndAfterAll
    with BeforeAndAfter {

  val t = 10.seconds
  override implicit val patienceConfig = PatienceConfig(timeout = t)
  val logger = LoggerFactory.getLogger(getClass)

  val friendTq = TableQuery[FriendTable]

  lazy val db = {
    val datasource = new JdbcDataSource()
    datasource.setUrl(s"jdbc:h2:mem:hfTest;DB_CLOSE_DELAY=-1")
    Database.forDataSource(datasource, None)
  }

  override def beforeAll = {
    db.run(friendTq.schema.create).futureValue
  }

  before {
    val friend1 = Friends(None, "喵", "汪")
    val friend2 = Friends(None, "jilen", "kerr")
    val friend3 = Friends(None, "小莎莎", "烟流")
    db.run(friendTq ++= List(friend1, friend2, friend3)).futureValue
  }

  after {
    db.run(friendTq.delete).futureValue
  }

  "model" should "select with DBIO mode" in {
    import ActionFunctionHelper._
    val friendQuery = for {
      inFriend <- result(friendTq).toAction
    } yield {
      inFriend.map { s =>
        println(s)
        s
      }
    }
    db.run(friendQuery.result[DBIO]).futureValue
    db.run(update(friendTq, Friends(None, "hahahahaha", "hahahahaha")).toAction.result[DBIO]).futureValue
    db.run(friendQuery.result[DBIO]).futureValue
  }

  "model" should "select with sync mode" in {
    import ActionFunctionHelper._
    implicit val syncUpdateSlick = syncActionFunction(H2Profile)
    val friendQuery = for {
      inFriend <- result(friendTq).toAction
    } yield {
      inFriend.map { s =>
        println(s)
        s
      }
    }
    friendQuery.result[SessionConn].withSession(db.createSession())
    update(friendTq, Friends(None, "hahahahaha", "hahahahaha")).toAction.result[SessionConn].withSession(db.createSession())
    friendQuery.result[SessionConn].withSession(db.createSession())
  }

}