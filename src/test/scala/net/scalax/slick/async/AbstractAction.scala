package net.scalax.slick.async

import slick.dbio.DBIO
import slick.jdbc.H2Profile

import scala.concurrent.ExecutionContext
import scala.language.higherKinds

trait ActionImpl[T, E <: QueryFunction[T], D] { self =>

  val qFunction: E
  def convert[DBAction[_]](actionFunction: ActionFunction[DBAction], qExtra: QueryExtra[T, E, DBAction], source: DBAction[T]): DBAction[D]

  def result[DBAction[_]](implicit actionFunction: ActionFunction[DBAction], qExtra: QueryExtra[T, E, DBAction]): DBAction[D] = {
    convert(actionFunction, qExtra, qExtra.apply(qFunction))
  }

  def map[S](f: D => S)(implicit executor: ExecutionContext): ActionImpl[T, E, S] = {
    new ActionImpl[T, E, S] {
      override val qFunction = self.qFunction
      override def convert[DBAction[_]](actionFunction: ActionFunction[DBAction], qExtra: QueryExtra[T, E, DBAction], source: DBAction[T]): DBAction[S] = {
        actionFunction.map(self.convert(actionFunction, qExtra, source), f)
      }
    }
  }

  def flatMap[S](f: D => ActionImpl[T, E, S])(implicit executor: ExecutionContext): ActionImpl[T, E, S] = {
    new ActionImpl[T, E, S] {
      override val qFunction = self.qFunction
      override def convert[DBAction[_]](actionFunction: ActionFunction[DBAction], qExtra: QueryExtra[T, E, DBAction], source: DBAction[T]): DBAction[S] = {
        actionFunction.flatMap(self.convert(actionFunction, qExtra, source), (s: D) => f(s).result(actionFunction, qExtra))
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
    Database.forDataSource(datasource)
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
      inFriend <- result(friendTq)
    } yield {
      inFriend.map { s =>
        println(s)
        s
      }
    }
    db.run(friendQuery.result[DBIO]).futureValue
    db.run(update(friendTq, Friends(None, "hahahahaha", "hahahahaha")).result[DBIO]).futureValue
    db.run(friendQuery.result[DBIO]).futureValue
  }

  "model" should "select with sync mode" in {
    import ActionFunctionHelper._
    implicit def syncConvertSlick[D]: QueryExtra[Seq[D], SlcikQueryResult[D], SessionConn] = syncConvert(H2Profile)
    implicit val syncUpdateSlick = syncUpdate(H2Profile)
    val friendQuery = for {
      inFriend <- result(friendTq)
    } yield {
      inFriend.map { s =>
        println(s)
        s
      }
    }
    friendQuery.result[SessionConn].withSession(db.createSession())
    update(friendTq, Friends(None, "hahahahaha", "hahahahaha")).result[SessionConn].withSession(db.createSession())
    friendQuery.result[SessionConn].withSession(db.createSession())
  }

}