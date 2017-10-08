package net.scalax.slick.async

import ActionFunctionHelper._

import slick.async.dbio.StreamingDBIO

import scala.language.higherKinds

import slick.async.jdbc.H2Profile.api._

import org.h2.jdbcx.JdbcDataSource
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case class Friends(
  id: Option[Long] = None,
  name: String,
  nick: String)

class FriendTable(tag: slick.lifted.Tag) extends Table[Friends](tag, "firend") {
  def id = column[Long]("id", O.AutoInc)
  def name = column[String]("name")
  def nick = column[String]("nick")

  def * = (id.?, name, nick).mapTo[Friends]
}

class AsyncTest extends FlatSpec
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
    friendTq.result
  }

  after {
    db.run(friendTq.delete).futureValue
  }

  "model" should "select with DBIO mode" in {
    try {
      val friendQuery = for {
        inFriend <- friendTq.asyncResult.set[StreamingDBIO]
      } yield {
        inFriend.map { s =>
          println(s)
          s
        }
      }
      db.run(friendQuery).futureValue
      db.run(friendTq.asyncUpdate(Friends(None, "hahahahaha", "hahahahaha")).set[StreamingDBIO]).futureValue
      db.run(friendQuery).futureValue
    } catch {
      case e: Exception =>
        logger.error("error", e)
        throw e
    }
  }

  "model" should "select with sync mode" in {
    val friendsSession = friendTq.asyncResult.set[SessionConn]

    val friends = friendsSession.withSession(db.createSession())
    println(friends)
    friendTq.map(s => s.name -> s.nick).asyncUpdate("hahahahaha" -> "hahahahaha").set[SessionConn].withSession(db.createSession())
    val friends1 = friendsSession.withSession(db.createSession())
    println(friends1)
  }

}