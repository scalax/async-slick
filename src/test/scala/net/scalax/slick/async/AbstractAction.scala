package net.scalax.slick.async

import slick.basic.BasicProfile
import slick.dbio.DBIO
import slick.lifted.Query

import scala.concurrent.ExecutionContext
import scala.language.higherKinds

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
  def successfully[T](m: T): ActionImpl[T] = {
    new ActionImpl[T] {
      override def result[DBAction[_]](implicit actionFunction: ActionFunction[DBAction]): DBAction[T] = {
        actionFunction.point(m)
      }
    }
  }
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
    val friend1 = Friends(None, "喵", "汪")
    val friend2 = Friends(None, "jilen", "kerr")
    val friend3 = Friends(None, "小莎莎", "烟流")
    db.run(friendTq.schema.create).futureValue
    db.run(friendTq ++= List(friend1, friend2, friend3)).futureValue
  }

  after {
    db.run(friendTq.delete).futureValue
  }

  "model" should "insert with json data" in {
    import ActionFunctionHelper._
    val friendQuery = for {
      inFriend <- friendTq.toAction
    } yield {
      inFriend.map { s =>
        println(s)
        s
      }
    }
    db.run(friendQuery.result[DBIO]).futureValue
  }

}