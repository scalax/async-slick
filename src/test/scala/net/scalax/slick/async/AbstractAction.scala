package net.scalax.slick.async

import slick.dbio.DBIO
import scala.concurrent.ExecutionContext
import scala.language.higherKinds

trait ActionFunction[DBAction[_]] {

  def point[T](m: T): DBAction[T]

  def map[T, S](m: DBAction[T], f: T => S)(implicit executor: ExecutionContext): DBAction[S]

  def flatMap[T, S](m: DBAction[T], f: T => DBAction[S])(implicit executor: ExecutionContext): DBAction[S]

}

class DBIOActionFunction extends ActionFunction[DBIO] {
  self =>

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
/*abstract trait AbstractAction[T] {
  def map[S](f: T => S)(implicit executor: ExecutionContext): AbstractAction[S]
  def flatMap[S](f: T => AbstractAction[S])(implicit executor: ExecutionContext): AbstractAction[S]
}*/
trait ActionImpl[DBAction[_], T] { self =>

  val actionFunction: ActionFunction[DBAction]

  val actionModel: DBAction[T]

  /*def successfully(m: T): AbstractAction[DBAction, T] = {
    new AbstractAction[DBAction, T] {
      override val actionFunction = self.actionFunction
      override val actionModel: DBAction[T] = actionFunction.point(m)
    }
  }*/

  def map[S](f: T => S)(implicit executor: ExecutionContext): ActionImpl[DBAction, S] = {
    new ActionImpl[DBAction, S] {
      override val actionFunction = self.actionFunction
      override val actionModel: DBAction[S] = self.actionFunction.map(self.actionModel, f)(executor)
    }
  }

  def flatMap[S](f: T => ActionImpl[DBAction, S])(implicit executor: ExecutionContext): ActionImpl[DBAction, S] = {
    new ActionImpl[DBAction, S] {
      override val actionFunction = self.actionFunction
      override val actionModel: DBAction[S] = self.actionFunction.flatMap(self.actionModel, { s: T => f(s).actionModel })(executor)
    }
  }

}