package slick.async.jdbc.config

import slick.ast._
import slick.async.dbio.NoStream
import slick.async.jdbc.JdbcInvokerComponent

trait CreateQueryActionExtensionMethodsContent { self =>
  val jdbcInvokerComponent: JdbcInvokerComponent

  def createQueryActionExtensionMethods[R, S <: NoStream](tree: Node, param: Any): QueryActionExtensionMethodsImpl[R, S] =
    new QueryActionExtensionMethodsImpl[R, S](tree, param) {
      override val jdbcInvokerComponent = self.jdbcInvokerComponent
    }
}

object CreateQueryActionExtensionMethodsContent {
  def apply[R, S <: NoStream](invoker: JdbcInvokerComponent, tree: Node, param: Any): QueryActionExtensionMethodsImpl[R, S] = {
    new CreateQueryActionExtensionMethodsContent {
      override val jdbcInvokerComponent = invoker
    }.createQueryActionExtensionMethods(tree, param)
  }
}