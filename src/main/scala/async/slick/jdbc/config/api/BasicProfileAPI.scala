package slick.async.jdbc.config

import slick.ast.Node
import slick.async.dbio.NoStream
import slick.async.jdbc.JdbcInvokerComponent
import slick.lifted._
import scala.language.higherKinds
import scala.language.implicitConversions

trait BasicProfileAPI { self =>

  protected val jdbcInvokerComponent: JdbcInvokerComponent
  protected val crudCompiler: CrudCompiler

  protected def createQueryActionExtensionMethods[R, S <: NoStream](tree: Node, param: Any): QueryActionExtensionMethodsImpl[R, S] =
    new QueryActionExtensionMethodsImpl[R, S](tree, param) {
      override val jdbcInvokerComponent = self.jdbcInvokerComponent
    }
  protected def createStreamingQueryActionExtensionMethods[R, T](tree: Node, param: Any): StreamingQueryActionExtensionMethodsImpl[R, T] =
    new StreamingQueryActionExtensionMethodsImpl[R, T](tree, param) {
      override val jdbcInvokerComponent = self.jdbcInvokerComponent
    }

  implicit final def anyToShapedValue[T, U](value: T)(implicit shape: Shape[_ <: FlatShapeLevel, T, U, _]): ShapedValue[T, U] =
    new ShapedValue[T, U](value, shape)

  implicit def streamableQueryActionExtensionMethods[U, C[_]](q: Query[_, U, C]): StreamingQueryActionExtensionMethodsImpl[C[U], U] =
    createStreamingQueryActionExtensionMethods[C[U], U](crudCompiler.queryCompiler.run(q.toNode).tree, ())
  implicit def runnableCompiledQueryActionExtensionMethods[RU](c: RunnableCompiled[_, RU]): QueryActionExtensionMethodsImpl[RU, NoStream] =
    createQueryActionExtensionMethods[RU, NoStream](c.compiledQuery, c.param)
  implicit def streamableCompiledQueryActionExtensionMethods[RU, EU](c: StreamableCompiled[_, RU, EU]): StreamingQueryActionExtensionMethodsImpl[RU, EU] =
    createStreamingQueryActionExtensionMethods[RU, EU](c.compiledQuery, c.param)
  // Applying a CompiledFunction always results in only a RunnableCompiled, not a StreamableCompiled, so we need this:
  implicit def streamableAppliedCompiledFunctionActionExtensionMethods[R, RU, EU, C[_]](c: AppliedCompiledFunction[_, Query[R, EU, C], RU]): StreamingQueryActionExtensionMethodsImpl[RU, EU] =
    createStreamingQueryActionExtensionMethods[RU, EU](c.compiledQuery, c.param)
  implicit def recordQueryActionExtensionMethods[M, R](q: M)(implicit shape: Shape[_ <: FlatShapeLevel, M, R, _]): QueryActionExtensionMethodsImpl[R, NoStream] =
    createQueryActionExtensionMethods[R, NoStream](crudCompiler.queryCompiler.run(shape.toNode(q)).tree, ())
}