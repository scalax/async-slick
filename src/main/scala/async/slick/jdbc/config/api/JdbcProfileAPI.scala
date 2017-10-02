package slick.async.jdbc.config

import slick.async.relational.RelationalProfile
import slick.lifted._

import scala.language.higherKinds
import scala.language.implicitConversions

trait JdbcProfileAPI { self =>

  protected val crudCompiler: CrudCompiler

  implicit def queryDeleteActionExtensionMethods[C[_]](q: Query[_ <: RelationalProfile#Table[_], _, C]): DeleteActionExtensionMethodsImpl =
    DeleteActionExtensionContent.createDeleteActionExtensionMethods(crudCompiler.deleteCompiler.run(q.toNode).tree, ())

  implicit def runnableCompiledDeleteActionExtensionMethods[RU, C[_]](c: RunnableCompiled[_ <: Query[_, _, C], C[RU]]): DeleteActionExtensionMethodsImpl =
    DeleteActionExtensionContent.createDeleteActionExtensionMethods(c.compiledDelete, c.param)

  implicit def runnableCompiledUpdateActionExtensionMethods[RU, C[_]](c: RunnableCompiled[_ <: Query[_, _, C], C[RU]]): UpdateActionExtensionMethodsImpl[RU] =
    UpdateActionExtensionContent.createUpdateActionExtensionMethods(c.compiledUpdate, c.param)

}