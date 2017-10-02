package slick.async.jdbc.config

import slick.lifted._
import scala.language.higherKinds
import scala.language.implicitConversions

trait LowPriorityAPI {
  protected val crudCompiler: CrudCompiler

  implicit def queryUpdateActionExtensionMethods[U, C[_]](q: Query[_, U, C]): UpdateActionExtensionMethodsImpl[U] =
    UpdateActionExtensionContent.createUpdateActionExtensionMethods(crudCompiler.updateCompiler.run(q.toNode).tree, ())
}