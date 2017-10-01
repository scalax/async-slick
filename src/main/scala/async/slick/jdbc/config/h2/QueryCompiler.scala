package slick.async.jdbc.config

import slick.compiler.{ Phase, QueryCompiler }

trait H2QueryCompiler extends RelationalQueryCompiler {

  override def capabilities: CommonCapabilities = new H2Capabilities {}

  override def computeQueryCompiler: QueryCompiler = {
    super.computeQueryCompiler.replace(Phase.resolveZipJoinsRownumStyle) - Phase.fixRowNumberOrdering
  }

}