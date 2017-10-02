package slick.async.jdbc.config

import slick.compiler.{ Phase, QueryCompiler }

trait H2QueryCompiler extends SqlQueryCompiler {

  override lazy val capabilities: BasicCapabilities = new H2Capabilities {}

  override lazy val computeQueryCompiler: QueryCompiler = {
    super.computeQueryCompiler.replace(Phase.resolveZipJoinsRownumStyle) - Phase.fixRowNumberOrdering
  }

}