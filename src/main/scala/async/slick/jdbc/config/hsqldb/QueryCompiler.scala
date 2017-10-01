package slick.async.jdbc.config

import slick.compiler.{ EmulateOuterJoins, Phase, QueryCompiler }
import slick.relational.RelationalCapabilities

trait HsqldbQueryCompiler extends RelationalQueryCompiler {

  override def capabilities: CommonCapabilities = new HsqldbCapabilities {}

  override def computeQueryCompiler: QueryCompiler = {
    super.computeQueryCompiler.replace(Phase.resolveZipJoinsRownumStyle) + Phase.specializeParameters - Phase.fixRowNumberOrdering
  }

}