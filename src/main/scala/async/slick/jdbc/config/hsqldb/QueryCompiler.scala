package slick.async.jdbc.config

import slick.compiler.{ EmulateOuterJoins, Phase, QueryCompiler }
import slick.relational.RelationalCapabilities

trait HsqldbQueryCompiler extends SqlQueryCompiler {

  override lazy val capabilities: CommonCapabilities = new HsqldbCapabilities {}

  override lazy val computeQueryCompiler: QueryCompiler = {
    super.computeQueryCompiler.replace(Phase.resolveZipJoinsRownumStyle) + Phase.specializeParameters - Phase.fixRowNumberOrdering
  }

}