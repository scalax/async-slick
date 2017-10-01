package slick.async.jdbc.config

import slick.async.jdbc.ProtectGroupBy
import slick.compiler._

trait SQLServerQueryCompiler extends SqlQueryCompiler {

  override lazy val capabilities: CommonCapabilities = new SQLServerCapabilities {}

  override lazy val computeQueryCompiler: QueryCompiler = {
    (super.computeQueryCompiler
      .addAfter(new RemoveTakeDrop(translateTake = false), Phase.expandSums)
      .addBefore(new ProtectGroupBy, Phase.mergeToComprehensions)
      .replace(new RemoveFieldNames(alwaysKeepSubqueryNames = true))
      + Phase.rewriteBooleans)
  }

}