package slick.async.jdbc.config

import slick.compiler.{ EmulateOuterJoins, Phase, QueryCompiler }
import slick.relational.RelationalCapabilities

trait DB2QueryCompiler extends RelationalQueryCompiler {

  override def capabilities: CommonCapabilities = new DB2Capabilities {}

  override def computeQueryCompiler: QueryCompiler = {
    (super.computeQueryCompiler.addAfter(Phase.removeTakeDrop, Phase.expandSums)
      + Phase.rewriteBooleans)
  }

}