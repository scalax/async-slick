package slick.async.jdbc.config

import slick.compiler.{ Phase, QueryCompiler }

trait DB2QueryCompiler extends SqlQueryCompiler {

  override def capabilities: CommonCapabilities = new DB2Capabilities {}

  override def computeQueryCompiler: QueryCompiler = {
    (super.computeQueryCompiler.addAfter(Phase.removeTakeDrop, Phase.expandSums)
      + Phase.rewriteBooleans)
  }

}