package slick.async.jdbc.config

import slick.compiler.{ Phase, QueryCompiler }

trait DB2QueryCompiler extends SqlQueryCompiler {

  override lazy val capabilities: BasicCapabilities = new DB2Capabilities {}

  override lazy val computeQueryCompiler: QueryCompiler = {
    (super.computeQueryCompiler.addAfter(Phase.removeTakeDrop, Phase.expandSums)
      + Phase.rewriteBooleans)
  }

}