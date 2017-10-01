package slick.async.jdbc.config

import slick.compiler.{ Phase, QueryCompiler }

trait DerbyQueryCompiler extends SqlQueryCompiler {

  override def capabilities: CommonCapabilities = new DerbyCapabilities {}

  override def computeQueryCompiler: QueryCompiler = {
    super.computeQueryCompiler + Phase.rewriteBooleans + Phase.specializeParameters
  }

}