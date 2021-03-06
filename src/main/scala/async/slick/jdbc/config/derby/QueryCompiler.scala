package slick.async.jdbc.config

import slick.compiler.{ Phase, QueryCompiler }

trait DerbyQueryCompiler extends SqlQueryCompiler {

  override lazy val capabilities: BasicCapabilities = new DerbyCapabilities {}

  override lazy val computeQueryCompiler: QueryCompiler = {
    super.computeQueryCompiler + Phase.rewriteBooleans + Phase.specializeParameters
  }

}