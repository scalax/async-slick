package slick.async.jdbc.config

import slick.compiler.{ Phase, QueryCompiler }

trait PostgresQueryCompiler extends SqlQueryCompiler {

  override def capabilities: CommonCapabilities = new PostgresCapabilities {}

  override def computeQueryCompiler: QueryCompiler = {
    super.computeQueryCompiler - Phase.rewriteDistinct
  }

}