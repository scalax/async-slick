package slick.async.jdbc.config

import slick.compiler.{ Phase, QueryCompiler }

trait PostgresQueryCompiler extends RelationalQueryCompiler {

  override def capabilities: CommonCapabilities = new PostgresCapabilities {}

  override def computeQueryCompiler: QueryCompiler = {
    super.computeQueryCompiler - Phase.rewriteDistinct
  }

}