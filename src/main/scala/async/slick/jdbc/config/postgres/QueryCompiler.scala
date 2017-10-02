package slick.async.jdbc.config

import slick.compiler.{ Phase, QueryCompiler }

trait PostgresQueryCompiler extends SqlQueryCompiler {

  override lazy val capabilities: BasicCapabilities = new PostgresCapabilities {}

  override lazy val computeQueryCompiler: QueryCompiler = {
    super.computeQueryCompiler - Phase.rewriteDistinct
  }

}