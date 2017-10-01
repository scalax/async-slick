package slick.async.jdbc.config

import slick.compiler.{ EmulateOuterJoins, Phase, QueryCompiler }
import slick.relational.RelationalCapabilities

trait RelationalQueryCompiler {

  def capabilities: CommonCapabilities = new RelationalComponentCapabilities {}

  def computeQueryCompiler: QueryCompiler = {
    val base = QueryCompiler.standard
    val canJoinLeft = capabilities.capabilities contains RelationalCapabilities.joinLeft
    val canJoinRight = capabilities.capabilities contains RelationalCapabilities.joinRight
    val canJoinFull = capabilities.capabilities contains RelationalCapabilities.joinFull
    if (canJoinLeft && canJoinRight && canJoinFull) base
    else base.addBefore(new EmulateOuterJoins(canJoinLeft, canJoinRight), Phase.expandRecords)
  }

}