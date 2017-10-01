package slick.async.jdbc.config

import slick.ast._
import slick.compiler.{ CompilerState, Phase, QueryCompiler }
import slick.util.ConstArray

trait OracleQueryCompiler extends SqlQueryCompiler {

  override lazy val capabilities: CommonCapabilities = new OracleCapabilities {}

  override lazy val computeQueryCompiler: QueryCompiler = {
    (super.computeQueryCompiler.addAfter(Phase.removeTakeDrop, Phase.expandSums)
      .replace(Phase.resolveZipJoinsRownumStyle)
      - Phase.fixRowNumberOrdering
      + Phase.rewriteBooleans + new RemoveSubqueryOrdering)
  }

}

/**
 * Remove ORDER BY from comprehensions that are used as arguments to a
 * scalar function.
 */
class RemoveSubqueryOrdering extends Phase {
  val name = "removeSubqueryOrdering"

  def apply(state: CompilerState) =
    state.map { n => ClientSideOp.mapServerSide(n)(n => rewrite(n, false)) }

  def rewrite(n: Node, inScalar: Boolean): Node = n match {
    case n: Comprehension if inScalar && n.orderBy.nonEmpty =>
      val n2 = n.copy(orderBy = ConstArray.empty) :@ n.nodeType
      n2.mapChildren(ch => rewrite(ch, false), keepType = true)
    case Apply(_, _) if !n.nodeType.structural.isInstanceOf[CollectionType] =>
      n.mapChildren(ch => rewrite(ch, true), keepType = true)
    case n =>
      n.mapChildren(ch => rewrite(ch, false), keepType = true)
  }
}