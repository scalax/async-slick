package slick.async.jdbc.config

import slick.basic.Capability
import slick.jdbc.JdbcCapabilities
import slick.relational.RelationalCapabilities
import slick.sql.SqlCapabilities

trait CommonCapabilities {
  /**
   * The capabilities supported by this profile. This can be used to query at
   * runtime whether a specific feature is supported.
   */
  final lazy val capabilities: Set[Capability] = computeCapabilities
  /** Compute the capabilities. This should be overridden in subclasses as needed. */
  protected def computeCapabilities: Set[Capability] = Set.empty
}

trait RelationalComponentCapabilities extends CommonCapabilities {
  override protected def computeCapabilities: Set[Capability] = super.computeCapabilities ++ RelationalCapabilities.all
}

trait SqlComponentCapabilities extends RelationalComponentCapabilities {
  override protected def computeCapabilities: Set[Capability] = super.computeCapabilities ++ SqlCapabilities.all
}

trait JdbcComponentCapabilities extends SqlComponentCapabilities {
  override protected def computeCapabilities: Set[Capability] = super.computeCapabilities ++ JdbcCapabilities.all
}