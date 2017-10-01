package slick.async.jdbc.config

import slick.basic.Capability
import slick.jdbc.JdbcCapabilities

trait PostgresCapabilities extends JdbcComponentCapabilities {
  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - JdbcCapabilities.insertOrUpdate
    - JdbcCapabilities.nullableNoDefault
    - JdbcCapabilities.supportsByte)
}