package slick.async.jdbc.config

import slick.basic.Capability
import slick.jdbc.JdbcCapabilities
import slick.relational.RelationalCapabilities

trait DB2Capabilities extends JdbcComponentCapabilities {
  override protected lazy val computeCapabilities: Set[Capability] = (super.computeCapabilities
    - RelationalCapabilities.reverse
    - JdbcCapabilities.insertOrUpdate
    - JdbcCapabilities.supportsByte
    - JdbcCapabilities.booleanMetaData)
}