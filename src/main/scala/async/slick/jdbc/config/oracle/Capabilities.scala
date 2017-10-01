package slick.async.jdbc.config

import slick.basic.Capability
import slick.jdbc.JdbcCapabilities
import slick.relational.RelationalCapabilities

trait OracleCapabilities extends JdbcComponentCapabilities {
  override protected lazy val computeCapabilities: Set[Capability] = (super.computeCapabilities
    - RelationalCapabilities.foreignKeyActions
    - JdbcCapabilities.insertOrUpdate
    - JdbcCapabilities.booleanMetaData
    - JdbcCapabilities.distinguishesIntTypes
    - JdbcCapabilities.supportsByte)
}