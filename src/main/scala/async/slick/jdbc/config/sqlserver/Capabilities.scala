package slick.async.jdbc.config

import slick.basic.Capability
import slick.jdbc.JdbcCapabilities
import slick.sql.SqlCapabilities

trait SQLServerCapabilities extends JdbcComponentCapabilities {
  override protected lazy val computeCapabilities: Set[Capability] = (super.computeCapabilities
    - JdbcCapabilities.forceInsert
    - JdbcCapabilities.returnInsertOther
    - JdbcCapabilities.insertOrUpdate
    - SqlCapabilities.sequence
    - JdbcCapabilities.supportsByte)
}