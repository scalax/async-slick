package slick.async.jdbc.config

import slick.basic.Capability
import slick.jdbc.JdbcCapabilities
import slick.sql.SqlCapabilities

trait HsqldbCapabilities extends JdbcComponentCapabilities {
  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - SqlCapabilities.sequenceCurr
    - JdbcCapabilities.insertOrUpdate)
}