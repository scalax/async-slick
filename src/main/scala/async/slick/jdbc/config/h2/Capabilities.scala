package slick.async.jdbc.config

import slick.basic.Capability
import slick.jdbc.JdbcCapabilities
import slick.relational.RelationalCapabilities
import slick.sql.SqlCapabilities

trait H2Capabilities extends JdbcComponentCapabilities {
  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - SqlCapabilities.sequenceMin
    - SqlCapabilities.sequenceMax
    - SqlCapabilities.sequenceCycle
    - JdbcCapabilities.returnInsertOther
    - RelationalCapabilities.joinFull
    - JdbcCapabilities.insertOrUpdate
    - RelationalCapabilities.reverse)
}