package slick.async.jdbc.config

import slick.basic.Capability
import slick.jdbc.JdbcCapabilities
import slick.relational.RelationalCapabilities
import slick.sql.SqlCapabilities

trait DerbyCapabilities extends JdbcComponentCapabilities {
  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - RelationalCapabilities.functionDatabase
    - RelationalCapabilities.pagingNested
    - JdbcCapabilities.returnInsertOther
    - SqlCapabilities.sequenceCurr
    // Cycling is broken in Derby. It cycles to the start value instead of min or max
    - SqlCapabilities.sequenceCycle
    - RelationalCapabilities.zip
    - RelationalCapabilities.joinFull
    - JdbcCapabilities.insertOrUpdate
    - RelationalCapabilities.replace
    - RelationalCapabilities.reverse
    - JdbcCapabilities.booleanMetaData
    - JdbcCapabilities.supportsByte
    - RelationalCapabilities.repeat)
}