package slick.async.jdbc.config

import slick.basic.Capability
import slick.jdbc.JdbcCapabilities
import slick.relational.RelationalCapabilities
import slick.sql.SqlCapabilities

trait MysqlCapabilities extends JdbcComponentCapabilities {
  override protected lazy val computeCapabilities: Set[Capability] = (super.computeCapabilities
    - JdbcCapabilities.returnInsertOther
    - SqlCapabilities.sequenceLimited
    - RelationalCapabilities.joinFull
    - JdbcCapabilities.nullableNoDefault
    - JdbcCapabilities.distinguishesIntTypes //https://github.com/slick/slick/pull/1735
  )
}