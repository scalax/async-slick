package slick.async.jdbc.config

import slick.basic.Capability
import slick.jdbc.JdbcCapabilities
import slick.relational.RelationalCapabilities
import slick.sql.SqlCapabilities

trait SQLiteCapabilities extends JdbcComponentCapabilities {
  override protected lazy val computeCapabilities: Set[Capability] = (super.computeCapabilities
    - RelationalCapabilities.functionDatabase
    - RelationalCapabilities.functionUser
    - RelationalCapabilities.joinFull
    - RelationalCapabilities.joinRight
    - JdbcCapabilities.mutable
    - SqlCapabilities.sequence
    - JdbcCapabilities.returnInsertOther
    - RelationalCapabilities.typeBigDecimal
    - RelationalCapabilities.typeBlob
    - RelationalCapabilities.zip
    - JdbcCapabilities.insertOrUpdate
    - JdbcCapabilities.defaultValueMetaData
    - JdbcCapabilities.booleanMetaData
    - JdbcCapabilities.supportsByte
    - JdbcCapabilities.distinguishesIntTypes
    - JdbcCapabilities.forUpdate)
}