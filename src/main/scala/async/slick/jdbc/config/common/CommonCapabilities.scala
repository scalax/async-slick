package slick.async.jdbc.config

import slick.basic.Capability
import slick.jdbc.JdbcCapabilities
import slick.relational.RelationalCapabilities
import slick.sql.SqlCapabilities

trait CommonCapabilities {
  /** The capabilities supported by this profile. This can be used to query at
    * runtime whether a specific feature is supported. */
  final val capabilities: Set[Capability] = computeCapabilities
  /** Compute the capabilities. This should be overridden in subclasses as needed. */
  protected def computeCapabilities: Set[Capability] = Set.empty
}

trait RelationalCapabilities extends CommonCapabilities {
  override protected def computeCapabilities: Set[Capability] = super.computeCapabilities ++ RelationalCapabilities.all
}

trait SqlCapabilities extends RelationalCapabilities {
  override protected def computeCapabilities: Set[Capability] = super.computeCapabilities ++ SqlCapabilities.all
}

trait JdbcCapabilities extends SqlCapabilities {
  override protected def computeCapabilities: Set[Capability] = super.computeCapabilities ++ JdbcCapabilities.all
}

trait SQLServerCapabilities extends JdbcCapabilities {
  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - JdbcCapabilities.forceInsert
    - JdbcCapabilities.returnInsertOther
    - JdbcCapabilities.insertOrUpdate
    - SqlCapabilities.sequence
    - JdbcCapabilities.supportsByte
    )
}

trait SQLiteCapabilities extends JdbcCapabilities {
  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
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
    - JdbcCapabilities.forUpdate
    )
}

trait PostgresCapabilities extends JdbcCapabilities {
  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - JdbcCapabilities.insertOrUpdate
    - JdbcCapabilities.nullableNoDefault
    - JdbcCapabilities.supportsByte
    )
}

trait OracleCapabilities extends JdbcCapabilities {
  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - RelationalCapabilities.foreignKeyActions
    - JdbcCapabilities.insertOrUpdate
    - JdbcCapabilities.booleanMetaData
    - JdbcCapabilities.distinguishesIntTypes
    - JdbcCapabilities.supportsByte
    )
}

trait MysqlCapabilities extends JdbcCapabilities {
  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - JdbcCapabilities.returnInsertOther
    - SqlCapabilities.sequenceLimited
    - RelationalCapabilities.joinFull
    - JdbcCapabilities.nullableNoDefault
    - JdbcCapabilities.distinguishesIntTypes //https://github.com/slick/slick/pull/1735
    )
}

trait HsqldbCapabilities extends JdbcCapabilities {
  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - SqlCapabilities.sequenceCurr
    - JdbcCapabilities.insertOrUpdate
    )
}

trait H2Capabilities extends JdbcCapabilities {
  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - SqlCapabilities.sequenceMin
    - SqlCapabilities.sequenceMax
    - SqlCapabilities.sequenceCycle
    - JdbcCapabilities.returnInsertOther
    - RelationalCapabilities.joinFull
    - JdbcCapabilities.insertOrUpdate
    - RelationalCapabilities.reverse
    )
}

trait DerbyCapabilities extends JdbcCapabilities {
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
    - RelationalCapabilities.repeat
    )
}

trait DB2Capabilities extends JdbcCapabilities {
  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - RelationalCapabilities.reverse
    - JdbcCapabilities.insertOrUpdate
    - JdbcCapabilities.supportsByte
    - JdbcCapabilities.booleanMetaData
    )
}