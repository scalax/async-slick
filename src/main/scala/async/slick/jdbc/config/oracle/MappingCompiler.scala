package slick.async.jdbc.config

import java.sql.ResultSet

import slick.ast._
import slick.async.jdbc._
import slick.relational.ResultConverter

class OracleMappingCompiler extends MappingCompiler {

  /** Create a (possibly specialized) `ResultConverter` for `Option` values of the given `JdbcType`. */
  override def createOptionResultConverter[T](ti: JdbcType[T], idx: Int): ResultConverter[JdbcResultConverterDomain, Option[T]] =
    if (ti.scalaType == ScalaBaseType.stringType)
      (new OptionResultConverter[String](ti.asInstanceOf[JdbcType[String]], idx) {
        override def read(pr: ResultSet) = {
          val v = ti.getValue(pr, idx)
          if ((v eq null) || v.length == 0) None else Some(v)
        }
      }).asInstanceOf[ResultConverter[JdbcResultConverterDomain, Option[T]]]
    else super.createOptionResultConverter(ti, idx)

}