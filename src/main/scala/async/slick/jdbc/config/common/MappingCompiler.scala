package slick.async.jdbc.config

import slick.ast._
import slick.async.jdbc._
import slick.compiler._
import slick.util.{ ConstArray, SQLBuilder }
import slick.ast.Util.nodeToNodeOps
import slick.async.relational.RelationalProfile
import slick.async.sql.SqlProfile
import slick.lifted.{ AbstractTable, RefTag, Rep, Tag }
import slick.relational.{ ResultConverter, ResultConverterCompiler, TypeMappingResultConverter }

/**
 * A ResultConverterCompiler that builds JDBC-based converters. Instances of
 * this class use mutable state internally. They are meant to be used for a
 * single conversion only and must not be shared or reused.
 */
class MappingCompiler extends ResultConverterCompiler[JdbcResultConverterDomain] {

  /** Create a (possibly specialized) `ResultConverter` for the given `JdbcType`. */
  def createBaseResultConverter[T](ti: JdbcType[T], name: String, idx: Int): ResultConverter[JdbcResultConverterDomain, T] =
    SpecializedJdbcResultConverter.base(ti, name, idx)

  /** Create a (possibly specialized) `ResultConverter` for `Option` values of the given `JdbcType`. */
  def createOptionResultConverter[T](ti: JdbcType[T], idx: Int): ResultConverter[JdbcResultConverterDomain, Option[T]] =
    SpecializedJdbcResultConverter.option(ti, idx)

  def createColumnConverter(n: Node, idx: Int, column: Option[FieldSymbol]): ResultConverter[JdbcResultConverterDomain, _] = {
    val JdbcTypeHelper(ti, option) = n.nodeType.structural
    if (option) createOptionResultConverter(ti, idx)
    else createBaseResultConverter(ti, column.fold("<computed>")(_.name), idx)
  }

  override def createGetOrElseResultConverter[T](rc: ResultConverter[JdbcResultConverterDomain, Option[T]], default: () => T) = rc match {
    case rc: OptionResultConverter[_] => rc.getOrElse(default)
    case _ => super.createGetOrElseResultConverter[T](rc, default)
  }

  override def createIsDefinedResultConverter[T](rc: ResultConverter[JdbcResultConverterDomain, Option[T]]) = rc match {
    case rc: OptionResultConverter[_] => rc.isDefined
    case _ => super.createIsDefinedResultConverter(rc)
  }

  override def createTypeMappingResultConverter(rc: ResultConverter[JdbcResultConverterDomain, Any], mapper: MappedScalaType.Mapper) = {
    val tm = new TypeMappingResultConverter(rc, mapper.toBase, mapper.toMapped)
    mapper.fastPath match {
      case Some(f) => f(tm).asInstanceOf[ResultConverter[JdbcResultConverterDomain, Any]]
      case None => tm
    }
  }
}