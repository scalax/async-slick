package slick.async.jdbc

import java.sql.{ Blob, Clob, Date, PreparedStatement, ResultSet, Time, Timestamp }
import java.util.UUID

import slick.SlickException
import slick.ast._

object JdbcTypeHelper {

  def unapply(t: Type) = Some((JdbcTypeHelper.jdbcTypeFor(t), t.isInstanceOf[OptionType]))

  val columnTypes = new JdbcTypes {}

  def jdbcTypeFor(t: Type): slick.async.jdbc.JdbcType[Any] = ((t.structural match {
    case tmd: slick.async.jdbc.JdbcType[_] => tmd
    case ScalaBaseType.booleanType => columnTypes.booleanJdbcType
    case ScalaBaseType.bigDecimalType => columnTypes.bigDecimalJdbcType
    case ScalaBaseType.byteType => columnTypes.byteJdbcType
    case ScalaBaseType.charType => columnTypes.charJdbcType
    case ScalaBaseType.doubleType => columnTypes.doubleJdbcType
    case ScalaBaseType.floatType => columnTypes.floatJdbcType
    case ScalaBaseType.intType => columnTypes.intJdbcType
    case ScalaBaseType.longType => columnTypes.longJdbcType
    case ScalaBaseType.nullType => columnTypes.nullJdbcType
    case ScalaBaseType.shortType => columnTypes.shortJdbcType
    case ScalaBaseType.stringType => columnTypes.stringJdbcType
    case t: OptionType => jdbcTypeFor(t.elementType)
    case t: ErasedScalaBaseType[_, _] => jdbcTypeFor(t.erasure)
    case t => throw new SlickException("JdbcProfile has no JdbcType for type " + t)
  }): JdbcType[_]).asInstanceOf[JdbcType[Any]]

  //TODO 未做数据库多样性处理
  //TODO 20180121 貌似已无数据库多样性工作，原代码中该方法没有被继承
  def valueToSQLLiteral(v: Any, tpe: Type): String = {
    val JdbcTypeHelper(ti, option) = tpe
    if (option) v.asInstanceOf[Option[Any]].fold("null")(ti.valueToSQLLiteral)
    else ti.valueToSQLLiteral(v)
  }
  //val scalarFrom: Option[String] = None
}