package slick.async.jdbc

import java.sql.{ Blob, Clob, Date, Time, Timestamp, ResultSet, PreparedStatement }
import java.util.UUID

import scala.reflect.ClassTag

import slick.SlickException
import slick.ast._
import slick.async.relational.{ RelationalProfile, RelationalTypesComponent }

trait JdbcTypesComponent extends RelationalTypesComponent { self: JdbcProfile =>

  abstract class MappedJdbcType[T, U](implicit val tmd: JdbcType[U], val classTag: ClassTag[T]) extends JdbcType[T] {
    def map(t: T): U
    def comap(u: U): T

    def newSqlType: Option[Int] = None
    def newSqlTypeName(size: Option[FieldSymbol]): Option[String] = None
    def newValueToSQLLiteral(value: T): Option[String] = None
    def newHasLiteralForm: Option[Boolean] = None

    def sqlType = newSqlType.getOrElse(tmd.sqlType)
    def sqlTypeName(sym: Option[FieldSymbol]) = newSqlTypeName(sym).getOrElse(tmd.sqlTypeName(sym))
    def setValue(v: T, p: PreparedStatement, idx: Int) = tmd.setValue(map(v), p, idx)
    def setNull(p: PreparedStatement, idx: Int): Unit = tmd.setNull(p, idx)
    def getValue(r: ResultSet, idx: Int) = {
      val v = tmd.getValue(r, idx)
      if ((v.asInstanceOf[AnyRef] eq null) || tmd.wasNull(r, idx)) null.asInstanceOf[T]
      else comap(v)
    }
    def wasNull(r: ResultSet, idx: Int) = tmd.wasNull(r, idx)
    def updateValue(v: T, r: ResultSet, idx: Int) = tmd.updateValue(map(v), r, idx)
    def valueToSQLLiteral(value: T) = newValueToSQLLiteral(value).getOrElse(tmd.valueToSQLLiteral(map(value)))
    def hasLiteralForm = newHasLiteralForm.getOrElse(tmd.hasLiteralForm)
    def scalaType = ScalaBaseType[T]
    override def toString = s"MappedJdbcType[${classTag.runtimeClass.getName} -> $tmd]"
    override def hashCode = tmd.hashCode() + classTag.hashCode()
    override def equals(o: Any) = o match {
      case o: MappedJdbcType[_, _] => tmd == o.tmd && classTag == o.classTag
      case _ => false
    }
  }

  object MappedJdbcType extends MappedColumnTypeFactory {
    def base[T: ClassTag, U: BaseColumnType](tmap: T => U, tcomap: U => T): BaseColumnType[T] = {
      assertNonNullType(implicitly[BaseColumnType[U]])
      new MappedJdbcType[T, U] with BaseTypedType[T] {
        def map(t: T) = tmap(t)
        def comap(u: U) = tcomap(u)
      }
    }
  }

  /*object JdbcType {
    def unapply(t: Type) = Some((jdbcTypeFor(t), t.isInstanceOf[OptionType]))
  }

  def jdbcTypeFor(t: Type): JdbcType[Any] = ((t.structural match {
    case tmd: JdbcType[_] => tmd
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
  }): JdbcType[_]).asInstanceOf[JdbcType[Any]]*/

  def defaultSqlTypeName(tmd: JdbcType[_], sym: Option[FieldSymbol]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR =>
      val size = sym.flatMap(_.findColumnOption[RelationalProfile.ColumnOption.Length])
      size.fold("VARCHAR(254)")(l => if (l.varying) s"VARCHAR(${l.length})" else s"CHAR(${l.length})")
    case java.sql.Types.DECIMAL => "DECIMAL(21,2)"
    case t => JdbcTypesComponent.typeNames.getOrElse(
      t,
      throw new SlickException("No SQL type name found in java.sql.Types for code " + t)
    )
  }

  //TODO 已经可以删除
  abstract class DriverJdbcType[@specialized T](implicit val classTag: ClassTag[T]) extends JdbcType[T] {
    def scalaType = ScalaBaseType[T]
    def sqlTypeName(sym: Option[FieldSymbol]): String = self.defaultSqlTypeName(this, sym)
    def valueToSQLLiteral(value: T) =
      if (hasLiteralForm) value.toString
      else throw new SlickException(sqlTypeName(None) + " does not have a literal representation")
    def hasLiteralForm = true
    def wasNull(r: ResultSet, idx: Int) = r.wasNull()
    def setNull(p: PreparedStatement, idx: Int): Unit = p.setNull(idx, sqlType)
  }

  class JdbcTypes extends slick.async.jdbc.JdbcTypes

  trait ImplicitColumnTypes extends super.ImplicitColumnTypes {
    implicit def booleanColumnType = columnTypes.booleanJdbcType
    implicit def blobColumnType = columnTypes.blobJdbcType
    implicit def byteColumnType = columnTypes.byteJdbcType
    implicit def byteArrayColumnType = columnTypes.byteArrayJdbcType
    implicit def charColumnType = columnTypes.charJdbcType
    implicit def clobColumnType = columnTypes.clobJdbcType
    implicit def dateColumnType = columnTypes.dateJdbcType
    implicit def doubleColumnType = columnTypes.doubleJdbcType
    implicit def floatColumnType = columnTypes.floatJdbcType
    implicit def intColumnType = columnTypes.intJdbcType
    implicit def longColumnType = columnTypes.longJdbcType
    implicit def shortColumnType = columnTypes.shortJdbcType
    implicit def stringColumnType = columnTypes.stringJdbcType
    implicit def timeColumnType = columnTypes.timeJdbcType
    implicit def timestampColumnType = columnTypes.timestampJdbcType
    implicit def uuidColumnType = columnTypes.uuidJdbcType
    implicit def bigDecimalColumnType = columnTypes.bigDecimalJdbcType
  }
}

object JdbcTypesComponent {
  private[slick] lazy val typeNames = Map() ++
    (for (f <- classOf[java.sql.Types].getFields)
      yield f.get(null).asInstanceOf[Int] -> f.getName)
}
