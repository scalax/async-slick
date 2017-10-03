package slick.async.jdbc.config

import slick.ast._
import slick.async.jdbc.MySQLProfile.{ RowNum, RowNumGen }
import slick.compiler.{ Phase, QueryCompiler, ResolveZipJoins }
import slick.util.ConstArray
import slick.ast.Util._
import slick.async.jdbc.JdbcModelBuilder
import slick.async.jdbc.meta.{ MColumn, MPrimaryKey, MTable }

import scala.concurrent.ExecutionContext

class MysqlModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext) extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {
  override def createPrimaryKeyBuilder(tableBuilder: TableBuilder, meta: Seq[MPrimaryKey]): PrimaryKeyBuilder = new PrimaryKeyBuilder(tableBuilder, meta) {
    // TODO: this needs a test
    override def name = super.name.filter(_ != "PRIMARY")
  }
  override def createColumnBuilder(tableBuilder: TableBuilder, meta: MColumn): ColumnBuilder = new ColumnBuilder(tableBuilder, meta) {
    override def default = meta.columnDef.map((_, tpe)).collect {
      case (v, "String") => Some(Some(v))
      case ("1" | "b'1'", "Boolean") => Some(Some(true))
      case ("0" | "b'0'", "Boolean") => Some(Some(false))
      case (v, "scala.math.BigDecimal") => Some(Some(scala.math.BigDecimal(v)))
    }.getOrElse {
      val d = super.default
      if (meta.nullable == Some(true) && d == None) {
        Some(None)
      } else d
    }
    override def length: Option[Int] = {
      val l = super.length
      if (tpe == "String" && varying && l == Some(65535)) None
      else l
    }
  }

  //Reference: https://github.com/slick/slick/issues/1419
  override def createTableNamer(meta: MTable): TableNamer = new TableNamer(meta) {
    override def schema = meta.name.catalog
    override def catalog = meta.name.schema
  }

  //https://dev.mysql.com/doc/connector-j/5.1/en/connector-j-reference-type-conversions.html
  import scala.reflect.{ ClassTag, classTag }
  override def jdbcTypeToScala(jdbcType: Int, typeName: String = ""): ClassTag[_] = {
    import java.sql.Types._
    jdbcType match {
      case SMALLINT => classTag[Int]
      case _ => super.jdbcTypeToScala(jdbcType, typeName)
    }
  }
}