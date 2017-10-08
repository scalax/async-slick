package slick.async.jdbc.config

import slick.ast._
import slick.async.relational.RelationalProfile
import slick.lifted.{ AbstractTable, RefTag, Rep, Tag }

abstract class ProfileTable[T](_tableTag: Tag, _schemaName: Option[String], _tableName: String) extends AbstractTable[T](_tableTag, _schemaName, _tableName) { table =>
  final type TableElementType = T

  def this(_tableTag: Tag, _tableName: String) = this(_tableTag, None, _tableName)

  def tableProvider: RelationalProfile

  def tableIdentitySymbol: TableIdentitySymbol = SimpleTableIdentitySymbol(tableProvider, schemaName.getOrElse("_"), tableName)

  val O: RelationalColumnOptions //: self.columnOptions.type = columnOptions

  /**
   * Note that Slick uses VARCHAR or VARCHAR(254) in DDL for String
   * columns if neither ColumnOption DBType nor Length are given.
   */
  def column[C](n: String, options: ColumnOption[C]*)(implicit tt: TypedType[C]): Rep[C] = {
    if (tt == null) throw new NullPointerException(
      "implicit TypedType[C] for column[C] is null. " +
        "This may be an initialization order problem. " +
        "When using a MappedColumnType, you may want to change it from a val to a lazy val or def.")
    new Rep.TypedRep[C] {
      override def toNode =
        Select((tableTag match {
          case r: RefTag => r.path
          case _ => tableNode
        }), FieldSymbol(n)(options, tt)) :@ tt
      override def toString = (tableTag match {
        case r: RefTag => "(" + _tableName + " " + r.path + ")"
        case _ => _tableName
      }) + "." + n
    }
  }
}