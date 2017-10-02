package slick.async.jdbc

/**
 * A schema description contains the SQL statements for creating and
 * dropping database entities. Schema descriptions can be combined for
 * creating or dropping multiple entities together, even if they have
 * circular dependencies.
 */
trait SchemaDescriptionDef {
  //def ++(other: SchemaDescription): SchemaDescription
  def ++(other: DDL): DDL

}