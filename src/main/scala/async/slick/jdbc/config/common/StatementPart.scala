package slick.async.jdbc

abstract class StatementPart
case object SelectPart extends StatementPart
case object FromPart extends StatementPart
case object WherePart extends StatementPart
case object HavingPart extends StatementPart
case object OtherPart extends StatementPart