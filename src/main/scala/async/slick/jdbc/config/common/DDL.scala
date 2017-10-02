package slick.async.jdbc

trait DDL extends SchemaDescriptionDef { self =>
  /** Statements to execute first for create(), e.g. creating tables and indexes. */
  protected def createPhase1: Iterable[String]

  /** Statements to execute after createPhase1, e.g. creating foreign keys. */
  protected def createPhase2: Iterable[String]

  /** All statements to execute for create() */
  def createStatements: Iterator[String] = createPhase1.iterator ++ createPhase2.iterator

  /** Statements to execute first for drop(), e.g. removing connections from other entities. */
  protected def dropPhase1: Iterable[String]

  /** Statements to execute after dropPhase1, e.g. actually dropping a table. */
  protected def dropPhase2: Iterable[String]

  /** All statements to execute for drop() */
  def dropStatements: Iterator[String] = dropPhase1.iterator ++ dropPhase2.iterator

  /** Statements to execute first for truncate() */
  protected def truncatePhase: Iterable[String]
  /** All statements to execute for truncate */
  def truncateStatements: Iterator[String] = truncatePhase.iterator

  /**
   * Create a new DDL object which combines this and the other DDL object.
   *
   * Composition is such that given {{{A.ddl ++ B.ddl}}} the create phases will be
   * run in FIFO order and the drop phases will be run in LIFO order.
   */
  override def ++(other: DDL): DDL = new DDL {
    protected lazy val createPhase1 = self.createPhase1 ++ other.createPhase1
    protected lazy val createPhase2 = self.createPhase2 ++ other.createPhase2
    protected lazy val dropPhase1 = other.dropPhase1 ++ self.dropPhase1
    protected lazy val dropPhase2 = other.dropPhase2 ++ self.dropPhase2
    protected lazy val truncatePhase = other.truncatePhase ++ self.truncatePhase
  }

  override def hashCode() =
    Vector(self.createPhase1, self.createPhase2, self.dropPhase1, self.dropPhase2, self.truncatePhase).hashCode

  override def equals(o: Any) = o match {
    case ddl: DDL =>
      self.createPhase1 == ddl.createPhase1 &&
        self.createPhase2 == ddl.createPhase2 &&
        self.dropPhase1 == ddl.dropPhase1 &&
        self.dropPhase2 == ddl.dropPhase2 &&
        self.truncatePhase == ddl.truncatePhase
    case _ => false
  }
}

object DDL {
  def apply(create1: Iterable[String], create2: Iterable[String], drop1: Iterable[String],
    drop2: Iterable[String], truncate: Iterable[String]): DDL = new DDL {
    protected def createPhase1 = create1
    protected def createPhase2 = create2
    protected def dropPhase1 = drop1
    protected def dropPhase2 = drop2
    protected def truncatePhase = truncate
  }

  def apply(create1: Iterable[String], drop2: Iterable[String]): DDL = apply(create1, Nil, Nil, drop2, Nil)

  def apply(create1: String, drop2: String): DDL = apply(Iterable(create1), Iterable(drop2))
}