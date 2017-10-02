package slick.async.jdbc

import scala.collection.mutable.Builder
import scala.language.{ higherKinds, implicitConversions }
import slick.ast._
import slick.ast.TypeUtil.:@
import slick.async.jdbc.config.BasicCapabilities
import slick.compiler.{ InsertCompiler, Phase, QueryCompiler }
import slick.lifted._
import slick.async.relational.{ RelationalProfile, RelationalTableComponent }
import slick.relational.CompiledMapping
import slick.async.sql.SqlProfile

/** Abstract profile for accessing SQL databases via JDBC. */
trait JdbcProfile extends SqlProfile with JdbcActionComponent
    with JdbcInvokerComponent with JdbcTypesComponent with JdbcModelComponent
    /* internal: */ with JdbcStatementBuilderComponent with JdbcMappingCompilerComponent {

  //@deprecated("Use the Profile object directly instead of calling `.profile` on it", "3.2")
  //override val profile: JdbcProfile = this

  type Backend = JdbcBackend
  val backend: Backend = JdbcBackend
  type ColumnType[T] = JdbcType[T]
  type BaseColumnType[T] = JdbcType[T] with BaseTypedType[T]
  val columnTypes: slick.async.jdbc.JdbcTypes = new JdbcTypes
  lazy val MappedColumnType = MappedJdbcType

  //override protected def computeCapabilities = super.computeCapabilities ++ JdbcCapabilities.all

  override val capabilitiesContent: BasicCapabilities

  //lazy val queryCompiler = compiler.computeQueryCompiler + new JdbcCodeGen(_.buildSelect)
  //lazy val updateCompiler = compiler.computeQueryCompiler + new JdbcCodeGen(_.buildUpdate)
  //lazy val deleteCompiler = compiler.computeQueryCompiler + new JdbcCodeGen(_.buildDelete)
  //lazy val insertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.NonAutoInc), new JdbcInsertCodeGen(crudCompiler.createInsertBuilder))
  //lazy val forceInsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.AllColumns), new JdbcInsertCodeGen(crudCompiler.createInsertBuilder))
  //lazy val upsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.AllColumns), new JdbcInsertCodeGen(crudCompiler.createUpsertBuilder))
  //lazy val checkInsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.PrimaryKeys), new JdbcInsertCodeGen(createCheckInsertBuilder))
  //lazy val updateInsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.AllColumns), new JdbcInsertCodeGen(createUpdateInsertBuilder))
  def compileInsert(tree: Node) = new JdbcCompiledInsert(tree)
  type CompiledInsert = JdbcCompiledInsert

  override final def buildTableSchemaDescription(table: RelationalTableComponent#Table[_]): DDL = createTableDDLBuilder(table).buildDDL
  final def buildSequenceSchemaDescription(seq: Sequence[_]): DDL = createSequenceDDLBuilder(seq).buildDDL

  trait LowPriorityAPI {
    implicit def queryUpdateActionExtensionMethods[U, C[_]](q: Query[_, U, C]): UpdateActionExtensionMethodsImpl[U] =
      createUpdateActionExtensionMethods(crudCompiler.updateCompiler.run(q.toNode).tree, ())
  }

  trait API extends LowPriorityAPI with super.API with ImplicitColumnTypes {
    type SimpleDBIO[+R] = SimpleJdbcAction[R]
    val SimpleDBIO = SimpleJdbcAction

    implicit def queryDeleteActionExtensionMethods[C[_]](q: Query[_ <: RelationalProfile#Table[_], _, C]): DeleteActionExtensionMethods =
      createDeleteActionExtensionMethods(crudCompiler.deleteCompiler.run(q.toNode).tree, ())
    implicit def runnableCompiledDeleteActionExtensionMethods[RU, C[_]](c: RunnableCompiled[_ <: Query[_, _, C], C[RU]]): DeleteActionExtensionMethods =
      createDeleteActionExtensionMethods(c.compiledDelete, c.param)

    implicit def runnableCompiledUpdateActionExtensionMethods[RU, C[_]](c: RunnableCompiled[_ <: Query[_, _, C], C[RU]]): UpdateActionExtensionMethods[RU] =
      createUpdateActionExtensionMethods(c.compiledUpdate, c.param)

    implicit def jdbcActionExtensionMethods[E <: Effect, R, S <: NoStream](a: DBIOAction[R, S, E]): JdbcActionExtensionMethods[E, R, S] =
      new JdbcActionExtensionMethods[E, R, S](a)

    implicit def actionBasedSQLInterpolation(s: StringContext): ActionBasedSQLInterpolation = new ActionBasedSQLInterpolation(s)
  }

  val api: API = new API {}

  def runSynchronousQuery[R](tree: Node, param: Any)(implicit session: Backend#Session): R = tree match {
    case rsm @ ResultSetMapping(_, _, CompiledMapping(_, elemType)) :@ CollectionType(cons, el) =>
      val b = cons.createBuilder(el.classTag).asInstanceOf[Builder[Any, R]]
      createQueryInvoker[Any](rsm, param, null).foreach({ x => b += x }, 0)(session)
      b.result()
    case First(rsm: ResultSetMapping) =>
      createQueryInvoker[R](rsm, param, null).first
  }
}
