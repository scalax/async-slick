scalaVersion := "2.12.3"

scalacOptions ++= Seq("-feature", "-deprecation")

val slickVersion = "3.2.1"

libraryDependencies += "com.typesafe.slick" %% "slick" % slickVersion

libraryDependencies += "com.typesafe.slick" %% "slick-hikaricp" % slickVersion exclude("com.zaxxer", "HikariCP-java6")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

libraryDependencies += "com.h2database" % "h2" % "1.4.192" % "test"

libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.21" % "test"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

lazy val fmpp = TaskKey[Seq[File]]("fmpp")
lazy val fmppConfig = config("fmpp").hide
lazy val fmppSettings = inConfig(Compile)(Seq(sourceGenerators += fmpp.taskValue, fmpp := fmppTask.value)) ++ Seq(
  libraryDependencies ++= Seq(
    ("net.sourceforge.fmpp" % "fmpp" % "0.9.15" % fmppConfig.name).intransitive,
    "org.freemarker" % "freemarker" % "2.3.23" % fmppConfig.name,
    "oro" % "oro" % "2.0.8" % fmppConfig.name,
    "org.beanshell" % "bsh" % "2.0b5" % fmppConfig.name,
    "xml-resolver" % "xml-resolver" % "1.2" % fmppConfig.name
  ),
  ivyConfigurations += fmppConfig,
  fullClasspath in fmppConfig := update.map { _ select configurationFilter(fmppConfig.name) map Attributed.blank }.value,
  mappings in (Compile, packageSrc) ++= {
    val fmppSrc = (sourceDirectory in Compile).value / "scala"
    val inFiles = fmppSrc ** "*.fm"
    ((managedSources in Compile).value.pair(Path.relativeTo((sourceManaged in Compile).value) | Path.flat)) ++ // Add generated sources to sources JAR
      (inFiles pair (Path.relativeTo(fmppSrc) | Path.flat)) // Add *.fm files to sources JAR
  }
)
lazy val fmppTask = Def.task {
  val s = streams.value
  val output = sourceManaged.value
  val fmppSrc = sourceDirectory.value / "scala"
  val inFiles = (fmppSrc ** "*.fm").get.toSet
  val cachedFun = FileFunction.cached(s.cacheDirectory / "fmpp", outStyle = FilesInfo.exists) { (in: Set[File]) =>
    IO.delete((output ** "*.scala").get)
    val args = "--expert" :: "-q" :: "-S" :: fmppSrc.getPath :: "-O" :: output.getPath ::
      "--replace-extensions=fm, scala" :: "-M" :: "execute(**/*.fm), ignore(**/*)" :: Nil
    toError((runner in fmpp).value.run("fmpp.tools.CommandLine", (fullClasspath in fmppConfig).value.files, args, s.log))
    (output ** "*.scala").get.toSet
  }
  cachedFun(inFiles).toSeq
}

fmppSettings