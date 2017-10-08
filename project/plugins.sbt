resolvers ++= Seq(
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  "mavenRepoJX" at "http://repo1.maven.org/maven2/"
)

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.8.0")