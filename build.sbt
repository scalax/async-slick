scalaVersion := "2.11.8"

scalacOptions ++= Seq("-feature", "-deprecation")

val slickVersion = "3.2.0-RC1"

libraryDependencies += "com.typesafe.slick" %% "slick" % slickVersion

libraryDependencies += "com.typesafe.slick" %% "slick-hikaricp" % slickVersion exclude("com.zaxxer", "HikariCP-java6")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

libraryDependencies += "com.h2database" % "h2" % "1.4.192" % "test"

libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.21" % "test"
