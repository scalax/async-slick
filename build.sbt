scalaVersion := "2.11.8"

scalacOptions ++= Seq("-feature", "-deprecation")

val slickVersion = "3.2.0-M2"

libraryDependencies += "com.typesafe.slick" %% "slick" % slickVersion

libraryDependencies += "com.typesafe.slick" %% "slick-hikaricp" % slickVersion exclude("com.zaxxer", "HikariCP-java6")