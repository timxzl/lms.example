name := "lms-example"

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.11.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % "2.11.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-library" % "2.11.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-reflect" % "2.11.2"

libraryDependencies += "EPFL" %% "lms" % "0.3-SNAPSHOT"

libraryDependencies += "EPFL" %% "js-scala" % "0.4-SNAPSHOT"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.5"

scalacOptions += "-Yvirtualize"

scalacOptions += "-deprecation"

scalacOptions += "-feature"
