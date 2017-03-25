libraryDependencies += "org.typelevel" %% "macro-compat" % "1.1.1"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test

crossScalaVersions := Seq("2.10.6", "2.11.8", "2.12.1")
