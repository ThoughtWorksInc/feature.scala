publishArtifact := false

organization in ThisBuild := "com.thoughtworks.caller"

crossScalaVersions in ThisBuild := Seq("2.10.6", "2.11.11", "2.12.2")

lazy val caller = crossProject

lazy val callerJVM = caller.jvm.addSbtFiles(file("../shared/build.sbt.shared"))

lazy val callerJS = caller.js.addSbtFiles(file("../shared/build.sbt.shared"))
