crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.2")

publishArtifact := false

organization in ThisBuild := "com.thoughtworks.constructor"

lazy val constructor = (crossProject in file(".")).configureAll(_.addSbtFiles(file("../shared/build.sbt")))

lazy val constructorJVM = constructor.jvm

lazy val constructorJS = constructor.js
