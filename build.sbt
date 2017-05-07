crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.2")

publishArtifact := false

organization in ThisBuild := "com.thoughtworks.constructor"

lazy val Constructor = crossProject.crossType(CrossType.Pure)

lazy val ConstructorJVM = Constructor.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val ConstructorJS = Constructor.js.addSbtFiles(file("../build.sbt.shared"))

lazy val Mixin = crossProject.crossType(CrossType.Pure)

lazy val MixinJVM = Mixin.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val MixinJS = Mixin.js.addSbtFiles(file("../build.sbt.shared"))
