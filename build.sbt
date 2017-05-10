crossScalaVersions := Seq("2.11.11", "2.12.2")

publishArtifact := false

organization in ThisBuild := "com.thoughtworks.constructor"

lazy val Constructor = crossProject.crossType(CrossType.Pure).dependsOn(Mixin % Test)

lazy val ConstructorJVM = Constructor.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val ConstructorJS = Constructor.js.addSbtFiles(file("../build.sbt.shared"))

lazy val Mixin = crossProject.crossType(CrossType.Pure)

lazy val MixinJVM = Mixin.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val MixinJS = Mixin.js.addSbtFiles(file("../build.sbt.shared"))

lazy val DelayMacros = crossProject.crossType(CrossType.Pure)

lazy val DelayMacrosJVM = DelayMacros.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val DelayMacrosJS = DelayMacros.js.addSbtFiles(file("../build.sbt.shared"))

lazy val Override = crossProject.crossType(CrossType.Pure).dependsOn(DelayMacros, Mixin % Test)

lazy val OverrideJVM = Override.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val OverrideJS = Override.js.addSbtFiles(file("../build.sbt.shared"))

lazy val unidoc = project
  .enablePlugins(StandaloneUnidoc, TravisUnidocTitle)
  .settings(
    UnidocKeys.unidocProjectFilter in ScalaUnidoc in UnidocKeys.unidoc := {
      inProjects(MixinJVM, ConstructorJVM, OverrideJVM)
    },
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )
