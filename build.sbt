publishArtifact := false

organization in ThisBuild := "com.thoughtworks.feature"

lazy val Caller = crossProject.crossType(CrossType.Pure)

lazy val CallerJVM = Caller.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val CallerJS = Caller.js.addSbtFiles(file("../build.sbt.shared"))

crossScalaVersions in ThisBuild := Seq("2.11.11", "2.12.2")

lazy val Constructor = crossProject.crossType(CrossType.Pure).dependsOn(Mixin % Test)

lazy val ConstructorJVM = Constructor.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val ConstructorJS = Constructor.js.addSbtFiles(file("../build.sbt.shared"))

lazy val Mixin = crossProject.crossType(CrossType.Pure)

lazy val MixinJVM = Mixin.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val MixinJS = Mixin.js.addSbtFiles(file("../build.sbt.shared"))

lazy val SyntacticTypeTree = crossProject.crossType(CrossType.Pure)

lazy val Untyper = crossProject.crossType(CrossType.Pure)

lazy val UntyperJVM = Untyper.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val UntyperJS = Untyper.js.addSbtFiles(file("../build.sbt.shared"))

lazy val DelayMacros = crossProject.crossType(CrossType.Pure)

lazy val DelayMacrosJVM = DelayMacros.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val DelayMacrosJS = DelayMacros.js.addSbtFiles(file("../build.sbt.shared"))

lazy val Override =
  crossProject.crossType(CrossType.Pure).dependsOn(Untyper, DelayMacros, Constructor % Test, Mixin % Test)

lazy val OverrideJVM = Override.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val OverrideJS = Override.js.addSbtFiles(file("../build.sbt.shared"))

lazy val unidoc = project
  .enablePlugins(StandaloneUnidoc, TravisUnidocTitle)
  .settings(
    UnidocKeys.unidocProjectFilter in ScalaUnidoc in UnidocKeys.unidoc := {
      inProjects(MixinJVM, ConstructorJVM, OverrideJVM, CallerJVM)
    },
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )
