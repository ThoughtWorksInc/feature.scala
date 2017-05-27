publishArtifact := false

organization in ThisBuild := "com.thoughtworks.feature"

crossScalaVersions in ThisBuild := Seq("2.11.11-bin-typelevel-4", "2.12.2-bin-typelevel-4")

scalaOrganization in updateSbtClassifiers in ThisBuild := (scalaOrganization in Global).value

scalaOrganization in ThisBuild := "org.typelevel"

lazy val Caller = crossProject.crossType(CrossType.Pure)

lazy val CallerJVM = Caller.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val CallerJS = Caller.js.addSbtFiles(file("../build.sbt.shared"))

lazy val Constructor = crossProject.crossType(CrossType.Pure).dependsOn(Mixin % Test)

lazy val ConstructorJVM = Constructor.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val ConstructorJS = Constructor.js.addSbtFiles(file("../build.sbt.shared"))

lazy val Mixin = crossProject.crossType(CrossType.Pure)

lazy val MixinJVM = Mixin.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val MixinJS = Mixin.js.addSbtFiles(file("../build.sbt.shared"))

lazy val Demixin = crossProject.crossType(CrossType.Pure)

lazy val DemixinJVM = Demixin.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val DemixinJS = Demixin.js.addSbtFiles(file("../build.sbt.shared"))

lazy val The = crossProject.crossType(CrossType.Pure)

lazy val TheJVM = The.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val TheJS = The.js.addSbtFiles(file("../build.sbt.shared"))

lazy val SyntacticTypeTree = crossProject.crossType(CrossType.Pure)

lazy val Untyper = crossProject.crossType(CrossType.Pure)

lazy val UntyperJVM = Untyper.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val UntyperJS = Untyper.js.addSbtFiles(file("../build.sbt.shared"))

lazy val New = crossProject.crossType(CrossType.Pure).dependsOn(Untyper)

lazy val NewJVM = New.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val NewJS = New.js.addSbtFiles(file("../build.sbt.shared"))

lazy val PartialApply = crossProject.crossType(CrossType.Pure)

lazy val PartialApplyJVM = PartialApply.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val PartialApplyJS = PartialApply.js.addSbtFiles(file("../build.sbt.shared"))

lazy val ImplicitApply = crossProject.crossType(CrossType.Pure)

lazy val ImplicitApplyJVM = ImplicitApply.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val ImplicitApplyJS = ImplicitApply.js.addSbtFiles(file("../build.sbt.shared"))

lazy val Override =
  crossProject
    .crossType(CrossType.Pure)
    .dependsOn(Untyper, Constructor % Test, Demixin % Test, Mixin % Test, The % Test, Caller % Test)

lazy val OverrideJVM = Override.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val OverrideJS = Override.js.addSbtFiles(file("../build.sbt.shared"))

lazy val unidoc = project
  .enablePlugins(StandaloneUnidoc, TravisUnidocTitle)
  .settings(
    UnidocKeys.unidocProjectFilter in ScalaUnidoc in UnidocKeys.unidoc := {
      inProjects(UntyperJVM,
                 MixinJVM,
                 DemixinJVM,
                 ConstructorJVM,
                 OverrideJVM,
                 CallerJVM,
                 TheJVM,
                 NewJVM,
                 PartialApplyJVM,
                 ImplicitApplyJVM)
    }
  )
