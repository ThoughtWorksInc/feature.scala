publishArtifact := false

organization in ThisBuild := "com.thoughtworks.feature"

crossScalaVersions in ThisBuild := Seq("2.11.11-bin-typelevel-4", "2.12.4-bin-typelevel-4")

parallelExecution in Global in Test := false

lazy val Caller = crossProject.crossType(CrossType.Pure)

lazy val CallerJVM = Caller.jvm

lazy val CallerJS = Caller.js

lazy val Constructor = crossProject.crossType(CrossType.Pure).dependsOn(Mixin % Test)

lazy val ConstructorJVM = Constructor.jvm

lazy val ConstructorJS = Constructor.js

lazy val Mixin = crossProject.crossType(CrossType.Pure)

lazy val MixinJVM = Mixin.jvm

lazy val MixinJS = Mixin.js

lazy val Demixin = crossProject.crossType(CrossType.Pure)

lazy val DemixinJVM = Demixin.jvm

lazy val DemixinJS = Demixin.js

lazy val The = crossProject.crossType(CrossType.Pure)

lazy val TheJVM = The.jvm

lazy val TheJS = The.js

lazy val SyntacticTypeTree = crossProject.crossType(CrossType.Pure)

lazy val Untyper = crossProject.crossType(CrossType.Pure)

lazy val UntyperJVM = Untyper.jvm

lazy val UntyperJS = Untyper.js

lazy val Factory = crossProject.crossType(CrossType.Pure).dependsOn(Untyper, The, ByName % Test)

lazy val FactoryJVM = Factory.jvm

lazy val FactoryJS = Factory.js

lazy val PartialApply = crossProject.crossType(CrossType.Pure)

lazy val PartialApplyJVM = PartialApply.jvm

lazy val PartialApplyJS = PartialApply.js

lazy val ImplicitApply = crossProject.crossType(CrossType.Pure)

lazy val ImplicitApplyJVM = ImplicitApply.jvm

lazy val ImplicitApplyJS = ImplicitApply.js

lazy val ByName = crossProject.crossType(CrossType.Pure)

lazy val ByNameJVM = ByName.jvm

lazy val ByNameJS = ByName.js

lazy val SelfType = crossProject.crossType(CrossType.Pure)

lazy val SelfTypeJVM = SelfType.jvm

lazy val SelfTypeJS = SelfType.js

lazy val Structural = crossProject.crossType(CrossType.Pure).dependsOn(Untyper)

lazy val StructuralJVM = Structural.jvm

lazy val StructuralJS = Structural.js

lazy val Glb = crossProject.crossType(CrossType.Pure)

lazy val GlbJVM = Glb.jvm

lazy val GlbJS = Glb.js

lazy val `mixins-ImplicitsSingleton` = crossProject.crossType(CrossType.Pure).dependsOn(Factory, ImplicitApply)

lazy val `mixins-ImplicitsSingletonJVM` = `mixins-ImplicitsSingleton`.jvm

lazy val `mixins-ImplicitsSingletonJS` = `mixins-ImplicitsSingleton`.js

lazy val unidoc = project
  .enablePlugins(StandaloneUnidoc, TravisUnidocTitle)
  .settings(
    scalacOptions += "-Yliteral-types",
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8"),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch),
    unidocProjectFilter in ScalaUnidoc in BaseUnidocPlugin.autoImport.unidoc := {
      inProjects(
        UntyperJVM,
        MixinJVM,
        DemixinJVM,
        ConstructorJVM,
        CallerJVM,
        TheJVM,
        FactoryJVM,
        PartialApplyJVM,
        ImplicitApplyJVM,
        ByNameJVM,
        `mixins-ImplicitsSingletonJVM`,
        SelfTypeJVM,
        StructuralJVM,
        GlbJVM
      )
    }
  )
