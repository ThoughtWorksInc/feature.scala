import org.scalajs.sbtplugin.ScalaJSPlugin
import sbt.Keys.libraryDependencies
import sbt.{AutoPlugin, CrossVersion, Def, PluginTrigger, Plugins}

/** Replace `CrossVersion.full` to `CrossVersion.patch` for all `libraryDependencies` */
object TypeLevelScalaIssue135Workaround extends AutoPlugin {

  override def requires: Plugins = ScalaJSPlugin // This plugin should run after ScalaJSPlugin, in order to replace dependencies introduced by ScalaJSPlugin

  override def trigger: PluginTrigger = allRequirements

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    libraryDependencies := {
      val oldDependencies = libraryDependencies.value
      for (libraryDependency <- oldDependencies) yield {
        libraryDependency.crossVersion match {
          case cross if cross == CrossVersion.full =>
            libraryDependency.withCrossVersion(CrossVersion.patch)
          case _ =>
            libraryDependency
        }
      }
    }
  )
}
