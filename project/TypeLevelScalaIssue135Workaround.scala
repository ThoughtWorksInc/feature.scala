import org.scalajs.sbtplugin.ScalaJSPlugin
import sbt.Keys.libraryDependencies
import sbt.{AutoPlugin, CrossVersion, Def, PluginTrigger, Plugins}

/** Replace `CrossVersion.full` to `CrossVersion.patch` for all `libraryDependencies` */
object TypeLevelScalaIssueWorkaround extends AutoPlugin {

  override def requires: Plugins = ScalaJSPlugin // This plugin should run after ScalaJSPlugin, in order to replace dependencies introduced by ScalaJSPlugin

  override def trigger: PluginTrigger = allRequirements

  private object CrossFull {
    def unapply(full: CrossVersion.Full): Some[String => String] = {
      Some(full.remapVersion)
    }
  }

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    libraryDependencies := {
      val oldDependencies = libraryDependencies.value
      val CrossFull(id) = CrossVersion.full
      for (libraryDependency <- oldDependencies) yield {
        libraryDependency.crossVersion match {
          case CrossFull(remapVersion) if remapVersion.getClass == id.getClass =>
            libraryDependency.copy(crossVersion = CrossVersion.patch)
          case _ =>
            libraryDependency
        }
      }
    }
  )
}
