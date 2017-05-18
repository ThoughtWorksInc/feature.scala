// Workaround for https://github.com/scala-js/scala-js/pull/2954
libraryDependencies := {
  val oldDependencies = libraryDependencies.value

  object Full {
    def unapply(full: CrossVersion.Full): Some[String => String] = {
      Some(full.remapVersion)
    }
  }
  val Full(id) = CrossVersion.full

  for (libraryDependency <- oldDependencies) yield {
    libraryDependency.crossVersion match {
      case Full(remapVersion) if remapVersion.getClass == id.getClass =>
        libraryDependency.copy(crossVersion = CrossVersion.patch)
      case _ =>
        libraryDependency
    }
  }
}
