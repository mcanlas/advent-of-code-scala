import sbt.Keys.*
import sbt.*

/**
  * Automatically enriches projects with the following settings (despite the word "override").
  */
object ProjectPlugin extends AutoPlugin {

  /**
    * Defines what members will be imported to the `build.sbt` scope.
    */
  val autoImport = ThingsToAutoImport

  /**
    * Thus plug-in will automatically be enabled; it has no requirements.
    */
  override def trigger: PluginTrigger = AllRequirements

  object ThingsToAutoImport {

    implicit class ProjectOps(p: Project) {
      def withCats: Project =
        p.settings(libraryDependencies += "org.typelevel" %% "cats-core" % "2.12.0")

      def withFileIO: Project =
        p.settings(libraryDependencies += "com.htmlism" %% "rufio-zio" % "72-35c7b930")
    }
  }
}
