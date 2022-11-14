import sbt.Keys._
import sbt._

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

  override val buildSettings: Seq[Setting[String]] = Seq(
    scalaVersion := "2.13.8"
  )

  object ThingsToAutoImport {

    implicit class ProjectOps(p: Project) {
      def withScala3: Project =
        p.settings(scalaVersion := "3.1.0")

      def withCats: Project =
        p.settings(libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0")

      def withEffectMonad: Project =
        p.settings(libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.0")

      def withFileIO: Project =
        p.settings(libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.1")

      def withFileIOScala3: Project =
        p
          .settings(
            libraryDependencies +=
              ("com.github.pathikrit" %% "better-files" % "3.9.1")
                .cross(CrossVersion.for3Use2_13)
          )

      def withTesting: Project =
        p.settings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test")
    }
  }
}
