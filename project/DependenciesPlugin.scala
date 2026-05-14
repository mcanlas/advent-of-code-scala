import sbt.Keys.*
import sbt.*

object DependenciesPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    implicit class DependencyOps(p: Project) {
      def withCats: Project =
        p
          .settings(libraryDependencies += "org.typelevel" %% "cats-core" % Versions.catsCore)

      def withFileIO: Project =
        p
          .settings(libraryDependencies += "com.htmlism" %% "rufio-zio" % Versions.rufioZio)
    }
  }
}
