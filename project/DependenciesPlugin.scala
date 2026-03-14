import sbt.Keys.*
import sbt.*

object DependenciesPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    implicit class DependencyOps(p: Project) {
      def withCats: Project =
        p
          .settings(libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0")

      def withFileIO: Project =
        p
          .settings(libraryDependencies += "com.htmlism" %% "rufio-zio" % "72-35c7b930")
    }
  }
}
