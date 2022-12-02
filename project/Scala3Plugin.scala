import sbt.Keys._
import sbt._

/**
  * Automatically enriches projects with the following settings (despite the word "override").
  */
object Scala3Plugin extends AutoPlugin {

  /**
    * Thus plug-in will automatically be enabled; it has no requirements.
    */
  override def trigger: PluginTrigger = AllRequirements

  override val buildSettings: Seq[Setting[_]] = Seq(
    scalaVersion := "3.2.1"
  )

  override val projectSettings: Seq[Setting[_]] = Seq(
    scalacOptions ++= Seq("-indent", "-rewrite")
  )
}