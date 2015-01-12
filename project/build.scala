import sbt._
import sbt.Keys._
import android.Keys._
import android.Dependencies.LibraryProject
import android.ArbitraryProject

object KeepshareBuild extends Build {

  val kpdGit = uri("https://github.com/pfn/keepassdroid.git#f36d0a3")

  val kpdBase = ArbitraryProject.git(kpdGit)

  lazy val kpdSettings = android.Plugin.androidBuild ++ Seq(
    platformTarget in Android := "android-18",
    libraryProject in Android := true,
    debugIncludesTests in Android := false
  )

  override def buildLoaders = ArbitraryProject.settingsLoader(
    Map(kpdBase -> kpdSettings))

  lazy val kpd = RootProject(kpdBase)

  lazy val root = Project(id="keepshare", base = file(".")) settings(
    android.Plugin.androidBuild :+
      (localProjects in Android  += LibraryProject(kpdBase)):_*) dependsOn(kpd)
}
