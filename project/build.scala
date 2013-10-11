import sbt._
import android.Keys._
import android.Dependencies.LibraryProject
import android.ArbitraryProject

object KeepshareBuild extends Build {

  //val kpdGit = uri("c:/Users/pfnguyen/src/keepassdroid" +
  val kpdGit = uri("https://github.com/pfn/keepassdroid.git#858f5e1")

  val kpdBase = ArbitraryProject.git(kpdGit)

  lazy val kpdSettings = android.Plugin.androidBuild ++ Seq(
    libraryProject in Android := true
  )

  override def buildLoaders = ArbitraryProject.settingsLoader(
    Map(kpdBase -> kpdSettings))

  lazy val kpd = RootProject(kpdBase)

  lazy val root = Project(id="keepshare", base = file(".")) settings(
    android.Plugin.androidBuild :+
      (localProjects in Android  += LibraryProject(kpdBase)):_*) dependsOn(kpd)
}