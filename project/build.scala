import sbt._
import sbt.Keys._
import android.Keys._
import android.Plugin.{androidBuild,flavorOf}

object ThisBuild extends Build {
    lazy val pro = project.in(file(".")).settings(androidBuild)

    lazy val lite = flavorOf(pro, "lite")
}
