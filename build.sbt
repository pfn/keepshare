import android.Keys._

name := "keepshare"

resolvers ++= Resolver.sonatypeRepo("snapshots") ::
  ("circular reveal" at "https://jitpack.io") :: Nil

javacOptions in Global ++= "-target" :: "1.7" :: "-source" :: "1.7" :: Nil

scalacOptions in Global += "-feature"

scalaVersion in Global := "2.11.6"

retrolambdaEnable in Android := false

debugIncludesTests in Android := false

libraryDependencies ++= Seq(
  "ch.acra" % "acra" % "4.6.1",
  "com.rengwuxian.materialedittext" % "library" % "2.0.3" exclude("com.android.support", "appcompat-v7"),
  "com.melnykov" % "floatingactionbutton" % "1.3.0" exclude("com.android.support", "appcompat-v7"),
  "com.github.ozodrukh" % "CircularReveal" % "1.0.6",
  "com.hanhuy.android" %% "scala-conversions" % "1.1",
  "com.hanhuy" %% "android-common" % "0.5",
  "com.hanhuy.keepassj" % "keepassj" % "2.29.4" exclude("xpp3", "xpp3"),
  "com.google.code.findbugs" % "jsr305" % "2.0.1",
  "com.google.code.gson" % "gson" % "2.2.4",
  "com.android.support" % "support-v4" % "22.1.0",
  "com.android.support" % "appcompat-v7" % "22.1.0",
  "io.reactivex" %% "rxscala" % "0.24.1",
  "io.reactivex" % "rxandroid" % "0.24.0",
  "com.google.android.gms" % "play-services-drive" % "7.0.0"
)

proguardOptions in Android ++=
  "-keepclassmembers class scala.runtime.RichInt { ** until(); }" ::
  "-dontwarn javax.naming.**" ::
  "-dontwarn sun.misc.Unsafe" ::
  Nil

proguardCache in Android += ProguardCache("com.google.common") % "com.google.guava"

ndkBuild in Android := Nil

proguardOptions in Android ++=
  "-keep class * extends junit.framework.TestCase { *; }" ::
  "-keep class scala.runtime.BoxesRunTime { *; }" :: Nil // for debugging only

packageName in (lite,Android) := "com.hanhuy.android.keepshare.lite"

run <<= run in (pro,Android)

run in lite <<= run in (lite,Android)

// all of this is YUCK! find a better solution for the plugin
projectLayout in (lite,Android) := {
  val wrapped = (projectLayout in (lite, Android)).value
  new android.ProjectLayout.Wrapped(wrapped) {
    override def res = wrapped.bin / "copy-res"
  }
}

collectResources in (lite,Android) <<= ( builder in (lite, Android)
  , debugIncludesTests in (lite, Android)
  , libraryProject in (lite, Android)
  , libraryProjects in (lite, Android)
  , projectLayout in (lite, Android)
  , ilogger in (lite, Android)
  , streams in (lite, Android)
  , baseDirectory in lite
  ) map {
  (bldr, noTestApk, isLib, libs, layout, logger, s, base) =>
    val wrapped = new android.ProjectLayout.Wrapped(layout) {
      override def res = layout.base / "src" / "lite" / "res"
    }
    android.Tasks.doCollectResources(bldr, noTestApk, isLib, libs :+ android.Dependencies.LibraryProject(base), wrapped, logger, s.cacheDirectory, s)
}

collectResources in (lite,Android) <<= collectResources in (lite,Android) dependsOn (copyResources in lite)

copyResources in lite := {
  val cacheFile = streams.value.cacheDirectory / "copy-resources"
  val proLayout = (projectLayout in (pro, Android)).value
  val liteLayout = (projectLayout in (lite, Android)).value
  val resTarget = liteLayout.res
  IO.copyDirectory(proLayout.res, resTarget, false, true)
  val mappings = ((proLayout.res ***).get --- proLayout.res) pair (rebase(proLayout.res, resTarget) | flat(resTarget))
  streams.value.log.debug("Copy resource mappings: " + mappings.mkString("\n\t", "\n\t", ""))
  Sync(cacheFile)(mappings)
  mappings
}
