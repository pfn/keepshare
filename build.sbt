import android.Keys._

name := "keepshare"

versionName in Android := {
  import com.typesafe.sbt.SbtGit.GitKeys.gitReader
  gitReader.value.withGit(_.describedVersion)
}

versionCode in Android := {
  val cmd = "git" :: "rev-list" :: "--count" :: "HEAD" :: Nil
  Option(cmd !!) map (_.trim.toInt)
}

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
  "com.hanhuy.android" %% "scala-conversions" % "1.3",
  "com.hanhuy.android" %% "scala-common" % "1.0",
  "com.hanhuy.keepassj" % "keepassj" % "2.29.6" exclude("xpp3", "xpp3"),
  "com.android.support" % "design" % "22.2.0",
  "com.android.support" % "appcompat-v7" % "22.2.0",
  "io.reactivex" %% "rxscala" % "0.24.1",
  "io.reactivex" % "rxandroid" % "0.24.0",
  "com.google.android.gms" % "play-services-drive" % "7.0.0"
)

proguardOptions in Android ++=
  "-keepclassmembers class scala.runtime.RichInt { ** until(); }" ::
  "-dontwarn javax.naming.**" ::
  "-dontwarn com.google.common.**" ::
  "-dontwarn sun.misc.Unsafe" ::
  Nil

proguardCache in Android ++= "com.google.common" :: "org.bouncycastle" :: Nil

shrinkResources in Android := true

//ndkBuild in Android := Nil
proguardOptions in Android ++=
  "-keep class * extends junit.framework.TestCase { *; }" ::
  "-keep class scala.runtime.BoxesRunTime { *; }" :: Nil // for debugging only

applicationId in (lite,Android) := "com.hanhuy.android.keepshare.lite"

run <<= run in (pro,Android)

run in lite <<= run in (lite,Android)

extraResDirectories in (lite,Android) += baseDirectory.value / "src" / "lite" / "res"

extraResDirectories in (test1,Android) += baseDirectory.value / "src" / "androidTest" / "res"

debugIncludesTests in (test1,Android) := true

apkbuildExcludes in (test1,Android) += "LICENSE.txt"

applicationId in (test1,Android) := "com.hanhuy.android.keepshare.test"

mergeManifests in (test1, Android) := false

proguardOptions in (test1,Android) ++= "-dontwarn **" ::
  "-keep class android.support.test.** { *; }" ::
  "-keepclasseswithmembers class * { @org.junit.Test <methods>; }" ::
  "-keepclassmembers class scala.reflect.ScalaSignature { java.lang.String bytes(); }" ::
  Nil

instrumentTestRunner in (test1,Android) :=
  "android.support.test.runner.AndroidJUnitRunner"

proguardCache in (test1,Android) += "android.support"

libraryDependencies in test1 ++=
  "com.android.support.test" % "runner" % "0.3" ::
    "com.android.support.test.espresso" % "espresso-core" % "2.2" ::
    Nil

watchSources in test1 <++= Def.task {
  val layout = (projectLayout in Android).value
  (layout.testSources ***) get
}
/*
onLoad in Global := {
  { (state: State) =>
    val nav = new ProjectNavigation(state)
    if ("pro" != Project.extract(state).currentRef.project)
      nav.selectProject(nav.rootRef.build, "pro")
    else state
  } compose (onLoad in Global).value
}
*/
