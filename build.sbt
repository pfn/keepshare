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
  "com.android.support" % "design" % "22.2.0",
  "com.android.support" % "appcompat-v7" % "22.2.0",
  "io.reactivex" %% "rxscala" % "0.24.1",
  "io.reactivex" % "rxandroid" % "0.24.0",
  "com.google.android.gms" % "play-services-drive" % "7.0.0"
)

proguardOptions in Android ++=
  "-keepclassmembers class scala.runtime.RichInt { ** until(); }" ::
  "-dontwarn javax.naming.**" ::
  "-dontwarn sun.misc.Unsafe" ::
  Nil

proguardCache in Android ++= "com.google.common" :: "org.bouncycastle" :: Nil

ndkBuild in Android := Nil

proguardOptions in Android ++=
  "-keep class * extends junit.framework.TestCase { *; }" ::
  "-keep class scala.runtime.BoxesRunTime { *; }" :: Nil // for debugging only

packageName in (lite,Android) := "com.hanhuy.android.keepshare.lite"

run <<= run in (pro,Android)

run in lite <<= run in (lite,Android)

extraResDirectories in (lite,Android) += baseDirectory.value / "src" / "lite" / "res"

onLoad in lite := {
  val prev = (onLoad in lite).value
  val f = (state: State) => {
    val nav = new ProjectNavigation(state)
    nav.selectProject(nav.rootRef.build, "pro")
  }
  f compose prev
}
