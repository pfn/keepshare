import android.Keys._

android.Plugin.androidBuild

name := "keepshare"

resolvers += Resolver.sonatypeRepo("snapshots")

javacOptions in Global ++= "-target" :: "1.7" :: "-source" :: "1.7" :: Nil

scalaVersion in Global := "2.11.6"

retrolambdaEnable in Android := false

libraryDependencies ++= Seq(
  "ch.acra" % "acra" % "4.6.1",
  "com.hanhuy" %% "android-common" % "0.4-SNAPSHOT",
  "com.hanhuy.keepassj" % "keepassj" % "2.28.3" exclude("xpp3", "xpp3"),
  "com.google.code.findbugs" % "jsr305" % "2.0.1",
  "com.google.code.gson" % "gson" % "2.2.4",
  "com.android.support" % "support-v4" % "22.0.0",
  "com.google.android.gms" % "play-services-drive" % "7.0.0"
)

proguardOptions in Android ++=
  "-keepclassmembers class scala.runtime.RichInt { ** until(); }" ::
  "-dontwarn javax.naming.**" ::
  "-dontwarn sun.misc.Unsafe" ::
  Nil

ndkBuild in Android := Nil

run <<= run in android.Keys.Android

proguardOptions in Android +=
  "-keep class * extends junit.framework.TestCase { *; }"
