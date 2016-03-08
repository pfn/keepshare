lazy val keepshare = project.in(file(".")).settings(androidBuild)

val supportSdkVersion = "23.2.0"
val gmsVersion = "8.3.0"

name := "keepshare"

ndkAbiFilter :=
  "armeabi-v7a" ::
  "x86" ::
  Nil

versionName := {
  import com.typesafe.sbt.SbtGit.GitKeys.gitReader
  gitReader.value.withGit(_.describedVersion)
}

versionCode := {
  val cmd = "git" :: "rev-list" :: "--count" :: "HEAD" :: Nil
  Option(cmd !!) map (_.trim.toInt)
}

android.dsl.javacFlags(Compile)("-target", "1.7", "-source", "1.7")

scalacOptions in Compile += "-feature"

scalaVersion := "2.11.7"

debugIncludesTests := false

libraryDependencies ++= Seq(
  "ch.acra" % "acra" % "4.8.2",
  "io.card" % "android-sdk" % "5.3.0",
  "com.hanhuy.android" %% "iota-pure" % "0.1",
  "com.hanhuy.android" %% "iota" % "1.0.4",
  "com.hanhuy.android" %% "scala-conversions" % supportSdkVersion,
  "com.hanhuy.android" %% "scala-conversions-design" % supportSdkVersion,
  "com.hanhuy.android" %% "scala-common" % "1.3",
  "com.hanhuy.keepassj" % "keepassj" % "2.31.1" exclude("xpp3", "xpp3"),
  "com.android.support" % "recyclerview-v7" % supportSdkVersion,
  "com.android.support" % "design" % supportSdkVersion,
  "com.android.support" % "appcompat-v7" % supportSdkVersion,
  "com.google.android.gms" % "play-services-drive" % gmsVersion,
  "com.google.android.gms" % "play-services-vision" % gmsVersion
)

proguardOptions ++=
  "-keepclassmembers class scala.runtime.RichInt { ** until(); }" ::
  "-dontwarn android.app.Notification" ::
  "-dontwarn iota.Internal210**" ::
  "-dontwarn javax.naming.**" ::
  "-dontwarn com.google.common.**" ::
  "-dontwarn sun.misc.Unsafe" ::
  Nil

proguardCache ++= "com.google.common" :: "org.bouncycastle" :: Nil

shrinkResources := true

proguardOptions += "-keep class scala.runtime.BoxesRunTime { *; }"

android.dsl.flavor("lite")(
  name := "keepshare-lite",
  applicationId := "com.hanhuy.android.keepshare.lite"
)

android.dsl.buildType("test")(
  name := "keepshare-test",
  extraResDirectories += baseDirectory.value / "src" / "androidTest" / "res",
  debugIncludesTests := true,
  applicationId := "com.hanhuy.android.keepshare.test",
  mergeManifests := false,
  proguardOptions ++= "-dontwarn **" ::
    "-keep class android.support.test.** { *; }" ::
    "-keepclasseswithmembers class * { @org.junit.Test <methods>; }" ::
    "-keepclassmembers class scala.reflect.ScalaSignature { java.lang.String bytes(); }" ::
    Nil,

  instrumentTestRunner :=
    "android.support.test.runner.AndroidJUnitRunner",

  proguardCache += "android.support",
  libraryDependencies ++=
    "com.android.support.test" % "runner" % "0.3" ::
      "com.android.support.test.espresso" % "espresso-core" % "2.2" ::
      Nil
)

android.dsl.buildType("protify")(protifySettings:_*)
android.dsl.extendBuildType("protify")(useProguardInDebug := false)

run <<= run in Android

android.dsl.apkExclude("LICENSE.txt")

android.Plugin.withVariant(keepshare, Some("protify"), None)
