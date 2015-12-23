val keepshare = project.in(file(".")).settings(androidBuild)

name := "keepshare"

versionName := {
  import com.typesafe.sbt.SbtGit.GitKeys.gitReader
  gitReader.value.withGit(_.describedVersion)
}

versionCode := {
  val cmd = "git" :: "rev-list" :: "--count" :: "HEAD" :: Nil
  Option(cmd !!) map (_.trim.toInt)
}

resolvers += "circular reveal" at "https://jitpack.io"

android.dsl.javacFlags(Compile)("-target", "1.7", "-source", "1.7")

scalacOptions in Compile += "-feature"

scalaVersion := "2.11.6"

debugIncludesTests := false

libraryDependencies ++= Seq(
  "ch.acra" % "acra" % "4.6.2",
  "com.melnykov" % "floatingactionbutton" % "1.3.0" exclude("com.android.support", "appcompat-v7"),
  "com.hanhuy.android" %% "iota" % "0.8",
  "com.github.ozodrukh" % "CircularReveal" % "1.0.6",
  "com.hanhuy.android" %% "scala-conversions" % "1.3",
  "com.hanhuy.android" %% "scala-common" % "1.0",
  "com.hanhuy.keepassj" % "keepassj" % "2.30.0" exclude("xpp3", "xpp3"),
  "com.android.support" % "design" % "23.1.1",
  "com.android.support" % "appcompat-v7" % "23.1.1",
  "io.reactivex" %% "rxscala" % "0.24.1",
  "io.reactivex" % "rxandroid" % "0.24.0",
  "com.google.android.gms" % "play-services-drive" % "7.0.0"
)

proguardOptions ++=
  "-keepclassmembers class scala.runtime.RichInt { ** until(); }" ::
  "-dontwarn android.app.Notification" ::
  "-dontwarn iota.**" ::
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

android.dsl.flavor("test")(
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
