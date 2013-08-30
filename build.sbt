import android.Keys._

android.Plugin.androidBuild

name := "keepshare"

libraryDependencies ++= Seq(
  "com.google.apis" % "google-api-services-drive" % "v2-rev96-1.16.0-rc" excludeAll(
    ExclusionRule(organization = "commons-logging"),
    ExclusionRule(organization = "xpp3"),
    ExclusionRule(organization = "commons-codec"),
    ExclusionRule(organization = "org.apache.httpcomponents")),
  "com.google.android.gms" % "play-services" % "3.1.36"
)
