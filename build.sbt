name := "keepshare"

libraryDependencies ++= Seq(
  "com.google.apis" % "google-api-services-drive" % "v2-rev96-1.16.0-rc" intransitive(),
  "com.google.api-client" % "google-api-client-android" % "1.16.0-rc" intransitive(),
  "com.google.api-client" % "google-api-client" % "1.16.0-rc" intransitive(),
  "com.google.http-client" % "google-http-client" % "1.16.0-rc" intransitive(),
  "com.google.http-client" % "google-http-client-gson" % "1.16.0-rc" intransitive(),
  "com.google.http-client" % "google-http-client-android" % "1.16.0-rc" intransitive(),
  "com.google.oauth-client" % "google-oauth-client" % "1.16.0-rc" intransitive(),
  "com.google.code.findbugs" % "jsr305" % "2.0.1",
  "com.google.code.gson" % "gson" % "2.2.4",
  "com.android.support" % "support-v4" % "18.0.0",
  "com.google.android.gms" % "play-services" % "3.1.36"
)

run <<= run in android.Keys.Android
