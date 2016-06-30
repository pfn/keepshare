package com.hanhuy.android.keepshare

import org.acra.ACRA
import org.acra.annotation.ReportsCrashes

object Application {
  case class LoggedException(msg: String, t: Throwable) extends Exception(msg, t)
  private var _instance: Application = _
  def instance = _instance

  def logException(msg: String, t: Throwable) =
    ACRA.getErrorReporter.handleSilentException(LoggedException(msg, t))
}

@ReportsCrashes(formUri = "http://hanhuy-acra.appspot.com/api/crashreport")
class Application extends android.app.Application {
  override def onCreate() {
    super.onCreate()
    ACRA.init(this)
    Application._instance = this
    PRNGFixes.apply()
  }
}
