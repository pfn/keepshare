package com.hanhuy.android.keepshare

object Application {
  private var _instance: Application = _
  def instance = _instance
}
class Application extends android.app.Application {
  override def onCreate() {
    super.onCreate()
    Application._instance = this
    PRNGFixes.apply()
  }
}
