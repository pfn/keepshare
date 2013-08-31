package com.hanhuy.android.keepshare

class Application extends android.app.Application {
  override def onCreate() {
    super.onCreate()
    PRNGFixes.apply()
  }
}
