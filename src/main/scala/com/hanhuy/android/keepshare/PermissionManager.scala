package com.hanhuy.android.keepshare

import android.content.{ActivityNotFoundException, Intent}
import android.content.pm.PackageManager
import android.net.Uri
import android.support.design.widget.Snackbar
import android.support.v4.app.ActivityCompat
import android.support.v4.content.ContextCompat
import android.support.v7.app.AppCompatActivity
import android.view.View

import scala.concurrent.{Future, Promise}

/**
  * @author pfnguyen
  */
trait PermissionManager { self: AppCompatActivity =>
  private[this] var results = Map.empty[Int,Promise[Unit]]
  private[this] var views = Map.empty[Int,View]
  private[this] var rationales = Map.empty[String,Int]

  final override def onRequestPermissionsResult(requestCode: Int,
                                          permissions: Array[String],
                                          grantResults: Array[Int]): Unit = {
    val success = grantResults forall (_ == PackageManager.PERMISSION_GRANTED)
    if (!success) {
      val rationaleres = rationales(permissions(0))
      val sb = Snackbar.make(views(requestCode), rationaleres, 10000)
      if (ActivityCompat.shouldShowRequestPermissionRationale(self, permissions(0))) {
        sb.setAction(R.string.request_again, new View.OnClickListener() {
          override def onClick(v: View) =
            requestPermission(permissions(0), rationales(permissions(0)), views(requestCode), requestCode)
        })
      } else {
        sb.setAction(R.string.add_permission, new View.OnClickListener() {
          override def onClick(v: View) = {
            try {
              val intent = new Intent(
                android.provider.Settings.ACTION_APPLICATION_DETAILS_SETTINGS)
              intent.setData(Uri.parse("package:" + self.getPackageName))
              self.startActivity(intent)

            } catch {
              case e: ActivityNotFoundException =>
                val intent = new Intent(
                  android.provider.Settings.ACTION_MANAGE_APPLICATIONS_SETTINGS)
                self.startActivity(intent)
            }
          }
        })
      }
      sb.setCallback(new Snackbar.Callback {
        override def onDismissed(snackbar: Snackbar, event: Int): Unit = {
          if (event != Snackbar.Callback.DISMISS_EVENT_ACTION) {
            results(requestCode).failure(new Exception)
            results -= requestCode
            views -= requestCode
          }
        }
      })
      sb.show()
    } else {
      results(requestCode).success(())
      results -= requestCode
      views -= requestCode
    }
  }

  def hasPermission(permission: String) =
    ContextCompat.checkSelfPermission(self, permission) == PackageManager.PERMISSION_GRANTED

  def requestPermission(permission: String,
                        rationale: Int,
                        view: View = self.getWindow.getDecorView,
                        requestCode: Int = (System.currentTimeMillis.toInt / 1000) & 0xff): Future[Unit] = {
    val p = results.getOrElse(requestCode, Promise[Unit]())
    if (hasPermission(permission)) {
      p.success(())
    } else {
      ActivityCompat.requestPermissions(self, Array(permission), requestCode)
      views += ((requestCode,view))
      rationales += ((permission, rationale))
      results += ((requestCode, p))
    }
    p.future
  }
}
