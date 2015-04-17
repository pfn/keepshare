package com.hanhuy.android.keepshare

import android.app.{Activity, ProgressDialog}
import android.content.Intent
import android.os.Bundle
import android.support.v7.app.ActionBarActivity
import android.view.{MenuItem, Menu}
import android.widget.Toast
import com.hanhuy.android.common.{UiBus, EventBus, ServiceBus}
import com.hanhuy.keepassj.PwDatabase

import scala.concurrent.{Promise, Future}

import Futures._

import scala.util.Try

/**
 * @author pfnguyen
 */
class AuthorizedActivity extends ActionBarActivity with EventBus.RefOwner {
  lazy val settings = Settings(this)
  lazy val km = new KeyManager(this, settings)
  private var serviceExited = false
  private var running = false

  private var dbPromise = Promise[PwDatabase]()
  var database = dbPromise.future
  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    if (settings.get(Settings.FIRST_RUN)) {
      startActivityForResult(SetupActivity.intent, RequestCodes.REQUEST_SETUP)
    } else if (!km.ready) {
      if (settings.get(Settings.NEEDS_PIN) && PINHolderService.instance.isEmpty)
        startActivityForResult(new Intent(this, classOf[PINEntryActivity]),
          RequestCodes.REQUEST_PIN)
      else
        startActivityForResult(SetupActivity.intent, RequestCodes.REQUEST_SETUP)
    } else if (KeyManager.cloudKey.isEmpty) {
      val p = ProgressDialog.show(this, getString(R.string.loading),
        getString(R.string.please_wait), true, false)
      Future {
        km.loadKey()
        km.getConfig.left map { x =>
          startActivityForResult(
            SetupActivity.intent, RequestCodes.REQUEST_SETUP)
        }
      }.onCompleteMain(_=> if (!isFinishing && p.isShowing) p.dismiss())
    } else if (km.ready) {
      openDatabase()
      database.onFailureMain { case _ => finish() }
    }
  }

  override def onActivityResult(requestCode: Int, resultCode: Int, data: Intent) = {
    val success = requestCode match {
      case RequestCodes.REQUEST_SETUP =>
        if (resultCode == Activity.RESULT_OK) {
          true
        } else {
          false
        }
      case RequestCodes.REQUEST_PIN => resultCode == Activity.RESULT_OK
      case RequestCodes.REQUEST_SETUP_PIN => invalidateOptionsMenu()
        true
    }
    if (!success)
      finish()
    else {
      dbPromise = Promise[PwDatabase]()
      database = dbPromise.future
      openDatabase()
      database.onFailureMain { case _ => finish() }
    }
  }
  override def onResume() = {
    super.onResume()
    running = true
    if (serviceExited)
      startActivityForResult(new Intent(this, classOf[PINEntryActivity]),
        RequestCodes.REQUEST_PIN)
    PINHolderService.instance foreach (_.ping())
  }
  override def onPause() = {
    super.onPause()
    running = false
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    super.onCreateOptionsMenu(menu)
    getMenuInflater.inflate(R.menu.authorized, menu)
    if (settings.get(Settings.NEEDS_PIN)) {
      menu.findItem(R.id.menu_setup_pin).setVisible(false)
    }
    true
  }
  override def onOptionsItemSelected(item: MenuItem) = item.getItemId match {
    case R.id.menu_setup =>
      startActivity(new Intent(this, classOf[SetupActivity]))
      true
    case R.id.menu_setup_pin =>
      if (km.ready) {
        startActivityForResult(new Intent(this, classOf[PINSetupActivity]),
          RequestCodes.REQUEST_SETUP_PIN)
      } else {
        Toast.makeText(this,
          R.string.setup_required_for_pin, Toast.LENGTH_SHORT).show()
      }
      true
    case _ => super.onOptionsItemSelected(item)
  }
  ServiceBus += {
    case PINServiceStart => serviceExited = false
    case PINServiceExit  => serviceExited = true
      if (running)
        startActivityForResult(new Intent(this, classOf[PINEntryActivity]),
          RequestCodes.REQUEST_PIN)
  }

  private def openDatabase() = {
    if (Database.isOpen && !dbPromise.isCompleted)
      dbPromise.success(Database.pwdatabase)
    else if (!dbPromise.isCompleted) {

      val p = ProgressDialog.show(this, getString(R.string.loading),
        getString(R.string.please_wait), true, false)
      Future {
        if (!settings.get(Settings.FIRST_RUN)) {
          km.loadKey()
          km.getConfig match {
            case Left(err) => err match {
              case KeyError.NeedPin =>
                startActivityForResult(
                  new Intent(this, classOf[PINEntryActivity]),
                  RequestCodes.REQUEST_PIN)
              case _ =>
                UiBus.post {
                  Toast.makeText(this,
                    R.string.failed_verify, Toast.LENGTH_LONG).show()
                }
                startActivityForResult(SetupActivity.intent,
                  RequestCodes.REQUEST_SETUP)
            }
            case Right((db, pw, keyf)) =>
              val b = new Bundle
              Try(Database.open(db, Option(pw), Option(keyf))) map (
                _ => dbPromise.trySuccess(Database.pwdatabase)) recover {
                case e: Exception =>
                  Toast.makeText(this, getString(R.string.failed_to_open) +
                    e.getMessage,
                    Toast.LENGTH_LONG).show()
                  startActivityForResult(SetupActivity.intent,
                    RequestCodes.REQUEST_SETUP)
              }
          }
        } else {
          startActivityForResult(SetupActivity.intent,
            RequestCodes.REQUEST_SETUP)
        }
        dbPromise.tryFailure(new IllegalAccessException)
        UiBus.post(if (running && p.isShowing) p.dismiss())
      }
    }
  }
}
