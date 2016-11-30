package com.hanhuy.android.keepshare

import android.app.{Activity, ProgressDialog}
import android.content.Intent
import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.view.{MenuItem, Menu}
import android.widget.Toast
import com.hanhuy.android.common._
import com.hanhuy.keepassj.PwDatabase

import scala.concurrent.{Promise, Future}

import Futures._

object AuthorizedActivity {
  val EXTRA_SAVED_PIN = "keepshare.extra.PIN"
}
/**
 * @author pfnguyen
 */
class AuthorizedActivity extends AppCompatActivity with EventBus.RefOwner with DialogManager {
  private val log = Logcat("AuthorizedActivity")
  lazy val settings = Settings(this)
  lazy val km = new KeyManager(this, settings)
  private var running = false

  private var dbFuture = Option.empty[Future[PwDatabase]]
  private val readyPromise = Promise[Unit]()

  override def onDestroy() = {
    dismissAllDialogs()
    super.onDestroy()
  }

  def ready = readyPromise.isCompleted
  def database = dbFuture getOrElse openDatabase()

  /**
    * To be overriden by inheritors
    */
  def onAuthenticated(): Unit = {

  }

  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    def onSuccess(): Unit = {
      readyPromise.trySuccess()
      val d = showingDialog(ProgressDialog.show(this, getString(R.string.loading_key),
        getString(R.string.please_wait), true, false))
      km.config flatMap {
        case Left(_) =>
          startActivityForResult(
            SetupActivity.intent, RequestCodes.REQUEST_SETUP)
          Future.failed(KeyError.LoadFailed("need setup"))
        case Right(_) => database
      } onCompleteMain { _ =>
        dismissDialog(d)
        onAuthenticated()
      }
    }
    (for {
      state <- savedInstanceState.? if BuildConfig.DEBUG
      savedkey <- state.getString(AuthorizedActivity.EXTRA_SAVED_PIN).?
    } yield savedkey) match {
      case Some(savedkey) =>
        PINHolderService.startWithKey(savedkey)
        onSuccess()
      case None =>
        if (settings.get(Settings.FIRST_RUN)) {
          startActivityForResult(SetupActivity.intent, RequestCodes.REQUEST_SETUP)
        } else if (!km.ready) {
          if (settings.get(Settings.NEEDS_PIN) && PINHolderService.instance.isEmpty)
            PINEntryActivity.requestPIN(this)
          else
            startActivityForResult(SetupActivity.intent, RequestCodes.REQUEST_SETUP)
        } else {
          onSuccess()
        }
    }
  }


  override def onSaveInstanceState(outState: Bundle) = {
    super.onSaveInstanceState(outState)
    if (BuildConfig.DEBUG) {
      PINHolderService.instance.foreach { service =>
        outState.putString(AuthorizedActivity.EXTRA_SAVED_PIN, KeyManager.hex(service.pinKey))
      }
    }
  }

  override def onActivityResult(requestCode: Int, resultCode: Int, data: Intent) = {
    val success = requestCode match {
      case RequestCodes.REQUEST_SIGN_IN =>
        recreate()
        false
      case RequestCodes.REQUEST_SETUP =>
        if (resultCode == Activity.RESULT_OK) {
          true
        } else {
          false
        }
      case RequestCodes.REQUEST_PIN => resultCode == Activity.RESULT_OK
      case RequestCodes.REQUEST_SETUP_PIN => invalidateOptionsMenu()
        true
      case _ => false
    }
    if (!success)
      finish()
    else {
      readyPromise.trySuccess()
      dbFuture foreach (_.onFailure { case _ => dbFuture = None })
      if (requestCode == RequestCodes.REQUEST_PIN) onAuthenticated()
    }
  }
  override def onResume() = {
    super.onResume()
    running = true
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
    case DatabaseClosed =>
      dbFuture = None
    case PINServiceExit  =>
      dbFuture = None
      if (running)
        Toast.makeText(this, "Your KeepShare session has timed out, exiting",
          Toast.LENGTH_LONG).show()
      finish()
  }

  private def openDatabase(): Future[PwDatabase] = {
    if (!readyPromise.isCompleted) {
      Future.failed(KeyError.NotReady)
    } else if (Database.isOpen)
      Future.successful(Database.pwdatabase)
    else {

      val f = km.config flatMap {
        case Left(err) => err match {
          case KeyError.NeedPin =>
            // ignore this case as it should be handled earlier by readyPromise
            Future.failed(KeyError.NeedPin)
          case _ =>
            UiBus.post {
              Toast.makeText(this,
                R.string.failed_verify, Toast.LENGTH_LONG).show()
            }
            startActivityForResult(SetupActivity.intent,
              RequestCodes.REQUEST_SETUP)
            Future.failed(KeyError.NeedSetup)
        }
        case Right((db, pw, keyf)) =>
          val b = new Bundle
          Database.open(db, Option(pw), Option(keyf)) recoverWith {
            case e =>
              UiBus.post {
                Toast.makeText(this, getString(R.string.failed_to_open) +
                  e.getMessage, Toast.LENGTH_LONG).show()
                log.e(e.getMessage, e)
              }
              startActivityForResult(SetupActivity.intent,
                RequestCodes.REQUEST_SETUP)
              Future.failed(KeyError.NeedSetup)
          }
      }
      if (!f.isCompleted) {
        val d = showingDialog(ProgressDialog.show(this, getString(R.string.loading_database),
          getString(R.string.please_wait), true, false))
        f onCompleteMain {_ =>
          dismissDialog(d)
        }
      }
      dbFuture = Some(f)
      f
    }
  }
}
